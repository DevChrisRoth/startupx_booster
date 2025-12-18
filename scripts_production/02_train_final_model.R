# ==============================================================================
# PRODUCTION: TRAIN FINAL MODEL
# Objective: Train the best model (defined in config) on the full training set,
#            evaluate on the held-out test set, and save for the GUI.
# ==============================================================================

library(tidyverse)
library(tidymodels)
library(ranger)
library(vip)
library(doFuture)

# Load helper functions for plotting and reporting
source("scripts_analysis/00_utils_modeling.R")
# Load the central model architecture definitions
source("config/model_configs.R")

# --- CONFIGURATION ---
# Define the winning model. To switch production models, change this one line.
WINNING_MODEL_NAME <- "Experiment_6_RF_ManualWeights"

# Define Output Directory
output_dir <- "output_production"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# ==============================================================================
# 1. Load & Prepare Data
# ==============================================================================
cat("--- 1. Loading Data ---\n")
student_data <- readRDS("data/processed/student_model_data.rds")

# Remove ctryalp (High cardinality/noise)
student_data <- student_data %>% select(-any_of(c("ctryalp")))

set.seed(42)
# Split: 80% Training (used for tuning & training), 20% Test (Held-out for final report)
data_split <- initial_split(student_data, prop = 0.80, strata = FUTSUPNO)
train_data <- training(data_split)
test_data  <- testing(data_split)

# Create a validation split within the training data for Threshold calculation
set.seed(42)
val_split <- initial_split(train_data, prop = 0.80, strata = FUTSUPNO)

# Create CV folds for Hyperparameter Tuning
set.seed(42)
cv_folds <- vfold_cv(train_data, v = 5, strata = FUTSUPNO)

cat(sprintf("Data Split: Training (%d), Test (%d)\n", nrow(train_data), nrow(test_data)))


# ==============================================================================
# 2. Define Model & Weights from Config
# ==============================================================================
cat(paste("--- 2. Configuring Model:", WINNING_MODEL_NAME, "---\n"))

# Calculate Class Weights (Logic from Utils)
weights <- calculate_class_weights(train_data, "FUTSUPNO")
cat(sprintf("Class Weights Applied: No=%.2f, Yes=%.2f\n", weights$rf["No"], weights$rf["Yes"]))

# Get all model configurations
all_configs <- get_model_configs()

# Get the specific configuration for our winning model
best_config <- all_configs[[WINNING_MODEL_NAME]]
if (is.null(best_config)) {
  stop(paste("Winning model name '", WINNING_MODEL_NAME, "' not found in config file."))
}

# Dynamically build the model spec and recipe from the config
# Note: We need to fix the `trees` parameter for production
final_model_spec <- best_config$model_spec_func(weights) %>%
  # Override the tune() for trees with a fixed production value
  update(trees = 1000) 
  
final_recipe <- best_config$recipe_func(train_data)

# Bundle into Workflow
rf_workflow <- workflow() %>%
  add_recipe(final_recipe) %>%
  add_model(final_model_spec)


# ==============================================================================
# 3. Hyperparameter Tuning
# ==============================================================================
cat("--- 3. Tuning Hyperparameters ---\n")

# Setup Parallel Processing
cores_to_use <- parallel::detectCores() - 1
registerDoFuture()
plan(multisession, workers = cores_to_use)

# Run Bayesian Optimization
set.seed(42)
tune_results <- tune_bayes(
  rf_workflow,
  resamples = cv_folds,
  param_info = best_config$param_grid, # Use param_grid from config
  initial = 10,
  iter = 20,
  metrics = metric_set(roc_auc), # Optimize for AUC
  control = control_bayes(save_pred = TRUE, verbose = TRUE)
)

# Select Best Parameters
best_params <- select_best(tune_results, metric = "roc_auc")
cat("\nBest Hyperparameters found:\n")
print(best_params)

# Finalize Workflow
final_workflow <- finalize_workflow(rf_workflow, best_params)

# Stop Parallel Processing
plan(sequential)


# ==============================================================================
# 4. Determine Optimal Threshold
# ==============================================================================
cat("--- 4. Determining Optimal Decision Threshold ---\n")

# We fit on the Train portion of val_split and evaluate on the Validation portion
set.seed(42)
validation_run <- last_fit(final_workflow, val_split)

# Use helper to find crossover point
optimal_point <- plot_sensitivity_specificity_tradeoff(
  model_results = validation_run,
  truth_col = FUTSUPNO,
  prob_col = .pred_Yes,
  output_dir = output_dir,
  plot_title_suffix = "(Production Threshold Determination)"
)

final_threshold <- optimal_point$.threshold
cat(sprintf("Optimal Threshold Selected: %.4f\n", final_threshold))


# ==============================================================================
# 5. Final Fit & Test Set Evaluation
# ==============================================================================
cat("--- 5. Final Training & Evaluation ---\n")

# Fit on the ENTIRE Training Data
final_fit <- fit(final_workflow, data = train_data)

# Predict on the Held-Out Test Set
test_predictions <- predict(final_fit, new_data = test_data, type = "prob") %>%
  bind_cols(test_data %>% select(FUTSUPNO))

# Calculate Metrics
predictions_reclassified <- test_predictions %>%
  mutate(.pred_class_optimal = factor(ifelse(.pred_Yes >= final_threshold, "Yes", "No"), 
                                      levels = c("No", "Yes")))

conf_matrix <- conf_mat(predictions_reclassified, truth = FUTSUPNO, estimate = .pred_class_optimal)
roc_auc_val <- roc_auc(test_predictions, truth = FUTSUPNO, .pred_Yes, event_level = "second")$.estimate

# Print to Console
print(conf_matrix)
cat(sprintf("\nFinal Test AUC: %.4f\n", roc_auc_val))

# Save ROC Plot
roc_data <- test_predictions %>% roc_curve(FUTSUPNO, .pred_Yes, event_level = "second")
roc_plot <- ggplot(roc_data, aes(x = 1 - specificity, y = sensitivity)) +
  geom_abline(linetype = "dashed", color = "grey70") +
  geom_line(color = "#00F2FF", linewidth = 1.2) +
  labs(title = "Final Test Set ROC", subtitle = paste("AUC =", round(roc_auc_val, 3))) +
  plot_theme # Using the consistent theme from utils

ggsave(file.path(output_dir, "final_test_roc.png"), roc_plot)


# ==============================================================================
# 6. Save Artifacts for GUI (Model & SHAP)
# ==============================================================================
cat("--- 6. Saving Model Artifacts ---\n")

# Save the actual model object
saveRDS(final_fit, file.path(output_dir, "final_model.rds"))

# Save the threshold
saveRDS(final_threshold, file.path(output_dir, "final_threshold.rds"))

# Save the test data
saveRDS(test_data, file.path(output_dir, "test_data_subset.rds"))

cat("\nSUCCESS: Final model and threshold saved to", output_dir, "\n")


source("scripts_production/00_utils_shap.R") # Load the utility

cat("Generating SHAP background data (Offline Optimization)...\n")
shap_background <- prepare_shap_background(
  final_workflow = final_fit,
  training_data = train_data, # Use training data for the baseline
  n_samples = 100 # 100-200 is a good balance for speed/accuracy
)

saveRDS(shap_background, file.path(output_dir, "shap_background.rds"))

cat("\nSUCCESS: Final model, threshold, and SHAP background saved to", output_dir, "\n")