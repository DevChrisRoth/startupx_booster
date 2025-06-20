# ===================================================================
# --- Main Modeling Script (for Experimentation & Validation) ---
# ===================================================================
#
# PURPOSE:
# To build, tune, and evaluate a model to predict student
# entrepreneurial intention (FUTSUPNO). This script uses a train/validation/test
# split to allow for robust model comparison before a final, unbiased evaluation.

# --- 1. SETUP ---
# Load core libraries
library(tidymodels)
library(dplyr)
library(doParallel)
library(themis) # Make sure themis is loaded for step_smote
library(xgboost)

# Load our custom functions for evaluation and plotting
source("./helper_functions.R")

# Load the data (assuming it's created by your EDA script)
source("EDA/eda_students.R")

# ctryalp: MANY categories = much noise. Most categories have very low feature importance
# Mindset_Asked: Based on survey design - not applicable to DIT Startup Campus
student_data <- student_data %>%
  select(-any_of(c("ctryalp", "Mindset_Asked")))

# --- Set the experiment name to keep results organized ---
experiment_name <- "Experiment_XGBoost_SMOTE"
output_base_dir <- "output" # Base folder for all experiments

# Create full path for this experiment's output
experiment_output_dir <- file.path(output_base_dir, experiment_name)

# Ensure the output directory exists
if (!dir.exists(experiment_output_dir)) {
  dir.create(experiment_output_dir, recursive = TRUE)
  cat(paste0("Created output directory: ", experiment_output_dir, "\n"))
} else {
  cat(paste0("Output directory already exists: ", experiment_output_dir, "\n"))
}


# --- 2. DATA SPLITTING (3-WAY SPLIT) & RESAMPLING ---

# First, split off the final, held-out test set (e.g., 20%)
data_split <- initial_split(student_data, prop = 0.80, strata = FUTSUPNO)
test_data <- testing(data_split) # This is locked away until the very end
train_val_data <- training(data_split)

# Now, split the remaining data into training and validation sets (e.g., 80/20 split of the 80%)
val_split <- initial_split(train_val_data, prop = 0.80, strata = FUTSUPNO)
train_data <- training(val_split)
validation_data <- testing(val_split)

cat(sprintf(
  "Data Split Summary:\n - Training Set: %d rows\n - Validation Set: %d rows\n - Test Set: %d rows\n",
  nrow(train_data), nrow(validation_data), nrow(test_data)
))


# Create cross-validation folds from the TRAINING data for tuning
cv_folds <- vfold_cv(train_data, v = 5, strata = FUTSUPNO)


# --- 3. FEATURE ENGINEERING RECIPE ---
# Add step_smote to handle the class imbalance.
my_recipe <-
  recipe(FUTSUPNO ~ ., data = train_data) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors()) %>%
  themis::step_smote(FUTSUPNO, over_ratio = tune())


# --- 4. MODEL SPECIFICATION & WORKFLOW ---
# Define the XGBoost model specification.
# Remove the scale_pos_weight argument from the engine.
xgb_spec <-
  boost_tree(
    trees = 500,
    tree_depth = tune(),
    learn_rate = tune(),
    min_n = tune()
  ) %>%
  set_engine("xgboost") %>%
  set_mode("classification")

# Combine the recipe and model into a single workflow object
xgb_workflow <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(xgb_spec)


# --- 5. HYPERPARAMETER TUNING ---
# Set up parallel processing to speed things up
registerDoParallel(cores = detectCores(logical = FALSE))

# Define the metrics we care about
metric_set_sens_spec <- metric_set(sens, yardstick::spec, roc_auc)

# Create a tuning grid for the XGBoost parameters plus over_ratio.
set.seed(42)
xgb_grid <- grid_space_filling(
  tree_depth(),
  learn_rate(),
  min_n(),
  over_ratio(),
  size = 20 # Increased size slightly to explore the 4D space
)


# Tune the model using the cross-validation folds and our new grid
set.seed(42)
xgb_tune_results <- tune_grid(
  xgb_workflow,
  resamples = cv_folds,
  grid = xgb_grid,
  metrics = metric_set_sens_spec,
  control = control_grid(save_pred = TRUE)
)

# Stop the parallel cluster
stopImplicitCluster()

# View the best performing hyperparameter sets, ranked by AUC
show_best(xgb_tune_results, metric = "roc_auc")


# --- 6. SELECT AND TRAIN ON VALIDATION SET ---
# Select the best hyperparameters based on CROSS-VALIDATION performance
best_params <- select_best(xgb_tune_results, metric = "roc_auc")

# Finalize the workflow with these best parameters
final_workflow <- finalize_workflow(xgb_workflow, best_params)

# Train the finalized workflow on the FULL training set and evaluate on the VALIDATION set
set.seed(42)
validation_fit <- last_fit(final_workflow, val_split)


# --- 7. EVALUATE ON VALIDATION SET & GENERATE REPORT ---
# Find the optimal threshold using the VALIDATION set predictions
optimal_point_validation <- plot_sensitivity_specificity_tradeoff(
  model_results = validation_fit,
  truth_col = FUTSUPNO,
  prob_col = .pred_Yes,
  output_dir = experiment_output_dir,
  plot_title_suffix = "(Validation Set)"
)

# Evaluate performance on the VALIDATION set and generate the report
evaluate_and_report_validation(
  validation_fit = validation_fit,
  tune_results = xgb_tune_results,
  best_params = best_params,
  optimal_threshold = optimal_point_validation$.threshold,
  experiment_name = experiment_name,
  output_dir = experiment_output_dir
)

# --- END OF EXPERIMENT ---
