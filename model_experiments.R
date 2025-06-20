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
library(themis) # For SMOTE

# Load our custom functions for evaluation and plotting
source("./helper_functions.R")

# Load the data (assuming it's created by your EDA script)
source("EDA/eda_students.R")
set.seed(42)

experiment_name <- "Experiment_RF_Baseline" # <<< CHANGE THIS FOR EACH NEW EXPERIMENT
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
test_data  <- testing(data_split)      # This is locked away until the very end
train_val_data <- training(data_split)

# Now, split the remaining data into training and validation sets (e.g., 80/20 split of the 80%)
val_split <- initial_split(train_val_data, prop = 0.80, strata = FUTSUPNO)
train_data <- training(val_split)
validation_data <- testing(val_split)

cat(sprintf("Data Split Summary:\n - Training Set: %d rows\n - Validation Set: %d rows\n - Test Set: %d rows\n",
            nrow(train_data), nrow(validation_data), nrow(test_data)))


# Create cross-validation folds from the TRAINING data for tuning
cv_folds <- vfold_cv(train_data, v = 5, strata = FUTSUPNO)


# --- 3. FEATURE ENGINEERING RECIPE ---
# Define the preprocessing steps for this experiment
my_recipe <-
  recipe(FUTSUPNO ~ ., data = train_data) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors()) %>%
  # Example: Tuning the over_ratio for SMOTE
  step_smote(FUTSUPNO, over_ratio = tune(), skip = TRUE)


# --- 4. MODEL SPECIFICATION & WORKFLOW ---
# Define the model for this experiment
rf_spec <-
  rand_forest(
    trees = 1000,
    mtry = tune(),
    min_n = tune()
  ) %>%
  set_engine("ranger", importance = "impurity") %>%
  set_mode("classification")

# Combine the recipe and model into a single workflow object
rf_workflow <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(rf_spec)


# --- 5. HYPERPARAMETER TUNING ---
# Set up parallel processing to speed things up
registerDoParallel(cores = detectCores(logical = FALSE))

# Define the metrics we care about
metric_set_sens_spec <- metric_set(sens, yardstick::spec, roc_auc)

# <<< THIS IS THE FIX >>>
# Manually define a grid that includes ALL tunable parameters from both
# the model spec (mtry, min_n) and the recipe (over_ratio).

# First, get the parameter objects from the workflow
all_params <- extract_parameter_set_dials(rf_workflow)

# Update the range for 'mtry' if needed. The default is 1 to number of predictors.
# Let's set a more reasonable upper bound, for example 50.
all_params <- all_params %>%
  update(mtry = mtry(range = c(5L, 50L)))

# Create a grid with combinations of all three parameters.
# We will test fewer levels for each to keep the total manageable.
set.seed(42)
combined_grid <- grid_regular(
  all_params,
  levels = 4 # This will create 4x4x4 = 64 combinations, adjust as needed for time
)


# Tune the model using the cross-validation folds and our new combined grid
set.seed(42)
rf_tune_results <- tune_grid(
  rf_workflow,
  resamples = cv_folds,
  grid = combined_grid, # <<< Pass the new combined grid here
  metrics = metric_set_sens_spec,
  control = control_grid(save_pred = TRUE)
)

# Stop the parallel cluster
stopImplicitCluster()

# View the best performing hyperparameter sets, ranked by AUC
show_best(rf_tune_results, metric = "roc_auc")


# --- 6. SELECT AND TRAIN ON VALIDATION SET ---
# Select the best hyperparameters based on CROSS-VALIDATION performance
best_params <- select_best(rf_tune_results, metric = "roc_auc")

# Finalize the workflow with these best parameters
final_workflow <- finalize_workflow(rf_workflow, best_params)

# Train the finalized workflow on the FULL training set and evaluate on the VALIDATION set
# This gives us our validation performance before touching the test set
set.seed(42)
validation_fit <- last_fit(final_workflow, val_split)


# --- 7. EVALUATE ON VALIDATION SET & GENERATE REPORT ---
# This is the final step for an individual experiment.
# All decisions about which model is "better" are made from this report.

# Find the optimal threshold using the VALIDATION set predictions
optimal_point_validation <- plot_sensitivity_specificity_tradeoff(
  model_results = validation_fit,
  truth_col = FUTSUPNO,
  prob_col = .pred_Yes,
  output_dir = experiment_output_dir,
  plot_title_suffix = "(Validation Set)" # Add suffix to plot title
)

# Evaluate performance on the VALIDATION set and generate the report
evaluate_and_report_validation(
  validation_fit = validation_fit,
  tune_results = rf_tune_results,
  best_params = best_params,
  optimal_threshold = optimal_point_validation$.threshold,
  experiment_name = experiment_name,
  output_dir = experiment_output_dir
)

# --- END OF EXPERIMENT ---