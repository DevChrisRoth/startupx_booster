# ===================================================================
# --- Main Modeling Script: Experiment_LogReg_LASSO_SMOTE ---
# ===================================================================

# --- 1. SETUP ---
# Load core libraries
library(tidymodels)
library(dplyr)
library(doParallel)
library(themis) # For step_smote
library(glmnet)

# Load our custom functions for evaluation and plotting
source("./helper_functions.R")

# Load the data (assuming it's created by your EDA script)
source("EDA/eda_students.R")

# --- Remove features not available in a real-world deployment ---
# Keeping age_is_missing as it is a valid behavioral feature
student_data <- student_data %>%
  select(-any_of(c("ctryalp", "Mindset_Asked")))

set.seed(42)

# --- Set the experiment name ---
experiment_name <- "Experiment_LogReg_LASSO_SMOTE"
output_base_dir <- "output"

# Create full path for this experiment's output
experiment_output_dir <- file.path(output_base_dir, experiment_name)

# Ensure the output directory exists
if (!dir.exists(experiment_output_dir)) {
  dir.create(experiment_output_dir, recursive = TRUE)
  cat(paste0("Created output directory: ", experiment_output_dir, "\n"))
} else {
  cat(paste0("Output directory already exists: ", experiment_output_dir, "\n"))
}


# --- 2. DATA SPLITTING & RESAMPLING ---
data_split <- initial_split(student_data, prop = 0.80, strata = FUTSUPNO)
test_data  <- testing(data_split)
train_val_data <- training(data_split)
val_split <- initial_split(train_val_data, prop = 0.80, strata = FUTSUPNO)
train_data <- training(val_split)
validation_data <- testing(val_split)

cat(sprintf("Data Split Summary:\n - Training Set: %d rows\n - Validation Set: %d rows\n - Test Set: %d rows\n",
            nrow(train_data), nrow(validation_data), nrow(test_data)))

cv_folds <- vfold_cv(train_data, v = 5, strata = FUTSUPNO)


# --- 3. FEATURE ENGINEERING RECIPE ---
# Use SMOTE to handle class imbalance by over-sampling
my_recipe <-
  recipe(FUTSUPNO ~ ., data = train_data) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors()) %>%
  themis::step_smote(FUTSUPNO) # Using SMOTE with default over_ratio = 1


# --- 4. MODEL SPECIFICATION & WORKFLOW ---
# Define the LASSO model specification (mixture = 1)
lasso_spec <-
  logistic_reg(penalty = tune(), mixture = 1) %>%
  set_engine("glmnet") %>%
  set_mode("classification")

lasso_workflow <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(lasso_spec)


# --- 5. HYPERPARAMETER TUNING ---
registerDoParallel(cores = detectCores(logical = FALSE))
metric_set_sens_spec <- metric_set(sens, yardstick::spec, roc_auc)

# Create a tuning grid for the penalty parameter.
set.seed(42)
penalty_grid <- grid_regular(penalty(), levels = 20)


# Tune the model
set.seed(42)
lasso_tune_results <- tune_grid(
  lasso_workflow,
  resamples = cv_folds,
  grid = penalty_grid,
  metrics = metric_set_sens_spec,
  control = control_grid(save_pred = TRUE)
)

stopImplicitCluster()
show_best(lasso_tune_results, metric = "roc_auc")


# --- 6. SELECT AND TRAIN ON VALIDATION SET ---
best_params <- select_best(lasso_tune_results, metric = "roc_auc")
final_workflow <- finalize_workflow(lasso_workflow, best_params)
set.seed(42)
validation_fit <- last_fit(final_workflow, val_split)


# --- 7. EVALUATE ON VALIDATION SET & GENERATE REPORT ---
optimal_point_validation <- plot_sensitivity_specificity_tradeoff(
  model_results = validation_fit,
  truth_col = FUTSUPNO,
  prob_col = .pred_Yes,
  output_dir = experiment_output_dir,
  plot_title_suffix = "(Validation Set)"
)

evaluate_and_report_validation(
  validation_fit = validation_fit,
  tune_results = lasso_tune_results,
  best_params = best_params,
  optimal_threshold = optimal_point_validation$.threshold,
  experiment_name = experiment_name,
  output_dir = experiment_output_dir
)