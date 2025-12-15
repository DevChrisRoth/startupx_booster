# ==============================================================================
# ANALYSIS: MODELING EXPERIMENTS
# Objective: Compare algorithms (LogReg, RF, XGBoost) to find the best model.
# ==============================================================================

library(tidyverse)
library(tidymodels)
library(themis)       # For SMOTE
library(glmnet)
library(ranger)
library(xgboost)
library(doFuture)     # For Parallel Processing
library(vip)

source("scripts_analysis/00_utils_modeling.R")

# ==============================================================================
# 1. Data Splitting & Resampling
# ==============================================================================
student_data <- readRDS("data/processed/student_model_data.rds")

set.seed(42)


# ==============================================================================
# 2. Data Splitting & Resampling
# ==============================================================================

data_split <- initial_split(student_data, prop = 0.80, strata = FUTSUPNO)
test_data  <- testing(data_split)
train_val_data <- training(data_split)
val_split <- initial_split(train_val_data, prop = 0.80, strata = FUTSUPNO)
train_data <- training(val_split)
validation_data <- testing(val_split)

cat(sprintf("Data Split Summary:\n - Training Set: %d rows\n - Validation Set: %d rows\n - Test Set: %d rows\n",
            nrow(train_data), nrow(validation_data), nrow(test_data)))

cv_folds <- vfold_cv(train_data, v = 5, strata = FUTSUPNO)

# ==============================================================================
# 3. Define Weights (For Class Imbalance)
# ==============================================================================
# Calculate weights using the helper function
weights <- calculate_class_weights(train_val_data, "FUTSUPNO")
xgb_weight_value <- weights$xgb
rf_class_weights <- weights$rf

cat(sprintf("XGBoost Scale Pos Weight: %.2f\n", xgb_weight_value))
cat(sprintf("RF Class Weights: No=%.2f, Yes=%.2f\n", rf_class_weights["No"], rf_class_weights["Yes"]))


# ==============================================================================
# 4. Define Experiment Configurations
# ==============================================================================

experiment_configs <- list(
  
list(
    name = "Experiment_1_LogReg_Baseline",
    model_spec = logistic_reg(penalty = tune(), mixture = 1) %>%
      set_engine("glmnet") %>% set_mode("classification"),
    recipe_func = function(data) {
      recipe(FUTSUPNO ~ ., data = data) %>%
        step_dummy(all_nominal_predictors()) %>%
        step_zv(all_predictors())
    },
    # Define the parameter space for Bayesian search
    param_grid = parameters(penalty())
  ),

  list(
    name = "Experiment_2_LogReg_SMOTE",
    model_spec = logistic_reg(penalty = tune(), mixture = 1) %>%
      set_engine("glmnet") %>% set_mode("classification"),
    recipe_func = function(data) {
      recipe(FUTSUPNO ~ ., data = data) %>%
        step_dummy(all_nominal_predictors()) %>%
        step_zv(all_predictors()) %>%
        # Use a fixed, strong over_ratio to isolate the effect of SMOTE
        step_smote(FUTSUPNO, over_ratio = 0.8)
    },
    param_grid = parameters(penalty())
  ),

  list(
    name = "Experiment_3_LogReg_Splines",
    model_spec = logistic_reg(penalty = tune(), mixture = 1) %>%
      set_engine("glmnet") %>% set_mode("classification"),
    recipe_func = function(data) {
      recipe(FUTSUPNO ~ ., data = data) %>%
        step_dummy(all_nominal_predictors()) %>%
        step_zv(all_predictors()) %>%
        # Add splines for age to allow for non-linear effects
        step_ns(age, deg_free = tune()) %>%
        # Also add SMOTE to make it comparable to the best RF/XGBoost models
        step_smote(FUTSUPNO, over_ratio = 0.8)
    },
    param_grid = parameters(penalty(), deg_free(range = c(2L, 5L)))
  ),

  # ===================================================================
  # --- 2. RANDOM FOREST FAMILY ---
  # ===================================================================

  list(
    name = "Experiment_4_RF_Baseline",
    model_spec = rand_forest(mtry = tune(), min_n = tune()) %>%
      set_engine("ranger", importance = "permutation") %>%
      set_mode("classification"),
    recipe_func = function(data) {
      recipe(FUTSUPNO ~ ., data = data) %>%
        step_dummy(all_nominal_predictors()) %>%
        step_zv(all_predictors())
    },
    param_grid = parameters(mtry(range = c(2, 25)), min_n(range = c(2, 20)))
  ),

  list(
    name = "Experiment_5_RF_SMOTE",
    model_spec = rand_forest(mtry = tune(), min_n = tune()) %>%
      set_engine("ranger", importance = "permutation") %>%
      set_mode("classification"),
    recipe_func = function(data) {
      recipe(FUTSUPNO ~ ., data = data) %>%
        step_dummy(all_nominal_predictors()) %>%
        step_zv(all_predictors()) %>%
        step_smote(FUTSUPNO, over_ratio = 0.8)
    },
    param_grid = parameters(mtry(range = c(2, 25)), min_n(range = c(2, 20)))
  ),

  list(
    name = "Experiment_6_RF_ManualWeights",
    model_spec = rand_forest(mtry = tune(), min_n = tune()) %>%
      set_engine(
        "ranger",
        importance = "permutation",
        class.weights = !!rf_class_weights
      ) %>%
      set_mode("classification"),
    recipe_func = function(data) {
      recipe(FUTSUPNO ~ ., data = data) %>%
        step_dummy(all_nominal_predictors()) %>%
        step_zv(all_predictors())
    },
    param_grid = parameters(mtry(range = c(2, 25)), min_n(range = c(2, 20)))
  ),


  # ===================================================================
  # --- 3. XGBOOST FAMILY ---
  # ===================================================================

  list(
    name = "Experiment_7_XGBoost_Baseline",
    model_spec = boost_tree(
      trees = tune(), min_n = tune(), tree_depth = tune(), learn_rate = tune()
    ) %>%
      set_engine("xgboost") %>% set_mode("classification"),
    recipe_func = function(data) {
      recipe(FUTSUPNO ~ ., data = data) %>%
        step_dummy(all_nominal_predictors()) %>%
        step_zv(all_predictors())
    },
    param_grid = parameters(
      trees(range = c(100, 1000)), min_n(), tree_depth(), learn_rate()
    )
  ),

  list(
    name = "Experiment_8_XGBoost_SMOTE",
    model_spec = boost_tree(
      trees = tune(), min_n = tune(), tree_depth = tune(), learn_rate = tune()
    ) %>%
      set_engine("xgboost") %>% set_mode("classification"),
    recipe_func = function(data) {
      recipe(FUTSUPNO ~ ., data = data) %>%
        step_dummy(all_nominal_predictors()) %>%
        step_zv(all_predictors()) %>%
        step_smote(FUTSUPNO, over_ratio = 0.8)
    },
    param_grid = parameters(
      trees(range = c(100, 1000)), min_n(), tree_depth(), learn_rate()
    )
  ),

  list(
    name = "Experiment_9_XGBoost_ManualWeights",
    model_spec = boost_tree(
      trees = tune(), min_n = tune(), tree_depth = tune(), learn_rate = tune()
    ) %>%
      set_engine("xgboost", scale_pos_weight = !!xgb_weight_value) %>%
      set_mode("classification"),
    recipe_func = function(data) {
      recipe(FUTSUPNO ~ ., data = data) %>%
        step_dummy(all_nominal_predictors()) %>%
        step_zv(all_predictors())
    },
    param_grid = parameters(
      trees(range = c(100, 1000)), min_n(), tree_depth(), learn_rate()
    )
  )
)

# ==============================================================================
# 5. Run Experiments Loop
# ==============================================================================

set.seed(42)
bayes_ctrl <- control_bayes(save_pred = TRUE, verbose = FALSE, seed = 42)
metric_set_sens_spec <- metric_set(sens, spec, roc_auc)

cores_to_use <- parallel::detectCores() - 1
registerDoFuture()
plan(multisession, workers = cores_to_use)

output_base_dir <- "output_experiments"

for (config in experiment_configs) {
  
  experiment_name <- config$name
  cat(paste("\n\n================== RUNNING:", experiment_name, "==================\n"))
  
  experiment_output_dir <- file.path(output_base_dir, experiment_name)
  if (!dir.exists(experiment_output_dir)) {
    dir.create(experiment_output_dir, recursive = TRUE)
  }
  
  current_recipe <- config$recipe_func(train_val_data)
  current_model_spec <- config$model_spec
  
  current_workflow <- workflow() %>%
    add_recipe(current_recipe) %>%
    add_model(current_model_spec)

  set.seed(42)
  tune_results <- tune_bayes(
    current_workflow,
    resamples = cv_folds,
    param_info = config$param_grid, # Verwenden Sie das neue param_grid
    initial = 10,                   # Startet mit 10 zufälligen Kombinationen
    iter = 30,                      # Führt 30 intelligente Iterationen durch
    metrics = metric_set_sens_spec,
    control = bayes_ctrl           # Verwendet die vordefinierte Kontrolle
  )
  
  best_params <- select_best(tune_results, metric = "roc_auc")
  final_workflow <- finalize_workflow(current_workflow, best_params)
  
  set.seed(42)
  validation_fit <- last_fit(final_workflow, initial_split(train_val_data, prop = 0.8, strata = FUTSUPNO))
  
  optimal_point <- plot_sensitivity_specificity_tradeoff(
    model_results = validation_fit,
    truth_col = FUTSUPNO,
    prob_col = .pred_Yes,
    output_dir = experiment_output_dir,
    plot_title_suffix = paste0("(", experiment_name, " - Validation)")
  )
  
  evaluate_and_report_validation(
    validation_fit = validation_fit,
    tune_results = tune_results,
    best_params = best_params,
    optimal_threshold = optimal_point$.threshold,
    experiment_name = experiment_name,
    output_dir = experiment_output_dir
  )
}

plan(sequential) # Resets to non-parallel processing
cat("\n\n--- ALL EXPERIMENTS COMPLETE ---\n")