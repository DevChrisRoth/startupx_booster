library(fastshap)
library(tidyverse)
library(tidymodels)

# --- 1. OFFLINE: Background Data Preparation ---
# Run this ONCE when saving your model artifacts.
# It creates the reference dataset that SHAP needs to compare against.
prepare_shap_background <- function(final_workflow, training_data, n_samples = 100, seed = 42) {
  
  # Extract the recipe
  recipe_obj <- extract_recipe(final_workflow)
  
  # Preprocess the training data to get it into the format the model engine expects
  # (converts factors to dummies, handles levels, etc.)
  set.seed(seed)
  background_processed <- bake(recipe_obj, new_data = training_data) %>%
    select(-any_of("FUTSUPNO")) %>% # Remove target
    sample_n(min(nrow(.), n_samples)) # Downsample for speed
  
  return(background_processed)
}


# --- 2. ONLINE: Fast SHAP Calculation ---
# Run this inside your GUI when a new tuple arrives.
# It takes the PRE-PROCESSED background data.
compute_single_shap <- function(final_workflow, background_processed, new_observation_raw) {
  
  # Extract necessary components
  model_engine <- extract_fit_engine(final_workflow)
  recipe_obj <- extract_recipe(final_workflow)
  
  # 1. Process ONLY the single new observation
  # We use the existing recipe to ensure it matches the training format exactly
  obs_processed <- bake(recipe_obj, new_data = new_observation_raw) %>%
    select(-any_of("FUTSUPNO"))
  
  # 2. Define the prediction wrapper for Ranger
  # The wrapper must accept the ENGINE and PROCESSED data
  pred_wrapper <- function(object, newdata) {
    # 'object' is the raw ranger model
    # 'newdata' is the dataframe of predictors (dummies, numerics)
    predict(object, data = newdata)$predictions[, "Yes"]
  }
  
  # 3. Compute SHAP
  # Since X is already processed, we don't need to do it here.
  shap_values <- fastshap::explain(
    model_engine,
    X = background_processed,    # Use the cached background
    newdata = obs_processed,     # The new user input
    pred_wrapper = pred_wrapper,
    nsim = 50,                   # Monte Carlo simulations
    adjust = TRUE
  )
  
  # 4. Formatting for visualization
  # We map the SHAP values back to the feature names
  df_shap <- data.frame(
    feature = names(shap_values),
    shap_value = as.numeric(shap_values[1, ]),
    # We grab the baked values for display, or you could pass raw values if preferred
    actual_value = t(obs_processed)[,1] 
  )
  
  return(df_shap)
}

# --- 3. VISUALIZATION (Same as before) ---
plot_shap_contribution <- function(shap_df, top_n = 6) {
  # ... (Use the plotting code from the previous response) ...
  # (Included below for completeness if you need it again)
    plot_data <- shap_df %>%
    mutate(abs_val = abs(shap_value)) %>%
    arrange(desc(abs_val)) %>%
    head(top_n) %>%
    mutate(
      direction = ifelse(shap_value > 0, "Positive (+)", "Negative (-)"),
      feature = gsub("yy", "", feature),
      label = paste0(feature, "\n(", round(as.numeric(actual_value), 2), ")") 
      # Note: 'actual_value' from 'bake' might be dummy encoded (0/1). 
      # If you want human-readable labels (e.g., "Agree"), pass 'new_observation_raw' 
      # to this function alongside 'shap_df' and look up the values there.
    )

  ggplot(plot_data, aes(x = reorder(label, shap_value), y = shap_value, fill = direction)) +
    geom_col(width = 0.7, color = "black", alpha = 0.8) +
    coord_flip() +
    scale_fill_manual(values = c("Positive (+)" = "#76D714", "Negative (-)" = "#EE3B3B")) +
    facet_wrap(~direction, scales = "free_y", ncol = 1) +
    theme_minimal() +
    labs(
      title = "Prediction Explanation",
      y = "Contribution (SHAP Value)",
      x = NULL
    ) +
    theme(legend.position = "none")
}