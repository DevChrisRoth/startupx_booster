library(fastshap)
library(tidyverse)
library(tidymodels)
library(ranger)

# --- Dictionary: Technical Name -> User Friendly Name ---
variable_labels <- c(
  # Demographics
  "ctryalp"   = "Country",
  "gender"    = "Gender",
  "age"       = "Age",
  "hhsize"    = "Household Size",
  "GEMOCCU"   = "Job Status",
  "GEMHHINC"  = "Income Level",
  "GEMEDUC"   = "Education",
  "cphhinc"   = "Covid Income Impact",
  
  # Entrepreneurial Perceptions
  "KNOWENyy"  = "Knows Founder",
  "OPPORTyy"  = "Sees Opportunities",
  "SUSKILyy"  = "Startup Skills",
  "FRFAILyy"  = "Fear of Failure",
  "EASYSTyy"  = "Ease of Starting",
  "CREATIVyy" = "Seen as Innovative",
  "VISIONyy"  = "Career Plan",
  
  # Semantic Fixes (Negative/Inverse Questions)
  "OPPISMyy"  = "Misses Opportunities", # Corrected semantic meaning
  "PROACTyy"  = "Hesitant to Act" ,      # Corrected semantic meaning
  "WBINC"     = "Country"
)

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

# --- Helper: Aggregate One-Hot Encoded SHAP values ---
aggregate_shap_features <- function(shap_df) {
  
  # 1. Define your Original Variable names manually
  # This ensures we capture all parts of the variable
  original_vars <- c(
    "GEMHHINC", "GEMEDUC", "WBINC", "gender", "GEMOCCU", 
    "cphhinc", "KNOWENyy", "OPPORTyy", "SUSKILyy", "FRFAILyy", "EASYSTyy",
    "OPPISMyy", "PROACTyy", "CREATIVyy", "VISIONyy", "FUTSUPNO",
    "age", "hhsize", "Mindset_Asked" 
  )
  
  col_names <- colnames(shap_df)
  aggregated_shap <- list()
  
  for (var in original_vars) {
    # Regex to find all OHE columns belonging to this variable
    # Matches: "Variable" AND ("Variable_..." OR "Variable....")
    pattern <- paste0("^", var, "([._].*|$)")
    
    matching_cols <- grep(pattern, col_names, value = TRUE)
    
    if (length(matching_cols) > 0) {
      if (length(matching_cols) == 1) {
        # Numeric/Binary variable (not OHE) -> Keep as is
        aggregated_shap[[var]] <- shap_df[[matching_cols]]
      } else {
        # OHE variable -> Sum the columns
        aggregated_shap[[var]] <- rowSums(shap_df[, matching_cols, drop = FALSE])
      }
    }
  }
  return(as.data.frame(aggregated_shap))
}


## --- 2. ONLINE: Fast SHAP Calculation ---
compute_single_shap <- function(final_workflow, background_processed, new_observation_raw) {
  
  # Extract components
  model_engine <- extract_fit_engine(final_workflow)
  recipe_obj <- extract_recipe(final_workflow)
  
  # 1. Process ONLY the single new observation
  obs_processed <- bake(recipe_obj, new_data = new_observation_raw) %>%
    select(-any_of("FUTSUPNO"))
  
  # Ensure standard data.frames for fastshap
  obs_processed_df <- as.data.frame(obs_processed)
  background_processed_df <- as.data.frame(background_processed)
  
  # 2. Define prediction wrapper
  pred_wrapper <- function(object, newdata) {
    predict(object, data = newdata)$predictions[, "Yes"]
  }
  
  # 3. Compute SHAP (Returns OHE columns)
  shap_values_raw <- fastshap::explain(
    model_engine,
    X = background_processed_df,
    newdata = obs_processed_df,
    pred_wrapper = pred_wrapper,
    nsim = 50,
    adjust = TRUE
  )
  
  # 4. AGGREGATE SHAP VALUES (The Fix)
  # Sum OHE columns back to original variables
  shap_values_agg <- aggregate_shap_features(as.data.frame(shap_values_raw))
  
  # 5. Format for visualization
  # We map the Aggregated Feature Name back to the RAW Value (User Input)
  df_shap <- data.frame(
    feature = colnames(shap_values_agg),
    shap_value = as.numeric(shap_values_agg[1, ]),
    stringsAsFactors = FALSE
  )
  
  # Look up the actual value from the RAW input (Text/Factor), not the processed input (0/1)
  df_shap$actual_value <- sapply(df_shap$feature, function(f) {
    val <- new_observation_raw[[f]]
    if(is.null(val) || length(val) == 0) return("NA")
    return(as.character(val))
  })
  
  return(df_shap)
}

plot_shap_contribution <- function(shap_df, top_n = 5) {
  
  # 1. Common Data Processing
  processed_df <- shap_df %>%
    
    # We remove 'Mindset_Asked' because it's a structural flag, not user behavior
    filter(feature != "Mindset_Asked") %>% 
    
    mutate(
      # --- RENAME LOGIC START ---
      # Check if the feature exists in our dictionary; if yes, use new name, else keep original
      clean_feature = ifelse(feature %in% names(variable_labels), 
                             variable_labels[feature], 
                             feature),
      # --- RENAME LOGIC END ---
      
      # Create label: "Name \n (Value)"
      label = paste0(clean_feature, "\n(", actual_value, ")"), 
      
      # Absolute value for sorting
      abs_val = abs(shap_value)
    )
  
  # Shared Theme
  common_theme <- theme_minimal() +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.y = element_text(size = 11, face = "bold", color = "#404040"),
      plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
      axis.title.x = element_text(size = 10, color = "grey30")
    )
  
  # 2. Positive Plot
  pos_data <- processed_df %>%
    filter(shap_value > 0) %>%
    arrange(desc(abs_val)) %>%
    head(top_n)
  
  p_pos <- ggplot(pos_data, aes(x = reorder(label, abs_val), y = abs_val)) +
    geom_col(width = 0.6, fill = "#76D714", color = "black", alpha = 0.9) +
    coord_flip() +
    labs(title = "Positive (+)", x = NULL, y = "Contribution") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1))) + 
    common_theme
  
  # 3. Negative Plot
  neg_data <- processed_df %>%
    filter(shap_value < 0) %>%
    arrange(desc(abs_val)) %>% 
    head(top_n)
  
  p_neg <- ggplot(neg_data, aes(x = reorder(label, abs_val), y = abs_val)) +
    geom_col(width = 0.6, fill = "#EE3B3B", color = "black", alpha = 0.9) +
    coord_flip() +
    labs(title = "Negative (-)", x = NULL, y = "Contribution") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
    common_theme
  
  return(list(positive = p_pos, negative = p_neg))
}