# ==============================================================================
# PRODUCTION PIPELINE: DATA CLEANING & PREPARATION
# Purpose: Raw Data -> Cleaned, Imputed .rds files
# Output: 
#   1. data/intermediate/global_cleaned_data.rds (For EDA)
#   2. data/processed/student_model_data.rds (For Modeling/GUI)
# ==============================================================================

source("scripts_production/00_utils_preprocessing.R")
source("scripts_production/00_utils_relabeling.R")

library(missForest)
library(doParallel)


# 0. Load Data using the shared function
gem_data <- load_data()
model_data <- scope_data(gem_data)

# 1. FEATURE ENGINEERING: MISSINGNESS FLAGS
# We must capture the NA patterns while they are still raw NAs.

# A. Handle Mindset (Split Sample)
mindset_cols <- get_mindset_columns()
model_data <- model_data %>%
  mutate(
    Mindset_Asked = as.factor(ifelse(
      rowSums(is.na(select(., all_of(mindset_cols)))) == length(mindset_cols),
      "Not_Asked", "Asked"
    ))
  )

# B. Handle Age
model_data <- model_data %>%
  mutate(age_is_missing = as.factor(ifelse(is.na(age), "Yes", "No")))

# 2. RELABELING
# Now we convert codes to Factors.
# Note: We do NOT fill the Mindset NAs yet, we just ensure the columns are factors.
model_data <- apply_gem_relabeling(model_data)

# 3. APPLY MISSING VALUE STRATEGY
# A. Drop Attitudes (Too much missing data)
model_data <- model_data %>% select(-any_of(get_attitude_columns()))
model_data <- model_data %>% select(-any_of(get_high_missingness_columns()))

# B. Fill Mindset NAs based on the Flag created in Step 1
model_data <- model_data %>%
  mutate(
    across(all_of(mindset_cols), ~ factor(case_when(
      Mindset_Asked == "Not_Asked" ~ "Not_Applicable",
      Mindset_Asked == "Asked" & is.na(.) ~ "Refused_Answer",
      TRUE ~ as.character(.)
    ), 
    # Explicitly restore the levels so they match the other data
    levels = c("Strongly Disagree", "Somewhat Disagree", "Neither", 
               "Somewhat Agree", "Strongly Agree", "Refused_Answer", "Not_Applicable"),
    ordered = TRUE)) # Keep them ordered!
  )

# --- Stage 1.12: Advanced Imputation with MissForest ---
# --- Rationale for Advanced Imputation ---
#
# 1. Why not simple imputation (median/mode)?
# Our initial analysis showed that even columns with <5% missingness still
# had thousands of missing values. Imputing these with a single value
# (like the median or mode) would artificially reduce the variance of the
# data and ignore the rich relationships between variables. For example,
# an imputed 'age' should likely be different for a student vs. a retiree.
# A multivariate approach is needed for more realistic imputations.
#
# 2. Why MissForest over MICE?
# Both MICE and MissForest are excellent multivariate imputation methods.
# However, MICE is parametric; it typically uses regression models (linear,
# logistic) to predict missing values. This assumes that the relationships
# between variables are well-described by these simple models.
# Our data consists of complex socio-demographic and perceptual variables
# where relationships are likely non-linear and interactive.
# MissForest is non-parametric. It uses a Random Forest, which excels at
# automatically capturing these complex interactions without making any
# assumptions about the underlying data distribution. For this reason, it is
# often more accurate for datasets like ours.
#
# 3. Why Single Imputation (MissForest) and not Multiple Imputation?
# Multiple Imputation (MI) is the theoretical gold standard for statistical
# inference because it correctly accounts for the uncertainty of the
# imputation process. However, it produces 'm' (e.g., 5-10) separate
# complete datasets. For our goal of predictive modeling, this would mean
# training, tuning, and evaluating 'm' different classification models,
# which creates a massive workflow overhead. Since our primary goal is
# predictive accuracy, not the precise estimation of coefficient p-values,
# the high-quality single dataset produced by MissForest is the optimal
# choice, balancing imputation accuracy with practical feasibility.
#

cat("\n--- Stage 1.12: Advanced Imputation using MissForest... ---\n")

# --- Setup Parallel Processing ---
# Best Practice: Use one less than the total number of available cores.
# This keeps the computer responsive by leaving a core for the OS and
# other background tasks.
cores_to_use <- parallel::detectCores() - 1
registerDoParallel(cores = cores_to_use)
cat("Parallel backend registered with", cores_to_use, "cores.\n")

# for reproduceablity
set.seed(42)

# --- Run MissForest ---
# The algorithm will iteratively use Random Forest to predict and fill the
# missing values in the dataset until the imputed values converge.
# This can take a few hours, even with parallel processing.

cat("Starting MissForest imputation... This may take a few hours\n")

# Note: missForest works on a matrix/dataframe. It will automatically
# identify which columns have NAs and impute them.
# 2nd Note: Reduced ntree and maxiter from defaults due to time constraints.
# For a final production model, these could be increased and tuned.
model_data_df <- as.data.frame(model_data) 
missForest_result <- missForest(
  model_data_df,
  parallelize = "forests",
  ntree = 30, # <-- default is 100. turn up if more time is available
  maxiter = 3, # <- default is 10. turn up if more time is available
  verbose = TRUE
)

cat("MissForest imputation complete.\n")

# The imputed dataset is in the $ximp element of the result
model_data <- missForest_result$ximp

# The OOBerror gives an estimate of imputation error.
# For categorical variables (NRMSE is not applicable), it shows the
# Proportion of Falsely Classified (PFC) entries. Lower is better.
cat("Imputation Out-of-Bag (OOB) Error (PFC for categorical):\n")
print(missForest_result$OOBerror)


# --- Cleanup Parallel Processing ---
# It's good practice to stop the cluster to release the cores.
stopImplicitCluster()
cat("Parallel backend stopped.\n")

# --- Final Verification ---
# Double-check that no missing values remain in the entire dataset.
cat("\n--- Final Verification: Checking for any remaining NAs ---\n")
remaining_nas <- colSums(is.na(model_data))
if (all(remaining_nas == 0)) {
  cat("Success! No missing values remain in the dataset.\n")
} else {
  cat("Warning: Some missing values still exist. Please review:\n")
  print(remaining_nas[remaining_nas > 0])
}

cat("\n--- Stage 1.12: Advanced Imputation using MissForest complete. ---\n")


# 5. SAVE GLOBAL DATA (With Weights!)
saveRDS(model_data, "data/intermediate/global_cleaned_data.rds")

# 6. PREPARE STUDENT DATA
student_data <- model_data %>%
  filter(GEMOCCU == "Student") %>%
  filter(hhsize <= 20) %>%
  select(-any_of(get_unnecessary_student_columns()))

saveRDS(student_data, "data/processed/student_model_data.rds")
