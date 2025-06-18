# ===================================================
# --- Explicitly handle missing Values ---
# ===================================================

setwd("C:\\Users\\timor\\Desktop\\Hochschule\\Machine_Learning")
source("EDA\\feature_selection.R")

library(dplyr)
library(scales)
library(forcats)

# ===================================================================
# --- Stage 1: Descriptive Analysis - counting missing values ---
# ===================================================================
cat("\n--- Descriptive Analysis of Missing Values ---\n")
missing_data <- sapply(model_data, function(x) c(
  absolute = sum(is.na(x)),
  relative = round(sum(is.na(x)) / length(x) * 100, 1)
))
missing_df <- as.data.frame(t(missing_data))
missing_df$absolute <- as.integer(missing_df$absolute)
missing_df_sorted <- missing_df[order(-missing_df$absolute), ]
print(missing_df_sorted)

# --- Analysis ---
# There are many columns with a high rate of missing values
# Requires careful treatment, because removing results in very high data loss
# Simple Imputation Techniques like Median might be too simple
# and cover up potential information


# ===================================================================
# --- Stage 2: Hierarchical Clustering ---
# ===================================================================
# Methodology from official GEM Website:
# "To save interview times and costs, the sets of questions related to attitudes
# and perceptions (four questions each) have been randomly assigned over
# two groups of respondents between 2003 and 2009."

# This means, whole subsets of columns have been randomly withheld from individuals
# = Missing Completely at Random (MCAR) with known mechanism 
# Although our dataset is not from this timespan, we can still suspect that this methodology also applies to 2021.

# Some columns have a high number of Missing Values (see Stage 1 output)
# These are likely clustered into groups (answers to questions that are "back to back" in the Questionaire):


# Generate a Dendogram (Hierarchical Cluster of the missing Values)
# Columns with similar "missingness patterns" will be joined together first.
# The result is a tree showing the order of merging.

# We convert the dataframe into a matrix of TRUE/FALSE where TRUE means NA.
# Then convert to 1s and 0s for distance calculation.
missing_matrix <- model_data %>%
  select(-FUTSUPNO) %>% # Exclude the target variable from the analysis
  is.na()

# Filter out columns that have no missing values at all before clustering
cols_with_missing <-
  colnames(missing_matrix)[colSums(missing_matrix) > 0]
missing_matrix_filtered <- missing_matrix[, cols_with_missing]

col_dist <- dist(t(missing_matrix), method = "euclidean")
set.seed(42)
hclust_result <- hclust(col_dist, method = "ward.D2")
par(mar = c(5, 1, 4, 8)) # Bottom, Left, Top, Right

plot(
  as.dendrogram(hclust_result),
  horiz = TRUE, # Plot horizontally for readability
  main = "Hierarchical Clustering of Missing Value Patterns",
  xlab = "Dissimilarity (Distance)",
  sub = "Columns with similar missingness are clustered together"
)

cat("\n--- Generating Dendogram for Hierarchical Clustering...  ---\n")

# --- Analysis ---
# Cluster 1: Columns with low missing values counts (middle)
# The cluster WBINC-SUSKILyy have the lowest missing values count (see missing_df_sorted)
# It makes sense that these columns have been merged together,
# but there is no meaningful information here

# Cluster 2: The "Mindset" Block (bottom)
# Branches are very short = 'missingness' pattern is similar
# The questions are a whole block in the Questionaire (Qi11 - Qi14)
mindset_cols <- c("OPPISMyy", "PROACTyy", "CREATIVyy", "VISIONyy")


# Cluster 3: The "Attitudes & Cultural Support" Block (Middle Bottom)
# They form another cluster & also form a block in the Questionaire (Qi15 - Qi18)
# The official Note from the GEM website exlicitly mentioned this group:
# "To save interview times... (see above)"
attitudes_perceptions_cols <- c("CULSUPyy", "EQUALIyy", "NBGOODyy", "NBSTATyy", "NBMEDIyy")
# Note: CULSUPyy is an index derived from the other 4x variables.
# If one of the other 4x columns is missing, then CULSUPyy will be missing.
# Hence, it has the highest absolute missing count of them all (see missing_df_sorted)


# ===================================================================
# --- Stage 3: Analyze Missingness Blocks ---
# ===================================================================
# Objective: Determine size of overlap between missing values in the clusters

# Let's numerically check this
analyze_missingness_block <- function(data, cols) {
  # Ensure all specified columns exist in the data to prevent errors
  if (!all(cols %in% names(data))) {
    stop("One or more specified columns are not in the dataframe.")
  }

  summary_table <- data %>%
    # For each row, count how many of the specified columns are NA
    mutate(
      missing_count = rowSums(is.na(select(., all_of(cols))))
    ) %>%
    # Categorize each row based on the count
    mutate(
      missing_status = case_when(
        missing_count == 0 ~ "All Present",
        missing_count == length(cols) ~ "All Missing",
        TRUE ~ "Partially Missing"
      )
    ) %>%
    # Count the number of rows in each category
    count(missing_status) %>%
    # Calculate the percentage for clarity
    mutate(percentage = scales::percent(n / sum(n), accuracy = 0.1))

  return(summary_table)
}


mindset_summary <- analyze_missingness_block(model_data, mindset_cols)
attitudes_summary <- analyze_missingness_block(model_data, attitudes_perceptions_cols)

# --- Print the Results ---
cat("--- Analysis of 'Mindset' Cluster ---\n")
print(mindset_summary)
# --- Analysis ---
# About half the sample (48.7%) was not asked these questions.
# -> split-sample design appears to be roughly 50/50
# Within the group that was asked, the partial non-response rate is significant.
# 12645 / (47538 + 12645) = 21%
# -> Over one-fifth failed to provide a clear answer for all four
# High, but plausible for these abstract mindset questions
# -> Seperate Treatment for All-Missing and Partially Missing

cat("\n--- Analysis of 'Attitudes / Perceptions' Cluster ---\n")
print(attitudes_summary)
# --- Analysis ---
# "All Missing" group is only 35.1%
# -> split-sample design might not be simply 50/50
# Within the group that was asked, the partial non-response rate is enormous
# 44079 / (32045 + 44079) = 58%
# -> This set of Questions seem to be very difficult to answer fully
# Imputating so many rows with an average would potentially introduce
# A LOT of bias into the data.
# -> Remove these columns.

model_data <- model_data %>% select(-any_of(attitudes_perceptions_cols))
cat(
  "Attitudes/Perceptions Columns removed due to high partial non-response. Remaining columns:",
  ncol(model_data), "\n"
)


# --- Analysis of FRFAILOP ---
# FRFAILOP = "Fear of failure (in 18-64 sample perceiving good opportunities)"
# This column is missing in 61.8% of cases due to its dependency on other
# answers. Its information is largely captured by other variables (FRFAILyy,
# OPPORTyy, age).
# -> Decision: Drop the column to avoid dealing with complex structural missingness.
model_data <- model_data %>% select(-FRFAILOP)
cat(
  "FRFAILOP column removed due to structural missingness. Remaining columns:",
  ncol(model_data), "\n"
)


# ===================================================================
# --- Stage 4: Feature Engineering from Missingness Patterns ---
# ===================================================================
# Instead of simple imputation, we create new features to capture the
# information contained within the *pattern* of missingness itself.

cat("\n--- Stage 4: Engineering features from missingness patterns ---\n")

model_data <- model_data %>%
  mutate(
    # Methodology: Create a binary flag 'Mindset_Asked' to permanently label
    # rows based on the reason for missingness (split-sample design vs.
    # partial non-response). This captures the survey's design as a feature.
    Mindset_Asked = as.factor(
      ifelse(
        rowSums(is.na(select(., all_of(mindset_cols)))) == length(mindset_cols),
        "Not_Asked",
        "Asked"
      )
    ),

    # Methodology: Since 'age' has a High/ Moderate Missingness (5.9%),
    # we create a binary flag to capture the act of
    # not providing an age, which could be a predictive behavior in itself.
    # We then impute the numeric value in the next stage.
    age_is_missing = as.factor(ifelse(is.na(age), "Yes", "No"))
  )

cat("Created 'Mindset_Asked' and 'age_is_missing' flags.\n")


# ===================================================================
# --- Stage 5: Advanced Imputation with MissForest ---
# ===================================================================

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

cat("\n--- Stage 5: Advanced Imputation using MissForest ---\n")

library(missForest)
library(doParallel)

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
# This can take a few minutes, even with parallel processing.
cat("Starting MissForest imputation... This may take a few minutes.\n")

# Note: missForest works on a matrix/dataframe. It will automatically
# identify which columns have NAs and impute them.
missForest_result <- missForest(
  model_data,
  parallelize = "variables",
  ntree = 50,
  maxiter = 6,
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


# ===================================================================
# --- Stage 6: Save Cleaned Data to Disk ---
# ===================================================================
# To avoid re-running this entire time-consuming cleaning and imputation
# process every time, we save the final, clean dataframe to a file.
# We use saveRDS() because it perfectly preserves all R data types,
# including factor levels, which is critical for our modeling scripts.

cat("\n--- Stage 6: Saving cleaned data to disk ---\n")

# Define the path for the cleaned data file
# It's good practice to have a 'cleaned' subfolder in your data directory
model_data_path <- "EDA/output/cleaned_model_data.rds"

# Create the directory if it doesn't exist to prevent errors
dir.create(dirname(model_data_path), showWarnings = FALSE, recursive = TRUE)

# Save the single 'model_data' object
saveRDS(model_data, file = model_data_path)

cat("Cleaned data successfully saved to:", model_data_path, "\n")