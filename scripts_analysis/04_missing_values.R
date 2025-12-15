# ==============================================================================
# ANALYSIS: MISSING VALUES
# ==============================================================================

source("scripts_production/00_utils_preprocessing.R") # Load the shared logic

# 1. Load Data using the shared function
# This ensures we are analyzing the EXACT same columns that would go into the model
gem_data <- load_data()
model_data <- scope_data(gem_data)

# --- Stage 1.8: Descriptive Analysis - counting missing values ---
cat("\n--- Stage 1.8: Descriptive Analysis of Missing Values ---\n")
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
# Requires careful treatment, because removing results in very high data loss.
# Simple Imputation Techniques like Median might be too simple
# and cover up potential information
cat("--- Stage 1.8: Missing Values analyzed. ---\n\n")



# --- Stage 1.9: Hierarchical Clustering of Missing Values ---
cat("--- Stage 1.9: Finding Patterns of Missing Values... ---\n\n")
# Methodology from official GEM Website:
# "To save interview times and costs, the sets of questions related to attitudes
# and perceptions (four questions each) have been randomly assigned over
# two groups of respondents between 2003 and 2009."

# This means, whole subsets of columns have been randomly withheld from individuals
# = Missing Completely at Random (MCAR) with known mechanism 
# Although our dataset is not from this timespan, we can still suspect that this methodology also applies to 2021.

# Some columns have a high number of Missing Values (see Stage 1.1 output)
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
mindset_cols <- get_mindset_columns()


# Cluster 3: The "Attitudes & Cultural Support" Block (Middle Bottom)
# They form another cluster & also form a block in the Questionaire (Qi15 - Qi18)
# The official Note from the GEM website exlicitly mentioned this group:
# "To save interview times... (see above)"
attitudes_perceptions_cols <- get_attitude_columns()
# Note: CULSUPyy is an index derived from the other 4x variables.
# If one of the other 4x columns is missing, then CULSUPyy will be missing.
# Hence, it has the highest absolute missing count of them all (see missing_df_sorted)
cat("--- Stage 1.9: Patterns of Missing Values found: \"Mindset\" and \"Attitudes & Cultural Support\" Block ---\n\n")


# --- Stage 1.10: Analyze Missingness Blocks ---
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
attitudes_summary <- analyze_missingness_block(model_data, attitudes_cols)

# --- Print the Results ---
cat("--- Stage 1.10: Analysis of 'Mindset' Cluster ---\n")
print(mindset_summary)
# --- Analysis ---
# About half the sample (48.3%) was not asked these questions.
# -> split-sample design appears to be roughly 50/50
# Within the group that was asked, the partial non-response rate is significant.
# 11554 / (46561 + 11554) = 19.88%
# -> Nearly one-fifth failed to provide a clear answer for all four
# High, but plausible for these abstract mindset questions
# -> Seperate Treatment for All-Missing and Partially Missing

cat("\n--- Stage 1.10: Analysis of 'Attitudes / Perceptions' Cluster ---\n")
print(attitudes_summary)
# --- Analysis ---
# "All Missing" group is only 35.0%
# -> split-sample design might not be simply 50/50
# Within the group that was asked, the partial non-response rate is enormous
# 42003 / (31067 + 42003) = 57.48%
# -> This set of Questions seem to be very difficult to answer fully
# Imputating so many rows even with MissForest would potentially introduce
# A LOT of bias into the data.
# -> Remove these columns.
