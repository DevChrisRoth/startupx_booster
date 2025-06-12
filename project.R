# ===================================================================
# Machine Learning Project: Predicting Entrepreneurship
# Author: Timo Rolf
# Date: 12.06.2025
# ===================================================================

# --- 0. SETUP & LIBRARIES ---
# Load all required libraries here
library(haven)
library(data.table)
# ... etc.


setwd("C:\\Users\\timor\\Desktop\\Hochschule\\Machine_Learning")

# --- 1. DATA LOADING & CONVERSION ---
# Define file paths
file_name <- "GEM2021APSGlobalIndividualLevelData_15Feb2023"

# Check if the final CSV exists. If not, create it from the SAV file.
sav_file_path <- sprintf("data/%s.sav", file_name)
csv_file_path <- sprintf("data/%s.csv", file_name)

if (!file.exists(csv_file_path)) {
  # If the source .sav file is also missing, stop with an error.
  if (!file.exists(sav_file_path)) {
    stop("Data file not found. Please place the .sav file in the 'data' folder.")
  }

  # Perform the one-time conversion
  cat("Converting .sav to .csv... this may take a moment.\n")
  library(haven)
  library(data.table)
  fwrite(read_sav(sprintf("data/%s.sav", file_name)), csv_file_path)
}

# Load the data from the (now guaranteed to exist) CSV file
gem_data <- fread(csv_file_path)


# --- 2. DATA PREPROCESSING & FEATURE ENGINEERING ---
# Subset columns, handle missing values, create new features


# --- 3. EXPLORATORY DATA ANALYSIS (EDA) ---
# Code for the plots in your presentation


# --- 4. MODEL TRAINING & EVALUATION ---
# Train/Test split, build models, generate confusion matrices


# --- 5. HYPERPARAMETER TUNING ---
# Code for cross-validation and tuning


# --- 6. FINAL MODEL & CONCLUSION ---
# Final model training on full training data, final evaluation
