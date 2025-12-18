# ===================================================================
# FINAL SUBMISSION SCRIPT: PREDICTING STUDENT ENTREPRENEURIAL INTENTION
# Author: Timo Rolf
# Course: Maschinelles Lernen (SS2025)
# Professor: Prof. Dr. Robert Hable
# GitHub: https://github.com/20timo05/GEM_Research
# ===================================================================

# --- 0.1: Install and Load Libraries ---
# This block ensures all necessary packages are installed and loaded.
required_packages <- c(
  "tidyverse",        # A meta-package including ggplot2, dplyr, stringr, purrr, etc.
  "tidymodels",       # A framework for modeling and machine learning.
  "data.table",       # For fast data manipulation.
  "haven",            # To read data from formats like SPSS, Stata, and SAS.
  "themis",           # For handling class imbalance in models (part of tidymodels).
  "glmnet",           # For fitting regularized models (Lasso, Ridge).
  "vip",              # For creating variable importance plots.
  "doParallel",       # For enabling parallel processing to speed up computations.
  "doFuture",         # Alternative to doParallel that is the current best practice.
  "caret",            # An alternative, comprehensive framework for modeling.
  "patchwork",        # For easily combining multiple ggplot plots.
  "sf",               # For handling spatial data (Simple Features).
  "rnaturalearth",    # Provides world map data for plotting.
  "rnaturalearthdata",# The data backbone for rnaturalearth.
  "viridis",          # Provides colorblind-friendly color palettes.
  "missForest",       # For MissForest Imputation
  "DALEX",            # For SHAP Plots
  "DALEXtra"          # For SHAP Plots
)

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

# ===================================================================
# --- SECTION 1: DATA CLEANING & PREPARATION ---
# ===================================================================
# This section contains the full data cleaning pipeline, including
# target leakage removal, handling of missing values using hierarchical
# clustering and missForest, and final data type conversion.


# --- Stage 1.0: DATA LOADING & CONVERSION ---
# Define file name
file_name <- "GEM2021APSGlobalIndividualLevelData_15Feb2023"

# Define file paths for the root project directory
sav_file_path <- sprintf("%s.sav", file_name)
csv_file_path <- sprintf("%s.csv", file_name)

# Check if the final CSV exists. If not, create it from the SAV file.
if (!file.exists(csv_file_path)) {
  # If the source .sav file is also missing, stop with an informative error.
  if (!file.exists(sav_file_path)) {
    stop(sprintf(
      "Data file '%s.sav' not found in the project's root directory.\n\nPlease download the 'GEM 2021 APS Global Individual Level Data_15 February 2023' from:\nhttps://www.gemconsortium.org/data/sets?id=aps\n\nThen, place the downloaded .sav file into your main project folder and re-run the script.",
      file_name
    ))
  }
  
  # Perform the one-time conversion
  cat("Converting .sav to .csv... this may take a moment.\n")
  fwrite(read_sav(sav_file_path), csv_file_path)
}

# Load the data from the (now guaranteed to exist) CSV file
library(data.table)
gem_data <- fread(csv_file_path)

cat("--- Stage 1.0: Data loaded successfully from the root directory.\n")

# --- Stage 1.1: Initial Scoping ---
# Objective: Filter the dataset to only include respondents where the target
# variable is known and their age falls into the defined 18-64 range.
# This defines our modeling population.
output_base_dir <- "output_final"
cleaned_model_data_name = sprintf("%s/cleaned_model_data.rds", output_base_dir)

if (file.exists(cleaned_model_data_name)) {
  model_data <- readRDS(cleaned_model_data_name)

} else {

  model_data <- gem_data %>% filter(!is.na(FUTSUPNO))
  model_data <- model_data %>% filter((age >= 18 & age <= 64) | is.na(age))
  cat("--- Stage 1.1: Data scoped successfully. Rows with known target variable:", nrow(model_data), "---\n\n")


  # --- Stage 1.2: Remove Target Leakage Variables ---
  # Objective: Remove all detailed follow-up questions from sections that are
  # only answered by individuals already engaged in entrepreneurial activity.
  # This is the most critical step to prevent the model from "cheating."
  columns_to_drop_stage2 <- c(
    # --- Start-Up (SU) Section & All Derived Variables ---
    "suacts", "suown", "suowners", "suwage", "suwageyr", "supay2", "suwhen",
    "suoppor", "suyr5job", "sunewcst", "sucompet", "sunewtec", "suexport",
    "sucstloc", "sucstnat", "sucstexp", "sunewprod", "sunewproc", "sunowjob",
    "sucrstart", "sucrgrow", "sumotiv1", "sumotiv2", "sumotiv3", "sumotiv4",
    "sucpnewopp", "sucpgovres", "sucptech1", "sucptech2", "susdg_soc",
    "susdg_env", "susdg_pri", "susdg_steps1", "susdg_steps2",
    "susdg_aware1", "susdg_aware2", "su_om", "su_om_ee",
    "subustype", "SUTYPE_ISIC4", "SUTYPE_R", "eng_subustype", "subustype_code",
    "SU_ISIC4_4D", "SU_HITEC", "su_yyTEC", "SU_SIC4C", "SU_ISIC4_1D",
    "SU_JOB5Y", "SU_JOBNW", "su_yyj5y", "su_yyjnw", "su_yyemp", "su_yyhjg",
    "SU_JOBGR", "SU_JOBGP", "su_yyjob", "su_yyexp", "su_yykix", "su_yyhix",
    "SU_EXP4C", "SU_OWNER", "SU_EXP5C", "SU_yyMOT1", "SU_yyMOT2", "SU_yyMOT3",
    "SU_yyMOT4", "SU_yyMOT1yes", "SU_yyMOT2yes", "SU_yyMOT3yes", "SU_yyMOT4yes",
    "SU_MKSC", "SU_CSTLOC", "SU_CSTNAT", "SU_CSTEXP", "SU_NEWPROD", "SU_NEWPROC",
    "SU_SPON", "SU_IND", "SU_CRSTART", "SU_CRGROW", "SU_CPNEWOPP", "SU_CPGOVRES",

    # --- Owner-Manager (OM) & Derived Baby/Established Business (BB/EB) Variables ---
    "omown", "omwage", "omwageyr", "omwhen", "omopport", "omyr5job",
    "omowners", "ompay2", "omcstloc", "omcstnat", "omcstexp", "omexport",
    "omnewprod", "omnewproc", "omnowjob", "omcrstart", "omcrgrow",
    "ommotiv1", "ommotiv2", "ommotiv3", "ommotiv4", "omrstart",
    "omcrnewopp", "omcrgovres", "omcrtech1", "omcrtech2", "omsdg_soc",
    "omsdg_env", "omsdg_pri", "omsdg_steps1", "omsdg_steps2",
    "omsdg_aware1", "omsdg_aware2", "omwgyr_c",
    "ombustype", "OMTYPE_ISIC4", "OMTYPE_R", "eng_ombustype", "ombustype_code",
    "BB_ISIC4_4D", "BB_HITEC", "bb_yyTEC", "BB_SIC4C", "BB_ISIC4_1D",
    "BB_JOB5Y", "BB_JOBNW", "bb_yyj5y", "bb_yyjnw", "bb_yyemp", "bb_yyhjg",
    "BB_JOBGR", "BB_JOBGP", "bb_yyjob", "bb_yyexp", "bb_yykix", "bb_yyhix",
    "BB_EXP4C", "BB_OWNER", "BB_EXP5C", "BB_yyMOT1", "BB_yyMOT2", "BB_yyMOT3",
    "BB_yyMOT4", "BB_yyMOT1yes", "BB_yyMOT2yes", "BB_yyMOT3yes", "BB_yyMOT4yes",
    "BB_MKSC", "BB_CSTLOC", "BB_CSTNAT", "BB_CSTEXP", "BB_NEWPROD", "BB_NEWPROC",
    "BB_SPON", "BB_IND", "BB_CRSTART", "BB_CRGROW", "BB_CPNEWOPP", "BB_CPGOVRES",
    "EB_ISIC4_4D", "EB_HITEC", "EB_yyTEC", "EB_SIC4C", "EB_ISIC4_1D",
    "EB_yyJ10", "EB_JOB5Y", "EB_JOBNW", "EB_yyJ5Y", "EB_yyJNW", "EB_yyEMP",
    "EB_yyHJG", "EB_JOBGR", "EB_JOBGP", "EB_yyJOB", "eb_yyexp", "eb_yykix",
    "eb_yyhix", "EB_EXP4C", "EB_OWNER", "EB_EXP5C", "EB_yyMOT1", "EB_yyMOT2",
    "EB_yyMOT3", "EB_yyMOT4", "EB_yyMOT1yes", "EB_yyMOT2yes", "EB_yyMOT3yes",
    "EB_yyMOT4yes", "EB_MKSC", "EB_CSTLOC", "EB_CSTNAT", "EB_CSTEXP",
    "EB_NEWPROD", "EB_NEWPROC", "EB_SPON", "EB_IND", "EB_CRSTART", "EB_CRGROW",
    "EB_CPNEWOPP", "EB_CPGOVRES",

    # --- Total Early-stage Activity (TEA) Derived Variables ---
    "TEAISIC4_4D", "TEAHITEC", "TEAyyTEC", "TEASIC4C", "TEAISIC4_1D",
    "TEAyyS1C", "TEAyyS2C", "TEAyyS3C", "TEAyyS4C", "TEAyyJ5Y", "TEAyyJNW",
    "teayyj5yr", "TEAyyEMP", "TEAyyHJG", "TEAJOBGR", "TEAJOBGP", "TEAyyJOB",
    "TEAyyEXP", "teayykix", "teayyhix", "TEAEXP4C", "TEAOWNER", "TEAEDHI",
    "TEAJOBEX", "TEAEXP5C", "TEAEXPST", "TEAEXPWK", "TEAyyMOT1", "TEAyyMOT2",
    "TEAyyMOT3", "TEAyyMOT4", "TEAyyMOT1yes", "TEAyyMOT2yes", "TEAyyMOT3yes",
    "TEAyyMOT4yes", "TEAyyMKSC", "TEACSTLOC", "TEACSTNAT", "TEACSTEXP",
    "TEANEWPROD", "TEANEWPROC", "TEAIMPACT1", "TEAIMPACT2", "TEAyyIND",
    "TEAyySPON", "TEAactivityr", "TEACRSTART", "TEACRGROW", "TEACPNEWOPP",
    "TEACPGOVRES",

    # --- Business Exit Section & Derived Variables ---
    "exbuscon", "exreason", "exreason_oth", "eng_exreason_oth", "EXREAS_O",
    "EXIT_RS", "EXIT_CTD", "EXIT_ENT",

    # --- Informal Investor (Angel) Section & Derived Variables ---
    "bafund", "barel", "barel_oth", "eng_barel_oth", "BAREL_O", "BAFUNDUS",
    "BUSANGVL",

    # --- Intrapreneurship (EEA/IP) Section & Derived Variables ---
    "ipactive", "ipactivenow", "ipphase1", "ipphase1role", "ipphase2",
    "ipphase2role", "ipinit", "ipteamsize", "ipjob5yr", "ipcstloc",
    "ipcstnat", "ipcstexp", "ipexport", "ipnewprod", "ipnewproc",
    "ipcrnewopp", "ipcrgovres", "EEA_NEWOPP", "EEA_GOVRES",
    "IPACT_ALL", "IPACT_EMP", "IPACTNOW_ALL", "IPACTNOW_EMP", "IPACTLD_ALL",
    "IPACTLD_EMP", "IPACTLDNOW_ALL", "IPACTLDNOW_EMP", "IP_MKSC",
    "IPNOW_MKSC", "IP_NEWPROD", "IPNOW_NEWPROD", "IP_NEWPROC",
    "IPNOW_NEWPROC", "IPIMPACT1", "IPNOWIMPACT1", "IPIMPACT2",
    "IPNOWIMPACT2", "IP_yyEXP", "IPNOW_yyEXP", "IP_yyKIX", "IPNOW_yyKIX",
    "ip_yyhix", "ipnow_yyhix", "IP_JOB5Y", "IPNOW_JOB5Y", "ip_yyj5y",
    "ipnow_yyj5y", "IP_yyHJG", "IPNOW_yyHJG", "IP_EXP4C", "IPNOW_EXP4C",

    # --- Other Firm/Activity-Related Variables ---
    "extyytec", "extyyjob", "exthitec", "BUSOWNER", "BUSOWN_A", "BO_EXAGR",
    "TEAyyMAL", "TEAyyFEM", "TEAyyMMOT1", "TEAyyFMOT1", "TEAyyMMOT2",
    "TEAyyFMOT2", "TEAyyMMOT3", "TEAyyFMOT3", "TEAyyMMOT4", "TEAyyFMOT4",
    "Bstartyy", "BJOBSTyy", "OWNMGEyy", "OMJOBSTyy", "BUSANGyy", "DISCENyy",
    "SUB", "SUBA", "SUBO", "SUBOA", "SUBOANW", "OMBABYX", "OMESTBX",
    "BABYBUSM", "BABYBUSO", "ESTBBUSM", "ESTBBUSO", "SUBOANWC",
    "SUBOANW1", "BABYBUS1", "ESTBBUS1", "TEAyy", "TEAyySTA", "ANYBUSOW",
    "ENTRACTALL", "ENTRACTRED", "ENTRACTEA", "ENTRACTEA2", "ENTRACTEA3",

    # --- Direct Parent of Target Variable ---
    "FUTSUPyy"
  )

  # Execute the cleanup
  model_data <- model_data %>% select(-any_of(columns_to_drop_stage2))
  cat("--- Stage 1.2: Leakage variables removed. Columns remaining:", ncol(model_data), "---\n\n")

  # --- Stage 1.3: Drop Raw Variables in Favor of Harmonized Versions ---
  # Objective: Use the official GEM harmonized variables (...yy) for modeling,
  # as they represent the official methodology. They don't give the raw Likert Scale answers
  # to specific questions, but a clear binary value. Don't know and reclassifications
  # due to later questions in the questionaire are already applied, which ensures a far cleaner dataset.
  # This involves removing the original, more granular variables they were derived from.
  columns_to_drop_stage3 <- c(
    # Core Activity Filter Questions (Replaced by ...yy flags)
    "bstart", "bjobst", "ownmge", "omjobst", "discent", "futsup", "busang",

    # Raw Perceptual & Attitude Variables (Replaced by ...yy flags)
    "knowentR", "opportL", "opportR", "suskillL", "suskillR", "fearfailL",
    "fearfailR", "easystartL", "equalincL", "nbgoodcL", "nbstatusL",
    "nbmediaL",

    # 5-point Likert Scales (Replaced by ...yy flags)
    "oppism", "proact", "creativ", "vision", "nbsocentL"
  )
  model_data <- model_data %>% select(-any_of(columns_to_drop_stage3))
  cat("--- Stage 1.3: Raw variables removed. Columns remaining:", ncol(model_data), "---\n\n")


  # --- Stage 1.4: Remove Redundant Disaggregated Variables ---
  # Objective: To avoid multicollinearity and simplify the model, we will use the
  # harmonized summary flags (e.g., OPPISMyy) and remove the more granular
  # binary flags they were derived from (e.g., OPPIS_1, OPPIS_2).
  # Multicollinearity destabilizes the coefficients, i.e. a small change in the
  # dataset might have a big impact on the value of the coefficients.
  # Consideration: Use the more granular Binary Flags for more
  # information as an experiment to improve performance evaluation metrics.
  columns_to_drop_stage4 <- c(
    # Disaggregated Mindset Variables
    "OPPIS_1", "OPPIS_2", "OPPIS_3",
    "PROAC_1", "PROAC_2", "PROAC_3",
    "CREATI_1", "CREATI_2", "CREATI_3",
    "VISIO_1", "VISIO_2", "VISIO_3",
    "SOCEN_1", "SOCEN_2", "SOCEN_3",

    # Disaggregated Support Variables
    "INDSPyyA", "INDSPyyB", "INDSPyyC", "INDSPyyD",
    "CULSPyyA", "CULSPyyB", "CULSPyyC", "CULSPyyD",

    # Disaggregated Occupation Variables
    "GEMWORK", "GEMWORK3", "occufull", "occupart", "occuself",
    "occuprbs", "occugov", "occunfp", "occuseek", "occustu",
    "occuump", "occuhome", "occurt", "occuds",

    # Redundant Education Variables (based on different Education Categories: GEM, UN & 97)
    "UNEDUC", "UNEDUC97",

    # World Bank WBINC provides a little more granularity over WBincREV
    "WBincREV"
  )

  model_data <- model_data %>% select(-any_of(columns_to_drop_stage4))
  cat(
    "--- Stage 1.4: Redundant disaggregated variables removed. Columns remaining:",
    ncol(model_data), "---\n\n"
  )


  # --- Stage 1.5: Remove Non-Predictor & Metadata Columns ---
  # Objective: Remove columns that are not features of the individual, such as
  # IDs, redundant codes, survey weights, and survey process metadata.
  columns_to_drop_stage5 <- c(
    # IDs and redundant country codes
    "setid", "ID", "country_name", "country", "REGION",

    # Redundant binned age columns (we use the numeric 'age')
    "age7c", "age9c",

    # Survey weights (not used for this classification task)
    # These are applied so that the sample is representative to the national population
    # Could be explored for real world predictive power across the population, or comparing national statistics.
    # (e.g., "25% of the German population intends to start a business",
    # whereas the goal here is predicting the outcome for a given individual)
    "weight", "WEIGHT_L", "WEIGHT_A",

    # Survey process metadata
    "dtsurv", "MSURV", "contact", "callback", "incontact", "doublecount",

    # Historical demographic questions (pre-COVID)
    "bc_occufull", "bc_occupart", "bc_occuself", "bc_occuseek"
  )
  model_data <- model_data %>% select(-any_of(columns_to_drop_stage5))
  cat("--- Stage 1.5: Metadata removed. Columns remaining:", ncol(model_data), "---\n\n")


  # --- Stage 1.6: Automated Data Hygiene ---
  # Objective: Use automated checks as a final safety net to find and remove
  # columns with no variance and to flag other potential issues.
  cat("--- Stage 1.6: Running Automated Data Sanity Checks ---\n")
  zv_cols <- names(which(sapply(model_data, function(x) length(unique(x)) == 1)))
  if (length(zv_cols) > 0) {
    cat("Found", length(zv_cols), "zero-variance columns:", paste(zv_cols, collapse = ", "), "\n")
    model_data <- model_data %>% select(-any_of(zv_cols))
    cat("---------- Automatically removing zero variance columns... ---------- \n")
  } else {
    cat("No zero-variance columns found.\n")
  }

  nzv_cols <- nearZeroVar(model_data, names = TRUE)
  cat("Found", length(nzv_cols), "near-zero-variance columns:", paste(nzv_cols, collapse = ", "), "\n")
  cat("--- Stage 1.6: Finished Automated Data Sanity Checks. Columns remaining:", ncol(model_data), "---\n\n")


  # --- Stage 1.7: Correct Datatypes ---
  # Objective: Define the correct data type for each remaining feature before modeling.
  cat("--- Stage 1.7: Correcting data types for modeling ---\n")

  numeric_cols <- c(
    "age", # Respondent's age in years. (Continuous)
    "hhsize" # Number of people in the household. (Discrete Count)
  )

  ordinal_cols <- c(
    "WBINC", # World Bank Income Level of the country (e.g., Low, Lower-middle, Upper-middle, High).
    "GEMHHINC", # Harmonized Household Income, typically in terciles (e.g., Lowest 1/3, Middle 1/3, Top 1/3).
    "GEMEDUC", # Harmonized Education Level (e.g., None, Secondary, Post-secondary, Graduate).
    "cphhinc" # Change in household income due to COVID-19 (e.g., Decreased, Stayed the same, Increased).
  )

  factor_cols <- c(
    # --- Contextual & Demographic ---
    "ctryalp", # Country Identifier (3-letter code, e.g., "USA", "DEU").
    "gender", # Respondent's gender.
    "GEMOCCU", # Harmonized Occupation Status (e.g., Employee, Self-Employed, Student).

    # --- Core Perceptual & Attitude Binary Flags (Yes/No) ---
    "KNOWENyy", # Knows an entrepreneur personally.
    "OPPORTyy", # Perceives good opportunities to start a business.
    "SUSKILyy", # Perceives having the required skills to start a business.
    "FRFAILyy", # Fear of failure would prevent starting a business.
    "EASYSTyy", # Believes it is easy to start a business in their country.
    "EQUALIyy", # Attitude: Prefers similar standards of living for everyone.
    "NBGOODyy", # National Belief: Starting a business is a desirable career choice.
    "NBSTATyy", # National Belief: Successful entrepreneurs have high status.
    "NBMEDIyy", # National Belief: Media gives attention to successful businesses.

    # --- Mindset & Support Binary Flags (Yes/No) ---
    "OPPISMyy", # Mindset: Generally optimistic about life.
    "PROACTyy", # Mindset: Proactive in taking initiative.
    "CREATIVyy", # Mindset: Sees oneself as creative/inventive.
    "VISIONyy", # Mindset: Has a clear vision for their professional future.
    "INDSUPyy", # Perceived Support: Believes there is support for new/growing firms.
    "CULSUPyy", # Perceived Support: Believes national culture is supportive.

    # --- Interaction & Target Binary Flags (Yes/No) ---
    "FRFAILOP", # Interaction: Experiences fear of failure despite seeing opportunities.
    "FUTSUPNO" # TARGET: Intends to start a business, but is not currently active.
  )

  # Apply the changes
  all_factor_cols <- c(ordinal_cols, factor_cols)

  model_data <- model_data %>%
    mutate(
      across(all_of(numeric_cols), as.numeric),
      across(all_of(all_factor_cols), as.factor)
    )

  cat("--- Stage 1.7: Datatypes corrected. ---\n\n")


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
  mindset_cols <- c("OPPISMyy", "PROACTyy", "CREATIVyy", "VISIONyy")


  # Cluster 3: The "Attitudes & Cultural Support" Block (Middle Bottom)
  # They form another cluster & also form a block in the Questionaire (Qi15 - Qi18)
  # The official Note from the GEM website exlicitly mentioned this group:
  # "To save interview times... (see above)"
  attitudes_perceptions_cols <- c("CULSUPyy", "EQUALIyy", "NBGOODyy", "NBSTATyy", "NBMEDIyy")
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
  attitudes_summary <- analyze_missingness_block(model_data, attitudes_perceptions_cols)

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

  model_data <- model_data %>% select(-any_of(attitudes_perceptions_cols))
  cat(
    "--- Stage 1.10: Attitudes/Perceptions Columns removed due to high partial non-response. Remaining columns:",
    ncol(model_data), " ---\n"
  )

  # --- Analysis of FRFAILOP ---
  # FRFAILOP = "Fear of failure (in 18-64 sample perceiving good opportunities)"
  # This column is missing in 61.8% of cases due to its dependency on other
  # answers. Its information is largely captured by other variables (FRFAILyy,
  # OPPORTyy, age).
  # -> Decision: Drop the column to avoid dealing with complex structural missingness.
  model_data <- model_data %>% select(-FRFAILOP)
  cat(
    "--- Stage 1.10: FRFAILOP column removed due to structural missingness. Remaining columns:",
    ncol(model_data), " ---\n"
  )

  # --- Stage 1.11: Feature Engineering from Missingness Patterns ---
  # Instead of simple imputation, we create new features to capture the
  # information contained within the *pattern* of missingness itself.

  high_missing_categorical <- c("INDSUPyy", "EASYSTyy", "GEMHHINC", "OPPORTyy", "SUSKILyy")

  model_data <- model_data %>%
    mutate(
      # --- Step 1: Create the 'Mindset_Asked' Flag ---
      # This correctly identifies respondents based on the survey's split-sample design.
      # This step remains unchanged.
      Mindset_Asked = as.factor(
        ifelse(
          rowSums(is.na(select(., all_of(mindset_cols)))) == length(mindset_cols),
          "Not_Asked",
          "Asked"
        )
      ),

      # --- Step 2: Recode Mindset Columns with Detailed Missingness ---
      # This is the new, more precise logic. It replaces the original 'across' call.
      across(all_of(mindset_cols), ~ {
        # The 'case_when' function checks conditions in order.
        factor(case_when(
          # If the respondent was NOT asked, the question is not applicable to them.
          Mindset_Asked == "Not_Asked" ~ "Not_Applicable",
          
          # If the respondent WAS asked, but the value is still NA, they refused to answer.
          Mindset_Asked == "Asked" & is.na(.) ~ "Refused_Answer",
          
          # Otherwise (they were asked and they answered), keep their original response.
          # We convert it to character to ensure it combines with the other text levels.
          TRUE ~ as.character(.) 
        ))
      }),

      # --- Handle High/Moderate Missingness Categorical Columns (>5%) ---
      # For these, we create a new factor level to treat "missing" as information.
      # This applies to: INDSUPyy, EASYSTyy, GEMHHINC, OPPORTyy, SUSKILyy
      across(
        all_of(high_missing_categorical),
        ~ fct_na_value_to_level(., level = "Unknown")
      ),
      # Methodology: Since 'age' has a High/ Moderate Missingness (5.9%),
      # we create a binary flag to capture the act of
      # not providing an age, which could be a predictive behavior in itself.
      # We then impute the numeric value in the next stage.
      age_is_missing = as.factor(ifelse(is.na(age), "Yes", "No"))
    )


  cat("--- Stage 1.11: 'Mindset_Asked' flag created. ---\n")
  cat("--- Stage 1.11: Mindset columns recoded into 'Not_Applicable' and 'Refused_Answer'. ---\n")


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
  missForest_result <- missForest(
    model_data,
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


  
  # --- Stage 1.13: Save Cleaned Data to Disk ---
  # Objective: To avoid re-running this entire time-consuming cleaning and imputation
  # process every time, we save the final, clean dataframe to a file.

  cat("\n--- Stage 1.13: Saving cleaned data to disk ---\n")

  dir.create(dirname(cleaned_model_data_name), showWarnings = FALSE, recursive = TRUE)
  saveRDS(model_data, file = model_data_path)

  cat("Cleaned data successfully saved to:", model_data_path, "\n")
}


# --- Stage 1.13: Relabel Categories ---
# Objective: Rename the numeric codes to human readable labels.
# e.g.: 1-5 -> "Strongly Disagree"-"Strongly Agree"



# This will show the new factor columns with their labeled counts.
cat("\n--- Stage 1.13: Data Summary after Relabeling ---\n")
summary(model_data)
cat("\n--- Stage 1.13: Relabeling complete. ---\n")

cat("--- DATA CLEANING AND PREPARATION COMPLETE ---\n")




# ===================================================================
# --- SECTION 2: EXPLORATORY DATA ANALYSIS (EDA) ---
# ===================================================================
# PURPOSE:
# This section performs a detailed exploratory data analysis specifically on the
# student sub-population from the GEM dataset. The goal is to uncover insights
# relevant to the DIT StartUp Center use case.
#
# METHODOLOGY:
# We first analyze the global dataset to understand broad trends. Then, we
# filter for respondents whose occupation is 'Student' and re-run the key
# analyses. By comparing the student-specific results to the global results,
# we can identify the unique characteristics, motivations, and barriers of
# entrepreneurial students.
# 
# CONSIDERATIONS
# This project uses unweighted data for the classification task. The survey weights were omitted,
# meaning our sample is likely not representative of the general population and may over-represent
# individuals interested in entrepreneurship. This is a deliberate trade-off, as our goal is to
# predict individual outcomes within the sample, not to perform population-level inference.

# Helper Functions for plotting
plot_univariate_dist <- function(data,
                                 feature_col,
                                 title,
                                 subtitle = "",
                                 xlab = "",
                                 bar_color = "darkcyan") {
  # Use the {{}} "curly-curly" operator to handle tidy evaluation
  summary_data <- data %>%
    count({{ feature_col }}) %>%
    mutate(
      percentage = n / sum(n),
      label = scales::percent(percentage, accuracy = 0.1)
    )

  ggplot(summary_data, aes(x = {{ feature_col }}, y = n)) +
    geom_col(fill = bar_color) +
    geom_text(aes(label = label), vjust = -0.5, size = 4) +
    labs(
      title = title,
      subtitle = subtitle,
      x = xlab,
      y = "Count"
    ) +
    theme_minimal(base_size = 14) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
}

plot_intention_by_cat <- function(data, feature_col, title, subtitle, xlab) {
  # Use the {{}} "curly-curly" operator from rlang to handle tidy evaluation
  ggplot(data, aes(x = {{ feature_col }}, fill = FUTSUPNO)) +
    geom_bar(position = "fill") +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_manual(
      values = c("No" = "firebrick", "Yes" = "seagreen"),
      name = "Intends to Start?"
    ) +
    labs(
      title = title,
      subtitle = subtitle,
      x = xlab,
      y = "Proportion of Respondents"
    ) +
    theme_minimal(base_size = 14)
}

# --- Stage 2.0: Further Data Cleaning & Filtering Student Subpopulation ---

# Rationale: EDA identified observations with unusually large household sizes (hhsize > 20).
# These are presumed to be non-traditional households (e.g., dormitories, barracks) 
# and do not represent the target population for this analysis.
# Action: To prevent these high-leverage points from skewing the model, they are removed.
threshold <- 20
rows_to_remove <- sum(model_data$hhsize > threshold)
cat(
  "--- Stage 2.0: Found",
  rows_to_remove,
  "rows with hhsize >",
  threshold,
  "to be removed from global dataset. ---\n"
)
# Remove the outliers (affects ~0.023% of data)
model_data <- model_data %>%
  filter(hhsize <= threshold)

# filter for Target Subpopulation
# Methodological Consideration: Filter Timing
# Using the Larger Dataset for Imputation created more robust models for filling missing values.
# Also the following EDA was conducted on both Global and Student Subpopulation.
# However, this could result in Target-Leakage as the Imputed Values had access to non-student rows.
student_data <- model_data %>%
  filter(GEMOCCU == "Student") %>% 
  select(-any_of(c("GEMOCCU")))
cat("\n--- Stage 2.0: Filtered data for Student-Specific Analysis. Remaining rows:",
  nrow(student_data), " ---\n")

# ===================================================================
# --- SECTION 3: HELPER FUNCTIONS FOR EVALUATION & PLOTTING ---
# ===================================================================
# Objective: In the following Section, we will conduct several experiments,
# including training 3x different ML Algorithms with their own set of unique
# hyperparameters. In order to be able to cross compare, we will create a set
# of helper functions that calculate key Evaluation Metrics and Graphs.

plot_theme <- theme(
  panel.background = element_rect(fill = "grey15", color = NA),
  plot.background = element_rect(fill = "grey15", color = NA),
  panel.grid.major = element_line(color = "grey35", linewidth = 0.4),
  panel.grid.minor = element_line(color = "grey25", linewidth = 0.2),
  axis.line = element_line(color = "grey70", linewidth = 0.5),
  axis.ticks = element_line(color = "grey70"),
  axis.text = element_text(color = "grey85"),
  axis.title = element_text(color = "grey95"),
  plot.title = element_text(color = "white", face = "bold"),
  plot.subtitle = element_text(color = "grey85")
)


#' Evaluate and Report Performance on the Validation Set
#'
#' Takes a fitted workflow from the validation split and produces a comprehensive
#' summary, including plots and a detailed report.txt.
#'
#' @param validation_fit The output object from `last_fit()` on the validation split.
#' @param tune_results The output from `tune_grid()`.
#' @param best_params The data frame of best hyperparameters.
#' @param optimal_threshold The numeric decision threshold found from the validation set.
#' @param experiment_name A string name for the current experiment.
#' @param output_dir The directory where plots and reports are saved.
evaluate_and_report_validation <- function(validation_fit,
                                           tune_results,
                                           best_params,
                                           optimal_threshold,
                                           experiment_name,
                                           output_dir) {

  # --- 1. SETUP: PREPARE ALL METRICS AND CONFIG INFO ---
  core_metrics <- collect_metrics(validation_fit)
  predictions <- collect_predictions(validation_fit)
  
  predictions_reclassified <- predictions %>%
    mutate(.pred_class_optimal = factor(ifelse(.pred_Yes >= optimal_threshold, "Yes", "No"), levels = c("No", "Yes")))
  
  conf_matrix <- conf_mat(predictions_reclassified, truth = FUTSUPNO, estimate = .pred_class_optimal)

  tn <- conf_matrix$table[1, 1]; fn <- conf_matrix$table[1, 2]; fp <- conf_matrix$table[2, 1]; tp <- conf_matrix$table[2, 2]
  sensitivity <- tp / (tp + fn); specificity <- tn / (tn + fp); precision <- tp / (tp + fp); accuracy <- (tp + tn) / (tp + tn + fp + fn)

  recipe_summary <- extract_recipe(validation_fit) %>% summary()
  model_spec <- extract_spec_parsnip(validation_fit)
  model_type <- class(model_spec)[1]

  # --- 2. CONSOLE OUTPUT (VALIDATION SET) ---
  cat("===============================================================\n")
  cat("--- VALIDATION SET EVALUATION: ", experiment_name, " ---\n")
  cat("===============================================================\n\n")
  cat("--- Validation Set Performance Metrics ---\n")
  print(core_metrics)
  cat("\n")
  cat(paste0("--- Confusion Matrix (Validation Set) at Optimal Threshold: ", round(optimal_threshold, 3), " ---\n"))
  print(conf_matrix)
  cat("\n")
  cat("--- Key Performance Rates (Validation Set) ---\n")
  cat(sprintf("Sensitivity: %.3f  (How many of the true founders did we find?)\n", sensitivity))
  cat(sprintf("Specificity: %.3f  (How well did we avoid contacting non-founders?)\n", specificity))
  cat(sprintf("Precision  : %.3f  (When we predict 'founder', how often are we right?)\n", precision))
  cat(sprintf("Accuracy   : %.3f  (Overall, what proportion of students did we classify correctly?)\n\n", accuracy))

  # --- 3. PLOTTING (from Validation Set) ---
  roc_data <- predictions %>% roc_curve(FUTSUPNO, .pred_Yes, event_level = "second")
  
  roc_plot <- ggplot(roc_data, aes(x = 1 - specificity, y = sensitivity)) +
    geom_abline(linetype = "dashed", color = "grey70", linewidth = 0.8) +
    geom_line(color = "#00F2FF", linewidth = 1.2, alpha = 0.8) +
    labs(
      title = "ROC Curve (Validation Set)", 
      subtitle = paste("Validation Set AUC =", round(core_metrics$.estimate[core_metrics$.metric == "roc_auc"], 3)),
      x = "1 - Specificity",
      y = "Sensitivity"
    ) +
    coord_equal() + 
    plot_theme

  vip_plot <- validation_fit %>% 
    extract_fit_parsnip() %>% 
    vip(num_features = 20, geom = "col") +
    labs(title = "Top 20 Most Important Predictors") +
    theme_minimal() + 
    plot_theme

  print(roc_plot)
  print(vip_plot)

  ggsave(filename = file.path(output_dir, "roc_curve_validation.png"), plot = roc_plot, width = 8, height = 8)
  ggsave(filename = file.path(output_dir, "variable_importance_plot.png"), plot = vip_plot, width = 10, height = 7)
  cat(paste0("Plots saved to: ", output_dir, "\n"))

  # --- 4. REPORTING (from Validation Set) ---
  report_path <- file.path(output_dir, "report.txt")
  file_conn <- file(report_path, "w") # Create and write to the file
  
  write_line <- function(text, ...) { writeLines(sprintf(text, ...), file_conn) }
  
  write_line("===============================================================")
  write_line("                 EXPERIMENT REPORT: %s", experiment_name)
  write_line("===============================================================")
  write_line("Report generated on: %s", Sys.time())
  write_line("\n--- CONFIGURATION ---")
  write_line("Model Type: %s", model_type)
  write_line("Recipe Steps Included: %s", str_c(unique(recipe_summary$type), collapse = ", "))
  
  write_line("\n--- HYPERPARAMETER TUNING (on Training Set CV) ---")
  write_line("Tuning Metric: roc_auc")
  best_params_cleaned <- best_params %>% select(!starts_with("."))
  params_list <- purrr::map2_chr(names(best_params_cleaned), best_params_cleaned, ~ paste(.x, "=", if(is.numeric(.y)) round(.y, 4) else as.character(.y)))
  best_params_str <- paste(params_list, collapse = ", ")
  write_line("Best Hyperparameters Found: %s", best_params_str)
  
  write_line("\n--- VALIDATION SET PERFORMANCE ---")
  write_line("Optimal Decision Threshold Applied: %.4f", optimal_threshold)
  write_line("\n** Core Metrics (Threshold Independent) **")
  core_metrics_df <- data.frame(Metric = core_metrics$.metric, Estimate = round(core_metrics$.estimate, 4))
  writeLines(capture.output(print(core_metrics_df, row.names = FALSE)), file_conn)
  
  write_line("\n** Performance at Optimal Threshold (with use-case descriptions) **")
  write_line("Accuracy   : %.4f  (Overall, what proportion of students did we classify correctly?)", accuracy)
  write_line("Sensitivity: %.4f  (How many of the true founders did we find?)", sensitivity)
  write_line("Specificity: %.4f  (How well did we avoid contacting non-founders?)", specificity)
  write_line("Precision  : %.4f  (When we predict 'founder', how often are we right?)", precision)
  
  write_line("\n** Confusion Matrix (Validation Set) **")
  writeLines(capture.output(print(conf_matrix)), file_conn)
  
  close(file_conn)
  cat(paste0("Report saved to: ", report_path, "\n"))
}


#' Reports the final, unbiased performance on the held-out test set.
#'
#' This function appends the final test set results to an existing report.txt.
#'
#' @param test_predictions A data frame of predictions on the test set.
#' @param optimal_threshold The threshold determined from the validation set.
#' @param output_dir The directory where the report is saved.
report_final_test_performance <- function(test_predictions, optimal_threshold, output_dir) {
    
  # Calculate final metrics based on test predictions
  predictions_reclassified <- test_predictions %>%
    mutate(.pred_class_optimal = factor(ifelse(.pred_Yes >= optimal_threshold, "Yes", "No"), levels = c("No", "Yes")))
  
  conf_matrix <- conf_mat(predictions_reclassified, truth = FUTSUPNO, estimate = .pred_class_optimal)
  
  tn <- conf_matrix$table[1, 1]; fn <- conf_matrix$table[1, 2]; fp <- conf_matrix$table[2, 1]; tp <- conf_matrix$table[2, 2]
  sensitivity <- tp / (tp + fn); specificity <- tn / (tn + fp); precision <- tp / (tp + fp); accuracy <- (tp + tn) / (tp + tn + fp + fn)
  roc_auc <- roc_auc(test_predictions, truth = FUTSUPNO, .pred_Yes, event_level = "second")$.estimate
  
  # --- CONSOLE OUTPUT (TEST SET) ---
  cat("\n--- UNBIASED TEST SET PERFORMANCE ---\n")
  cat(paste0("--- Confusion Matrix (Test Set) at Threshold: ", round(optimal_threshold, 3), " ---\n"))
  print(conf_matrix)
  cat("\n--- Final Test Set Rates ---\n")
  cat(sprintf("Test Set AUC: %.4f\n", roc_auc))
  cat(sprintf("Sensitivity : %.3f\n", sensitivity))
  cat(sprintf("Specificity : %.3f\n", specificity))
  cat(sprintf("Precision   : %.3f\n", precision))
  cat(sprintf("Accuracy    : %.3f\n\n", accuracy))

  # --- APPEND TO REPORT ---
  report_path <- file.path(output_dir, "report.txt")
  file_conn <- file(report_path, "a") # Open in append mode
  
  write_line <- function(text, ...) { writeLines(sprintf(text, ...), file_conn) }
  
  write_line("\n\n===============================================================")
  write_line("--- FINAL UNBIASED PERFORMANCE ON HELD-OUT TEST SET ---")
  write_line("===============================================================")
  write_line("Threshold determined from validation set (%.4f) was applied.", optimal_threshold)
  write_line("\n** Final Test Set Metrics **")
  write_line("Test Set AUC : %.4f", roc_auc)
  write_line("Accuracy     : %.4f", accuracy)
  write_line("Sensitivity  : %.4f", sensitivity)
  write_line("Specificity  : %.4f", specificity)
  write_line("Precision    : %.4f", precision)
  write_line("\n** Final Test Set Confusion Matrix **")
  writeLines(capture.output(print(conf_matrix)), file_conn)
  
  close(file_conn)
  cat(paste0("Final test results appended to: ", report_path, "\n"))
}



#' Plot the Sensitivity vs. Specificity Tradeoff Curve
#'
#' This function takes prediction results, plots the tradeoff curve, and returns
#' the optimal crossover point.
#'
#' @param model_results The output from `last_fit()` or predictions data frame.
#' @param plot_title_suffix A string to append to the plot title (e.g., "(Validation Set)").
#' @return A data frame containing the optimal threshold.
plot_sensitivity_specificity_tradeoff <- function(model_results,
                                                  truth_col,
                                                  prob_col,
                                                  output_dir,
                                                  event_level = "second",
                                                  plot_title_suffix = "") {
  roc_data <- model_results %>%
    collect_predictions() %>%
    roc_curve(
      {{ truth_col }},
      {{ prob_col }},
      event_level = event_level
    )

  crossover_point <- roc_data %>%
    mutate(difference = abs(sensitivity - specificity)) %>%
    filter(difference == min(difference)) %>%
    arrange(desc(sensitivity)) %>%
    .[1, ]

  tradeoff_data <- roc_data %>%
    select(.threshold, sensitivity, specificity) %>%
    pivot_longer(
      cols = c(sensitivity, specificity),
      names_to = "metric",
      values_to = "value"
    )

  tradeoff_plot <- ggplot(tradeoff_data, aes(x = .threshold, y = value, color = metric)) +
    geom_line(linewidth = 1.2, alpha = 0.8) +
    geom_vline(xintercept = crossover_point$.threshold, linetype = "dotted", color = "grey70") +
    geom_point(data = crossover_point, aes(x = .threshold, y = sensitivity), size = 4, color = "white") +
    geom_point(data = crossover_point, aes(x = .threshold, y = sensitivity), size = 2.5, color = "darkcyan") +
    geom_text(data = crossover_point, aes(x = .threshold, y = sensitivity, label = paste0("Optimal Threshold: ", round(.threshold, 2), "\nSens = ", round(sensitivity, 2), "\nSpec = ", round(specificity, 2))),
              hjust = -0.1, vjust = 0.5, nudge_x = 0.02, color = "white", size = 3.5) +
    geom_vline(xintercept = 0.5, linetype = "dashed", color = "firebrick") +
    annotate("text", x = 0.5, y = 0.1, label = "Default 0.5 Threshold", color = "firebrick", angle = 90, vjust = -0.5, size = 3.5) +
    scale_color_manual(values = c("sensitivity" = "darkcyan", "specificity" = "darkorange")) +
    labs(
      title = paste("Sensitivity-Specificity Tradeoff", plot_title_suffix),
      subtitle = "Shows model performance across all possible decision points",
      x = "Decision Threshold", y = "Rate", color = "Metric"
    ) +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
    theme_minimal() + plot_theme

  # Create a unique filename for the plot
  plot_filename <- paste0("sensitivity_specificity_tradeoff.png")
  
  print(tradeoff_plot)
  ggsave(
    filename = file.path(output_dir, plot_filename),
    plot = tradeoff_plot, width = 9, height = 6, dpi = 300
  )
  
  return(crossover_point)
}

# ===================================================================
# --- SECTION 4: MODELING EXPERIMENTS ---
# ===================================================================

# --- Stage 4.0: Preprocessing for Real World Use --- 
# ctryalp: Experiments showed: MANY categories = much noise. Most categories have very low feature importance
# WBINC can be used as PROXY
student_data <- student_data %>% select(-any_of(c("ctryalp")))

set.seed(42)


# --- Stage 4.1: DATA SPLITTING & RESAMPLING ---
data_split <- initial_split(student_data, prop = 0.80, strata = FUTSUPNO)
test_data  <- testing(data_split)
train_val_data <- training(data_split)
val_split <- initial_split(train_val_data, prop = 0.80, strata = FUTSUPNO)
train_data <- training(val_split)
validation_data <- testing(val_split)

cat(sprintf("Data Split Summary:\n - Training Set: %d rows\n - Validation Set: %d rows\n - Test Set: %d rows\n",
            nrow(train_data), nrow(validation_data), nrow(test_data)))

cv_folds <- vfold_cv(train_data, v = 5, strata = FUTSUPNO)

# --- Stage 4.2: EXPERIMENT CONFIGS ---

# --- For XGBoost (scale_pos_weight) ---
count_no <- sum(train_val_data$FUTSUPNO == "No")
count_yes <- sum(train_val_data$FUTSUPNO == "Yes")
xgb_weight_value <- count_no / count_yes
cat(sprintf("Calculated scale_pos_weight for XGBoost: %.2f\n", xgb_weight_value))

# --- For Random Forest (class.weights) ---
# Calculate weights as the inverse of class frequencies, then normalize
rf_class_weights <- c(
  "No" = (1 / count_no) * (count_no + count_yes) / 2,
  "Yes" = (1 / count_yes) * (count_no + count_yes) / 2
)
cat(sprintf("Calculated class weights for Random Forest: No=%.2f, Yes=%.2f\n", 
            rf_class_weights["No"], rf_class_weights["Yes"]))


# --- Create the comprehensive list of all experiment configurations ---
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


set.seed(42)
bayes_ctrl <- control_bayes(save_pred = TRUE, verbose = FALSE, seed = 42)
metric_set_sens_spec <- metric_set(sens, spec, roc_auc)

cores_to_use <- parallel::detectCores() - 1
registerDoFuture()
plan(multisession, workers = cores_to_use)


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
    initial = 10,                  # Startet mit 10 zufälligen Kombinationen
    iter = 30,                     # Führt 30 intelligente Iterationen durch
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




# ===================================================================
# --- SECTION 5: Final Model Training & Evaluation ---
# ===================================================================
# Objective: We will take the best-performing model identified from the
# experiments, retrain it on the complete training and validation dataset, and
# then perform a final evaluation on the held-out test set. This provides a
# definitive assessment of the model's generalization performance.

# --- Helper Function for Final Test Set Evaluation ---

#' Evaluate, Plot, and Report Final Performance on the Test Set
#'
#' Takes the final fitted model, evaluates it on the test set, generates
#' plots, and appends the final results to the experiment's report.
#'
#' @param final_fit A model workflow object fitted on the full train+val data.
#' @param test_data The held-out test dataset.
#' @param optimal_threshold The numeric decision threshold from the validation run.
#' @param experiment_name The string name of the winning experiment.
#' @param output_dir The directory where final plots and reports are saved.
evaluate_and_report_test <- function(final_fit,
                                     test_data,
                                     optimal_threshold,
                                     experiment_name,
                                     output_dir) {

  # --- 1. GET PREDICTIONS ON THE TEST SET ---
  test_predictions <- predict(final_fit, new_data = test_data, type = "prob") %>%
    bind_cols(test_data %>% select(FUTSUPNO)) # Add truth column

  # --- 2. CALCULATE METRICS AT OPTIMAL THRESHOLD ---
  predictions_reclassified <- test_predictions %>%
    mutate(.pred_class_optimal = factor(ifelse(.pred_Yes >= optimal_threshold, "Yes", "No"), levels = c("No", "Yes")))

  conf_matrix <- conf_mat(predictions_reclassified, truth = FUTSUPNO, estimate = .pred_class_optimal)

  # Extract values from confusion matrix
  tn <- conf_matrix$table[1, 1]; fn <- conf_matrix$table[1, 2]
  fp <- conf_matrix$table[2, 1]; tp <- conf_matrix$table[2, 2]

  # Calculate key metrics
  sensitivity <- tp / (tp + fn)
  specificity <- tn / (tn + fp)
  precision <- tp / (tp + fp)
  accuracy <- (tp + tn) / (tp + tn + fp + fn)
  roc_auc_val <- roc_auc(test_predictions, truth = FUTSUPNO, .pred_Yes, event_level = "second")$.estimate

  # --- 3. GENERATE AND SAVE FINAL PLOTS (from Test Set) ---
  roc_data <- test_predictions %>% roc_curve(FUTSUPNO, .pred_Yes, event_level = "second")

  # Final ROC Curve
  roc_plot_test <- ggplot(roc_data, aes(x = 1 - specificity, y = sensitivity)) +
    geom_abline(linetype = "dashed", color = "grey70", linewidth = 0.8) +
    geom_line(color = "#00F2FF", linewidth = 1.2, alpha = 0.8) +
    labs(
      title = "Final ROC Curve (Held-Out Test Set)",
      subtitle = paste("Test Set AUC =", round(roc_auc_val, 3)),
      x = "1 - Specificity",
      y = "Sensitivity"
    ) +
    coord_equal() +
    plot_theme

  # Final Variable Importance Plot (from model trained on train+val)
  vip_plot_final <- final_fit %>%
    extract_fit_parsnip() %>%
    vip(num_features = 20, geom = "col") +
    labs(title = "Final Top 20 Predictors (Model Trained on All Data)") +
    theme_minimal() +
    plot_theme

  print(roc_plot_test)
  print(vip_plot_final)

  # Save plots to the best model's output directory
  ggsave(filename = file.path(output_dir, "roc_curve_FINAL_TEST.png"), plot = roc_plot_test, width = 8, height = 8)
  ggsave(filename = file.path(output_dir, "variable_importance_plot_FINAL.png"), plot = vip_plot_final, width = 10, height = 7)
  cat(paste0("Final plots saved to: ", output_dir, "\n"))


  # --- 4. APPEND FINAL REPORT (using existing helper function) ---
  # This function handles console output and appending to report.txt
  report_final_test_performance(
      test_predictions = test_predictions,
      optimal_threshold = optimal_threshold,
      output_dir = output_dir
  )
}


# ===================================================================
# --- Stage 5.1: RETRAIN AND EVALUATE THE WINNING MODEL ---
# ===================================================================
cat("\n\n===============================================================\n")
cat("--- FINAL RETRAINING & EVALUATION ON TEST SET ---\n")
cat("===============================================================\n")

# --- Identify the best performing experiment configuration ---
# Based on the analysis, this is 'Experiment_6_RF_ManualWeights'.
# It achieved the highest ROC_AUC with 0.7879 followed closely by Logistic Regression.
# Despite having poorer Interpretability, we still chose the RandomForest as we are
# interested in uncovering the complex psychological patterns that drive
# Entrepreneurship. A logistic Regression is inherently limited to finding linear
# patterns. We combat the issue of Interpretability using aggregated SHAP plots later.
best_experiment_name <- "Experiment_6_RF_ManualWeights"
best_config <- Find(function(x) x$name == best_experiment_name, experiment_configs)
best_experiment_output_dir <- file.path(output_base_dir, "Final_Evaluation")
if (!dir.exists(best_experiment_output_dir)) {
    dir.create(best_experiment_output_dir, recursive = TRUE)
  }

# --- Re-create the workflow for the best model ---
best_workflow <- workflow() %>%
  add_recipe(best_config$recipe_func(train_val_data)) %>%
  add_model(best_config$model_spec)

# --- Re-run a quick tuning process on the FULL train+val data to find the absolute best parameters ---
# This is more robust than using parameters found on a smaller subset.
cat("\n--- Step 1: Re-tuning best model on the complete train+val dataset ---\n")
set.seed(42)
final_cv_folds <- vfold_cv(train_val_data, v = 10, strata = FUTSUPNO) # 10 folds for robustness

# === RE-ENABLE MULTIPROCESSING FOR THE FINAL TUNE ===
cat("Setting up parallel processing...\n")
registerDoFuture()
plan(multisession, workers = cores_to_use) # 'cores_to_use' was defined in Section 4

final_tune_results <- tune_bayes(
  best_workflow,
  resamples = final_cv_folds,
  param_info = best_config$param_grid,
  initial = 10,
  iter = 30,
  metrics = metric_set(roc_auc),
  control = control_bayes(save_pred = TRUE, verbose = FALSE, seed = 42)
)

# === SHUT DOWN THE PARALLEL BACKEND ===
plan(sequential)
cat("Parallel processing shut down.\n")

best_final_params <- select_best(final_tune_results, metric = "roc_auc")
cat("\nBest hyperparameters found for the final model:\n")
print(best_final_params)

final_workflow <- finalize_workflow(best_workflow, best_final_params)

# --- Step 2: Determine the Optimal Threshold from the ORIGINAL Validation Set ---
# We must use the threshold found from the validation set during the experiment
# phase to avoid data leakage from the test set.
cat("\n--- Step 2: Recalculating optimal threshold from the original validation run ---\n")
set.seed(42)
# Re-run `last_fit` on the original train/validation split to get validation predictions
validation_run_fit <- last_fit(final_workflow, val_split)

# Use the plotting function to find the crossover point on the validation set
optimal_point_final <- plot_sensitivity_specificity_tradeoff(
  model_results = validation_run_fit,
  truth_col = FUTSUPNO,
  prob_col = .pred_Yes,
  output_dir = best_experiment_output_dir,
  plot_title_suffix = "(Recalculated from Validation Set for Final Model)"
)
final_optimal_threshold <- optimal_point_final$.threshold
cat(sprintf("\nOptimal Threshold determined from validation set: %.4f\n", final_optimal_threshold))


# --- Step 3: Train the final model on the ENTIRE train+val dataset ---
cat("\n--- Step 3: Fitting the final model on all train+val data ---\n")
set.seed(42)
final_model_fit <- fit(final_workflow, data = train_val_data)
cat("Final model training complete.\n")

# --- Step 4: Perform final evaluation on the held-out test set ---
cat("\n--- Step 4: Evaluating final model on the unseen test set ---\n")
evaluate_and_report_test(
  final_fit = final_model_fit,
  test_data = test_data,
  optimal_threshold = final_optimal_threshold,
  experiment_name = best_experiment_name,
  output_dir = best_experiment_output_dir
)

cat("\n\n--- FINAL ANALYSIS COMPLETE ---\n")



# ===================================================================
# --- SECTION 6: MODEL EXPLAINABILITY WITH SHAP PLOTS ---
# ===================================================================

# --- Step 1: Create a DALEX explainer object ---
# This remains the same.
cat("\n--- Creating DALEX explainer for the final model ---\n")

explainer_rf <- explain(
  model = final_model_fit,
  data = train_val_data %>% select(-FUTSUPNO),
  y = as.numeric(train_val_data$FUTSUPNO == "Yes"),
  label = "Random Forest Final",
  predict_function = function(model, newdata) {
    predict(model, new_data = newdata, type = "prob")$.pred_Yes
  }
)
cat("Explainer created successfully.\n")


# --- Step 2: Create a Unified Pool of Students for Analysis ---
# To improve efficiency, we will create ONE pool of students that contains
# only correctly classified (TP & TN) individuals with complete data.
# Both the single-student plot and the aggregate plot will use this pool.
cat("\n\n--- Creating a unified analysis pool of correctly classified students with complete data ---\n")

# Get predictions for the test set to identify TP & TN
test_set_predictions <- predict(final_model_fit, new_data = test_data, type = "prob") %>%
  bind_cols(test_data %>% select(FUTSUPNO)) %>%
  mutate(student_id = row_number())

# Classify predictions
classified_predictions <- test_set_predictions %>%
  mutate(
    .pred_class_optimal = factor(ifelse(.pred_Yes >= final_optimal_threshold, "Yes", "No"), levels = c("No", "Yes")),
    classification_type = case_when(
      FUTSUPNO == "Yes" & .pred_class_optimal == "Yes" ~ "True Positive",
      FUTSUPNO == "No"  & .pred_class_optimal == "No"  ~ "True Negative",
      TRUE ~ "Incorrectly Classified"
    )
  )

# Join classifications back to the original data
test_data_classified <- test_data %>%
  mutate(student_id = row_number()) %>%
  left_join(classified_predictions %>% select(student_id, classification_type), by = "student_id")

# --- Create the FINAL ANALYSIS POOL ---
unwanted_values <- c("Unknown", "Refused", "Refused_Answer", "Not_Applicable")
final_analysis_pool <- test_data_classified %>%
  filter(
    # Keep only correctly classified students
    classification_type %in% c("True Positive", "True Negative"),
    # AND ensure they have complete data across all relevant columns
    across(
      .cols = where(is.factor) | where(is.character),
      .fns = ~ !(.x %in% unwanted_values)
    )
  )

cat("Prediction outcomes on the test set:\n")
print(table(classified_predictions$classification_type))
cat(sprintf("\nCreated final analysis pool with %d correctly classified students who have complete data.\n", nrow(final_analysis_pool)))


# --- Step 3: Generate All SHAP Plots from the Unified Pool ---
# We will now generate both plots. This entire step is skipped if the pool is empty.

if (nrow(final_analysis_pool) > 0) {
  
  # --- 3a. Single Student Waterfall Plot ---
  cat("\n\n--- Generating SHAP Waterfall plot for a single student from the analysis pool ---\n")
  set.seed(42) # for reproducible choice
  student_for_analysis <- final_analysis_pool %>% sample_n(1)
  
  cat(sprintf("Selected one student (Classification: %s) for the waterfall plot.\n", student_for_analysis$classification_type))

  shap_explanation_single <- predict_parts(
    explainer = explainer_rf,
    new_observation = student_for_analysis,
    type = "shap"
  )

  custom_colors <- c("-1" = "#B22222", "1" = "#2E8B57") # firebrick and seagreen
  shap_waterfall_plot <- plot(shap_explanation_single, max_features = 15, show_boxplots = FALSE) +
    ggplot2::scale_fill_manual(values = custom_colors, guide = "none") +
    labs(
      title = "SHAP Explanation for a Correctly Classified Student",
      subtitle = "How each feature pushed the prediction from the baseline average"
    ) +
    plot_theme
    
  output_filename_single_shap <- file.path(best_experiment_output_dir, "SHAP_Waterfall_Single_Correct_Student.png")
  ggsave(filename = output_filename_single_shap, plot = shap_waterfall_plot, width = 12, height = 8, dpi = 300)
  cat(sprintf("SHAP waterfall plot saved to: %s\n", output_filename_single_shap))

  # --- 3b. Aggregate Summary Plot ---
  cat("\n\n--- Generating Aggregate SHAP Plot for a sample from the analysis pool ---\n")
  sample_size <- min(200, nrow(final_analysis_pool))
  set.seed(123) # Use a different seed for the aggregate sample
  analysis_sample <- final_analysis_pool %>% sample_n(sample_size)
  
  cat(sprintf("Analyzing a random sample of %d students from the pool.\n", sample_size))
  
  # Setup and run parallel processing
  cat("Calculating aggregate SHAP values... (this may take a moment)\n")
  registerDoFuture()
  plan(multisession, workers = cores_to_use)
  
  shap_explanation_combined <- predict_parts(explainer = explainer_rf, new_observation = analysis_sample, type = "shap")
  
  plan(sequential)
  cat("Calculation complete.\n")

  # Generate and save the aggregate plot
  shap_combined_plot <- plot(shap_explanation_combined, max_features = 20) +
  ggplot2::scale_fill_manual(values = custom_colors, guide = "none") +
    labs(
      title = "Aggregate SHAP Plot for Correctly Classified Students (TP & TN)",
      subtitle = paste("Top features driving correct predictions (Sample Size:", sample_size, ")")
    ) +
    plot_theme
  
  output_filename_shap_combined <- file.path(best_experiment_output_dir, "SHAP_Aggregate_Correct_Predictions.png")
  ggsave(filename = output_filename_shap_combined, plot = shap_combined_plot, width = 12, height = 10, dpi = 300)
  cat(sprintf("Combined SHAP plot saved to: %s\n", output_filename_shap_combined))
  
} else {
  cat("\nWARNING: The final analysis pool is empty. No SHAP plots can be generated.\n")
}

cat("\n\n--- SHAP ANALYSIS COMPLETE ---\n")