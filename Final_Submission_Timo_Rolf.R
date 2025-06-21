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
  "missForest"        # For MissForest Imputation    
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

if (file.exists("output_final/cleaned_model_data.rds")) {
  model_data <- readRDS("output_final/cleaned_model_data.rds")

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
      # Methodology:
      # 1. Create a binary flag 'Mindset_Asked' to permanently label rows based
      #    on the reason for missingness (split-sample vs. partial non-response).
      #    This captures the survey's design.
      # 2. Impute the remaining NA values in the mindset columns. We will turn the
      #    NAs into an explicit factor level 'Not_Answered' using the modern forcats function.
      Mindset_Asked = as.factor(
        ifelse(
          rowSums(is.na(select(., all_of(mindset_cols)))) == length(mindset_cols),
          "Not_Asked",
          "Asked"
        )
      ),

      # Step 2: Impute the NA values
      across(
        all_of(mindset_cols),
        ~ fct_na_value_to_level(., level = "Not_Answered")
      ),

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


  cat("\n--- Stage 1.11: Handled high-missingness categorical columns by creating 'Unknown' level. ---\n")
  cat("\n--- Stage 1.11: Engineering features from missingness patterns: Created 'Mindset_Asked' and 'age_is_missing' flags. ---\n")


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
  missForest_result <- missForest(
    model_data,
    parallelize = "forests",
    ntree = 75,
    maxiter = 5,
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

  model_data_path <- "output_final/cleaned_model_data.rds"
  dir.create(dirname(model_data_path), showWarnings = FALSE, recursive = TRUE)
  saveRDS(model_data, file = model_data_path)

  cat("Cleaned data successfully saved to:", model_data_path, "\n")
}


# --- Stage 1.13: Relabel Categories ---
# Objective: Rename the numeric codes to human readable labels.
# e.g.: 1-5 -> "Strongly Disagree"-"Strongly Agree"

# Helper function for 5-point Likert scales
# This avoids repeating the same code for multiple variables.
relabel_likert <- function(variable) {
  # Define the levels for the ordered factor
  likert_levels <- c(
    "Strongly Disagree",
    "Somewhat Disagree",
    "Neither",
    "Somewhat Agree",
    "Strongly Agree",
    "Not Answered"
  )

  factor(
    case_when(
      variable == "1" ~ "Strongly Disagree",
      variable == "2" ~ "Somewhat Disagree",
      variable == "3" ~ "Neither",
      variable == "4" ~ "Somewhat Agree",
      variable == "5" ~ "Strongly Agree",
      TRUE ~ "Not Answered"
    ),
    levels = likert_levels,
    ordered = TRUE
  )
}


# Relabeling the data by overwriting original columns 
model_data <- model_data %>%
  mutate(
    # WBINC: World Bank Income Level (Codebook, p. 3)
    WBINC = factor(
      WBINC,
      levels = 1:4,
      labels = c("Low", "Lower Middle", "Upper Middle", "High")
    ),

    # gender: Gender of respondent (Codebook, p. 3)
    gender = factor(gender, levels = 1:2, labels = c("Male", "Female")),

    # GEMOCCU: GEM harmonized work status (Codebook, p. 3)
    GEMOCCU = factor(
      GEMOCCU,
      levels = 1:7,
      labels = c(
        "Full/Part-time Employee",
        "Part-time Employee Only",
        "Retired/Disabled",
        "Homemaker",
        "Student",
        "Not Working",
        "Self-employed"
      )
    ),

    # GEMHHINC: GEM harmonized household income thirds (Codebook, p. 3)
    GEMHHINC = factor(
      case_when(
        GEMHHINC == "33" ~ "Lowest Third",
        GEMHHINC == "3467" ~ "Middle Third",
        GEMHHINC == "68100" ~ "Upper Third",
        TRUE ~ "Unknown"
      ),
      levels = c("Lowest Third", "Middle Third", "Upper Third", "Unknown")
    ),

    # GEMEDUC: GEM harmonized educational attainment (Codebook, p. 3)
    GEMEDUC = factor(
      case_when(
        GEMEDUC == 0 ~ "None",
        GEMEDUC == 111 ~ "Some Secondary",
        GEMEDUC == 1212 ~ "Secondary Degree",
        GEMEDUC == 1316 ~ "Post-Secondary",
        GEMEDUC == 1720 ~ "Graduate Experience",
        TRUE ~ "Other/Unknown"
      ),
      levels = c(
        "None",
        "Some Secondary",
        "Secondary Degree",
        "Post-Secondary",
        "Graduate Experience",
        "Other/Unknown"
      )
    ),

    # cphhinc: Change in household income due to COVID-19 (Codebook, p. 8)
    cphhinc = factor(
      cphhinc,
      levels = 1:5,
      labels = c(
        "Strongly Decrease",
        "Somewhat Decrease",
        "No Change",
        "Somewhat Increase",
        "Strongly Increase"
      ),
      ordered = TRUE
    ),

    # KNOWENyy: Knows someone who started a business in past 2 years (Codebook, p. 8)
    KNOWENyy = factor(
      KNOWENyy,
      levels = 0:1,
      labels = c("None", "At least one")
    ),

    # OPPORTyy: Sees good opportunities for starting a business (Codebook, p. 8)
    OPPORTyy = factor(
      case_when(OPPORTyy == 0 ~ "Disagree", OPPORTyy == 1 ~ "Agree", TRUE ~ "Unknown"),
      levels = c("Disagree", "Agree", "Unknown")
    ),

    # SUSKILyy: Has knowledge/skill/experience to start a business (Codebook, p. 9)
    SUSKILyy = factor(
      case_when(SUSKILyy == 0 ~ "Disagree", SUSKILyy == 1 ~ "Agree", TRUE ~ "Unknown"),
      levels = c("Disagree", "Agree", "Unknown")
    ),

    # FRFAILyy: Fear of failure would prevent starting a business (Codebook, p. 9)
    FRFAILyy = factor(FRFAILyy, levels = 0:1, labels = c("Disagree", "Agree")),

    # EASYSTyy: Believes it is easy to start a business in their country (Codebook, p. 9)
    EASYSTyy = factor(
      case_when(
        EASYSTyy == 0 ~ "Disagree",
        EASYSTyy == 1 ~ "Agree",
        EASYSTyy == 9 ~ "Refused",
        TRUE ~ "Unknown"
      ),
      levels = c("Disagree", "Agree", "Refused", "Unknown")
    ),

    # --- Applying the Likert scale relabeling function ---
    OPPISMyy = relabel_likert(OPPISMyy),
    PROACTyy = relabel_likert(PROACTyy),
    CREATIVyy = relabel_likert(CREATIVyy),
    VISIONyy = relabel_likert(VISIONyy),

    # INDSUPyy: Individual perception to entrepreneurship Index (Codebook, p. 9)
    INDSUPyy = factor(
      case_when(
        INDSUPyy == 0 ~ "0 of 3",
        INDSUPyy == 1 ~ "1 of 3",
        INDSUPyy == 2 ~ "2 of 3",
        INDSUPyy == 3 ~ "3 of 3",
        TRUE ~ "Unknown"
      ),
      levels = c("0 of 3", "1 of 3", "2 of 3", "3 of 3", "Unknown"),
      ordered = TRUE
    ),

    # FUTSUPNO: Entrepreneurial intentions (non-entrepreneurs) (Codebook, p. 13)
    FUTSUPNO = factor(FUTSUPNO, levels = 0:1, labels = c("No", "Yes")),
  )

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
student_data <- model_data %>%
  filter(GEMOCCU == "Student") %>% 
  select(-any_of(c("GEMOCCU")))
cat("\n--- Stage 2.0: Filtered data for Student-Specific Analysis. Remaining rows:",
  nrow(student_data), " ---\n")

# --- Stage 2.1: Target Variable Analysis ---
# Objective: Understand the distribution of entrepreneurial intention among students.


# GLOBAL
target_summary <- model_data %>%
  count(FUTSUPNO) %>%
  mutate(
    percentage = n / sum(n),
    label = paste0(round(percentage * 100, 1), "%")
  )

# Plot the distribution
ggplot(target_summary, aes(x = FUTSUPNO, y = n, fill = FUTSUPNO)) +
  geom_col() +
  geom_text(aes(label = label), vjust = -0.5, size = 5) +
  labs(
    title = "Distribution of Entrepreneurial Intentions (Global)",
    subtitle = "Target variable (FUTSUPNO) shows significant class imbalance.",
    x = "Intends to Start a Business in Next 3 Years?",
    y = "Number of Respondents"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("No" = "firebrick", "Yes" = "seagreen"))

# STUDENTS ONLY
# Calculate frequency and percentage for plot labels
student_target_summary <- student_data %>%
  count(FUTSUPNO) %>%
  mutate(
    percentage = n / sum(n),
    label = paste0(round(percentage * 100, 1), "%")
  )

# Plot the distribution
ggplot(student_target_summary, aes(x = FUTSUPNO, y = n, fill = FUTSUPNO)) +
  geom_col() +
  geom_text(aes(label = label), vjust = -0.5, size = 5) +
  labs(
    title = "Distribution of Entrepreneurial Intentions (Students Only)",
    subtitle = "The class imbalance is less severe compared to the general population.",
    x = "Intends to Start a Business in Next 3 Years?",
    y = "Number of Student Respondents"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("No" = "firebrick", "Yes" = "seagreen"))

# --- Student-Specific Analysis ---
# Among students, the 'Yes' class is 19.3% and the 'No' class is 80.7%.
#
# --- Comparison to Global Data ---
# The intention rate for students is really similar to the general population (19.4%).
# Students are a high-potential group for entrepreneurial initiatives as they usually
# have less obligations then the rest of the population. This validates the need
# for more specific support by DIT StartUp Campus.


# --- Stage 2.2: Univariate Analysis - Numeric Features ---
# Objective: Understand the distribution of individual numeric predictors.

# GLOBAL
plot_age_hist <- ggplot(model_data, aes(x = age)) +
  geom_histogram(binwidth = 2, fill = "steelblue", color = "white") +
  labs(title = "Distribution of Respondent Age (18-64) (Global)", y = "Count") +
  theme_minimal(base_size = 14)

plot_age_box <- ggplot(model_data, aes(y = age)) +
  geom_boxplot(fill = "steelblue") +
  labs(title = "Boxplot of Age") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

# Display plots side-by-side
plot_age_hist + plot_age_box

# STUDENTS ONLY
plot_age_hist_student <- ggplot(student_data, aes(x = age)) +
  geom_histogram(binwidth = 1, fill = "darkorange", color = "white") +
  labs(title = "Distribution of Respondent Age (Students Only)", y = "Count") +
  theme_minimal(base_size = 14)

plot_age_box_student <- ggplot(student_data, aes(y = age)) +
  geom_boxplot(fill = "darkorange") +
  labs(title = "Boxplot of Age (Students)") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

plot_age_hist_student + plot_age_box_student

# --- Student-Specific Analysis ---
# The age distribution for students is heavily left-skewed, with the vast
# majority concentrated between 18 and 25 years old.
#
# --- Comparison to Global Data ---
# This is drastically different from the general population's distribution,
# which was centered around 30-50. This validates our filter and highlights
# the youth of our target demographic for the DIT use case.



# GLOBAL
ggplot(model_data, aes(x = hhsize)) +
  geom_histogram(binwidth = 1, fill = "darkgreen", color = "white") +
  labs(
    title = "Distribution of Household Size (Global)",
    x = "Number of People in Household",
    y = "Count"
  ) +
  theme_minimal(base_size = 14)

# STUDENTS ONLY
ggplot(student_data, aes(x = hhsize)) +
  geom_histogram(binwidth = 1, fill = "darkgreen", color = "white") +
  labs(
    title = "Distribution of Household Size (Students Only)",
    x = "Number of People in Household",
    y = "Count"
  ) +
  theme_minimal(base_size = 14)

# --- Student-Specific Analysis ---
# The household size for students is also right-skewed, but the peak appears
# to be slightly lower (around 3-5 members) than the general population.
#
# --- Comparison to Global Data ---
# This could reflect students often living in small student apartments instead of at home.
# This is a minor difference but adds context to their living situations.


# --- Stage 2.3: Univariate Analysis - Key Categorical Features ---
# Objective: Understand the distribution of key categorical predictors.

# --- Occupation Status (GEMOCCU) - Global Only ---
# Note: This is not plotted for students as it's their defining characteristic.
# The original plot reordered factors for emphasis. A standard call is used here for consistency.
plot_univariate_dist(
    data = model_data,
    feature_col = GEMOCCU,
    title = "Distribution of Occupation Status (Global)",
    xlab = "Occupation",
    bar_color = "purple"
) + coord_flip() # Flipping coordinates for readability of labels


# --- Perceived Skills (SUSKILyy) ---
# GLOBAL
plot_univariate_dist(
  data = model_data,
  feature_col = SUSKILyy,
  title = "Distribution of Perceived Skills (Global)",
  xlab = "Perceives Having Skills to Start a Business?",
  bar_color = "coral"
)

plot_univariate_dist(
  data = student_data,
  feature_col = SUSKILyy,
  title = "Distribution of Perceived Skills (Students Only)",
  xlab = "Perceives Having Skills to Start a Business?",
  bar_color = "coral"
)

# --- Student-Specific Analysis ---
# Among students, 63.0% 'Disagree' and 37.0% 'Agree' that they have the
# necessary skills.
#
# --- Comparison to Global Data ---
# Students show a drastic smaller level of confidence compared to the general population.
# Hypothesis: The Center's marketing should heavily emphasize their practical, skills-based workshops
# (e.g., coding bootcamps, marketing seminars, finance 101). This directly addresses the most significant lever
# we can pull to encourage entrepreneurship.
# -> This needs to be confirmed after modeling is complete.

# --- Fear of Failure (FRFAILyy) ---
# GLOBAL
plot_univariate_dist(
  data = model_data,
  feature_col = FRFAILyy,
  title = "Distribution of Fear of Failure (Global)",
  xlab = "Would not start a business for fear it might fail",
  bar_color = "steelblue"
)

plot_univariate_dist(
  data = student_data,
  feature_col = FRFAILyy,
  title = "Distribution of Fear of Failure (Students Only)",
  xlab = "Would not start a business for fear it might fail",
  bar_color = "steelblue"
)

# --- Student-Specific Analysis ---
# For students, the split is 49.7% 'Disagree' and 50.3% 'Agree'.
#
# --- Comparison to Global Data ---
# This is fairly similar to the general population.
# Hypothesis: The DIT StartUp Center should host workshops specifically on 'Overcoming Founder Anxiety'
# or feature successful alumni who can talk about their own early failures."




# --- Stage 2.4: Bivariate Analysis ---

# --- Economic & Geographic Context ---
plot_intention_by_cat(
  model_data,
  WBINC,
  title = "Proportion of Intentions by World Bank Income Level",
  subtitle = "Entrepreneurial intention is highest in low-income countries and decreases as national income rises.",
  xlab = "World Bank Income Level"
)

plot_intention_by_cat(
  student_data,
  WBINC,
  title = "Intentions by World Bank Income Level (Students Only)",
  subtitle = "Does a student's background influence their entrepreneurial intention?",
  xlab = "World Bank Income Level of Home Country"
)
# --- Student-Specific Analysis ---
# The trend of higher intention among students from lower-income countries
# is even more pronounced than in the general population.
#
# --- Comparison to Global Data ---
# This strongly suggests that "necessity-driven" entrepreneurship is a
# powerful motivator for international students from developing economies.
# DIT can use this to tailor their messaging, offering entrepreneurship as a
# viable and empowering career path.

# --- Change in Household Income due to Corona Virus ---
plot_intention_by_cat(
  model_data,
  cphhinc,
  title = "Proportion of Intentions by Change in Household Income due to Corona Virus (Global)",
  subtitle = "A strong change (both positive & negative) strongly correlates with Intention.",
  xlab = "Change In Household Income due to Corona Virus"
)

plot_intention_by_cat(
  student_data,
  cphhinc,
  title = "Intentions by Change in Household Income (Students Only)",
  subtitle = "How does income shock due to COVID-19 affect student intentions?",
  xlab = "Change In Household Income due to Corona Virus"
)
# --- Student-Specific Analysis ---
# The pattern of high intention at the extremes (Strongly Decrease/Increase)
# holds for students.
#
# --- Comparison to Global Data ---
# The effect seems just as strong. This indicates that economic shocks,
# whether positive (creating opportunity) or negative (creating necessity),
# are a powerful catalyst for entrepreneurial thought among students.


# --- WORLD HEATMAP ---
# # Objective: Visualize the geographic distribution of entrepreneurial intention.

# We group by country and use weighted.mean() with the WEIGHT_L variable.
# This adjusts the sample to be representative of the 18-64 population in each country.
country_intention_weighted <- gem_data %>%
  # Ensure the weight column is numeric, handling potential NAs
  filter(!is.na(WEIGHT_L)) %>%
  group_by(ctryalp) %>%
  summarise(
    # Calculate the WEIGHTED proportion using the correct weight variable
    proportion_yes = weighted.mean(FUTSUPNO == 1.0, w = WEIGHT_L, na.rm = TRUE),
    n_respondents = n()
  ) %>%
  ungroup()

# --- 5c. Get World Map Spatial Data ---
world_map_sf <- ne_countries(scale = "medium", returnclass = "sf")

# Join our weighted summary data with the map's spatial data by country code.
world_map_with_data <- world_map_sf %>%
  left_join(country_intention_weighted, by = c("iso_a2" = "ctryalp"))

ggplot(data = world_map_with_data) +
  geom_sf(aes(fill = proportion_yes)) +
  scale_fill_viridis_c(
    option = "plasma",
    labels = scales::percent,
    na.value = "grey80",
    name = "Intention to Start\n(Proportion 'Yes')"
  ) +
  labs(
    title = "Global Distribution of Entrepreneurial Intention (Weighted)",
    subtitle = "Estimated proportion of the 18-64 population intending to start a business in the next 3 years.",
    caption = "Source: GEM 2021 APS. Weights (WEIGHT_L) applied to make samples nationally representative."
  ) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    plot.caption = element_text(hjust = 0.5, size = 9),
    legend.position = "bottom",
    legend.key.width = unit(2, "cm")
  )
# --- Analysis ---
# For Low-Income Nations, this is a clear sign of "necessety-driven" entrepreneurship.
# In High-Income Nations the Opportunity Cost is higher. People have stable jobs & good salaries.
# -> Consideration: Drop ctryalp in favor of only WBINC


# ---The Entrepreneurial Attidues ---
# This section dives into the core, high-level attitudes towards entrepreneurship
# showing a strong correlation with intention.

plot_intention_by_cat(
  model_data,
  SUSKILyy,
  title = "Proportion of Intentions by Perceived Skills (Global)",
  subtitle = "Having the perceived skill appears to be a strong driver of intention.",
  xlab = "Perceives Having Skills to Start a Business?"
)

plot_intention_by_cat(
  student_data,
  SUSKILyy,
  title = "Proportion of Intentions by Perceived Skills (Students Only)",
  subtitle = "Perceived skill is a massive driver of intention for students.",
  xlab = "Perceives Having Skills to Start a Business?"
)
# --- Student-Specific Analysis ---
# The link between perceived skills and intention is extremely strong for students.
#
# --- Comparison to Global Data ---
# The effect appears even stronger than in the general population. This reinforces
# that skills-based workshops are likely the most effective tool for the DIT
# StartUp Center to increase entrepreneurial intention.


plot_intention_by_cat(
  model_data,
  FRFAILyy,
  title = "Proportion of Entrepreneurial Intentions by Fear of Failure (Global)",
  subtitle = "A clear relationship is visible between fear of failure and intention.",
  xlab = "Fear of Failure Would Prevent Starting a Business?"
)

plot_intention_by_cat(
  student_data,
  FRFAILyy,
  title = "Proportion of Entrepreneurial Intentions by Fear of Failure (Students Only)",
  subtitle = "Fear of failure remains a significant barrier, even for students.",
  xlab = "Fear of Failure Would Prevent Starting a Business?"
)
# --- Student-Specific Analysis ---
# Students who agree that fear would stop them have a much lower intention rate.
#
# --- Comparison to Global Data ---
# The pattern is the same as globally, but because students are generally
# less fearful, addressing this barrier for the minority who are afraid could
# be a very high-impact intervention for DIT.


plot_intention_by_cat(
  model_data,
  KNOWENyy,
  title = "Proportion of Intentions by Knowing an Entrepreneur (Global)",
  subtitle = "Knowing an Entrepreneur personally is a strong driver of intention.",
  xlab = "Knows an Entrepreneur Personally?"
)

plot_intention_by_cat(
  student_data,
  KNOWENyy,
  title = "Proportion of Intentions by Knowing an Entrepreneur (Students Only)",
  subtitle = "Role models are critically important for students.",
  xlab = "Knows an Entrepreneur Personally?"
)
# --- Student-Specific Analysis ---
# Knowing an entrepreneur personally dramatically increases a student's
# intention to start a business.
#
# --- Comparison to Global Data ---
# The effect is just as strong, if not stronger, than in the general population.
# This provides a clear mandate for DIT to create mentorship programs, alumni
# networking events, and guest lectures from founders.


plot_intention_by_cat(
  model_data,
  OPPORTyy,
  title = "Proportion of Intentions by Perceived Opportunities (Global)",
  subtitle = "Perceiving opportunities is strongly correlated with intention.",
  xlab = "Perceives Good Opportunities to Start a Business?"
)

plot_intention_by_cat(
  student_data,
  OPPORTyy,
  title = "Intentions by Perceived Opportunities (Students Only)",
  subtitle = "Perceiving opportunities is strongly correlated with student intention.",
  xlab = "Perceives Good Opportunities to Start a Business?"
)
# --- Student-Specific Analysis ---
# For students, perceiving opportunities is a very strong predictor of intention.
#
# --- Comparison to Global Data ---
# The relationship is just as clear and strong as in the general population,
# confirming its status as a core driver of entrepreneurship across all groups.


# --- The Entrepreneurial Mindset ---
# This section explores deeper, more nuanced personality traits, revealing complex relationships.


plot_intention_by_cat(
  model_data,
  EASYSTyy,
  title = "Proportion of Intentions by Perceived Ease of Starting a Business (Global)",
  subtitle = "Intention to start is higher for those perceiving startin is easy in their country.",
  xlab = "Is it Easy to Start a Business in Your Country?"
)

plot_intention_by_cat(
  student_data,
  EASYSTyy,
  title = "Proportion of Intentions by Perceived Ease of Starting a Business (Students Only)",
  subtitle = "Does the perceived ease of starting a business matter to students?",
  xlab = "Is it Easy to Start a Business in Your Country?"
)
# --- Student-Specific Analysis ---
# The Split appears to be roughly 50/ 50.
# 
# --- Comparison to Global Data ---
# The correlation between Perceived Ease of Starting and the actual Intention to start is more balanced out.
# This could imply students not being aware of how easy it is to start a business.


plot_intention_by_cat(
  model_data,
  OPPISMyy,
  title = "Proportion of Intentions by Optimism (Global)",
  subtitle = "Optimistic individuals are more likely to intend to start a business.",
  xlab = "Optimism about the Future"
)

 plot_intention_by_cat(
  student_data,
  OPPISMyy,
  title = "Proportion of Intentions by Optimism (Students Only)",
  subtitle = "Optimism shows a similar U-shaped relationship for students.",
  xlab = "Optimism about the Future"
)
# --- Student-Specific Analysis ---
# The relationship is not a simple U-shape. The highest intention rate is
# found among students who "Somewhat Disagree" with being optimistic,
# possibly representing "Pragmatic Realists" motivated to solve problems.
# The classic "Opportunity-Driven Optimists" ("Strongly Agree") have the
# second-highest rate.
#
# --- Comparison to Global Data ---
# This pattern is more nuanced than the U-shape in the general population.
# It suggests that for students, a degree of realism or dissatisfaction is a
# stronger entrepreneurial driver than pure pessimism. This is a key insight
# for DIT's marketing and program design.


plot_intention_by_cat(
  model_data,
  PROACTyy,
  title = "Proportion of Intentions by \"Even when you spot a profitable opportunity, you rarely act on it\" (Global)",
  subtitle = "Counter-Intuitive: self-perceived proactiveness and entrepreneurial intention is non-linear",
  xlab = "Self-Perceived Proactiveness"
)

plot_intention_by_cat(
  student_data,
  PROACTyy,
  title = "Proportion of Intentions by \"Even when you spot a profitable opportunity, you rarely act on it\" (Students Only)",
  subtitle = "The non-linear relationship between proactiveness and intention persists.",
  xlab = "Self-Perceived Proactiveness"
)

# --- Student-Specific Analysis ---
# The "Doers" (Disagree): This group fits the classic entrepreneurial archetype. They see themselves
# as action-oriented, which translates directly into the highest intention to start a business.
# The "Fence-Sitters" (Neither): This group is undecided about their proactiveness and shows the lowest
# entrepreneurial intention, likely representing a more cautious or uncertain population segment.
# The "Aspirational Dreamers" (Agree): Counter-intuitively, this group admits to being passive yet
# still shows high intention. This suggests they possess the desire to be entrepreneurs but are
# self-aware of their own behavioral barriers to acting on opportunities.
#
# --- Comparison to Global Data ---
# This complex psychological pattern is not unique to the general population;
# it's also present in students. DIT needs to cater to both those who are
# ready to act and those who have the dream but need a push.


plot_intention_by_cat(
  model_data,
  CREATIVyy,
  title = "Proportion of Intentions by \"Other people think you are highly innovative\" (Global)",
  subtitle = "Creative individuals are more likely to intend to start a business.",
  xlab = "Self-Perceived Creativity"
)

plot_intention_by_cat(
  student_data,
  CREATIVyy,
  title = "Intentions by Perceived Creativity (Students Only)",
  subtitle = "Question: 'Other people think you are highly innovative?'",
  xlab = "Self-Perceived Creativity"
)

# --- Student-Specific Analysis ---
# This plot reveals a J-shaped relationship. As expected, intention rises
# as students agree more strongly that others see them as innovative.
# However, there is also a high intention rate among those who "Strongly Disagree".
#
# This suggests two distinct potential founder mindsets:
# 1. The "Validated Innovator": Those who feel their novel ideas are recognized.
# 2. The "Pragmatic Executor": Those who may not define success by innovation.
#    They might believe a business succeeds through superior execution of a
#    known model, not necessarily a groundbreaking idea. They disagree with
#    the statement because "innovative" isn't their primary focus.
#
# --- Comparison to Global Data ---
# This complex pattern is also present in the global data, but it provides a
# crucial insight for DIT. The StartUp Center should support both types of
# founders. They need programs for the "Innovators" (e.g., hackathons, patent
# support) and separate programs for the "Executors" (e.g., workshops on
# operational excellence, customer service, and business process management).


plot_intention_by_cat(
  model_data,
  VISIONyy,
  title = "Proportion of Intentions by \"Every decision you make is part of your long-term career plan\" (Global)",
  subtitle = "Having a clear vision for the future strongly correlates with entrepreneurial intention.",
  xlab = "Has a Clear Vision for the Future"
)

plot_intention_by_cat(
  student_data,
  VISIONyy,
  title = "Proportion of Intentions by \"Every decision you make is part of your long-term career plan\" (Students Only)",
  subtitle = "Having a long-term vision is a powerful driver for students.",
  xlab = "Has a Clear Vision for the Future"
)
# --- Student-Specific Analysis ---
# Students who agree they have a long-term career plan are far more likely
# to have entrepreneurial intentions.
#
# --- Comparison to Global Data ---
# This effect is just as strong, if not stronger, than in the general population.
# DIT can leverage this by framing entrepreneurship as a viable, long-term
# career path, not just a short-term project.


plot_intention_by_cat(
  model_data,
  INDSUPyy,
  title = "Proportion of Intentions by Perceived Societal Support (Global)",
  subtitle = "Perceiving societal support for independence is linked to higher intention.",
  xlab = "Society Supports Independent Thought and Action?"
)

plot_intention_by_cat(
  student_data,
  INDSUPyy,
  title = "Intentions by Perceived Societal Support (Students Only)",
  subtitle = "Societal support for independence is linked to higher student intention.",
  xlab = "Society Supports Independent Thought and Action?"
)

# --- Student-Specific Analysis ---
# There is a strong, clear positive relationship: as students' perception of
# societal support for independence increases, so does their entrepreneurial
# intention.
#
# --- Comparison to Global Data ---
# This effect is even more pronounced among students than in the general
# population, suggesting they are highly influenced by their cultural
# environment. This is a key insight for DIT: fostering a campus culture
# that champions independence can directly increase the number of student
# founders.


# --- Demographic Drivers ---
# This section explores how personal circumstances and
# background influence intention, revealing nuanced relationships.

ggplot(model_data, aes(x = FUTSUPNO, y = age, fill = FUTSUPNO)) +
  geom_boxplot() +
  labs(
    title = "Age Distribution by Intention to Start a Business (Global)",
    subtitle = "Respondents intending to start a business tend to be younger.",
    x = "Intends to Start in Next 3 Years?",
    y = "Age"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("No" = "firebrick", "Yes" = "seagreen")) +
  coord_flip()

ggplot(student_data, aes(x = FUTSUPNO, y = age, fill = FUTSUPNO)) +
  geom_boxplot() +
  labs(
    title = "Age Distribution by Intention (Students Only)",
    subtitle = "Within the student group, older/ more experienced students show higher intention.",
    x = "Intends to Start in Next 3 Years?",
    y = "Age"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("No" = "firebrick", "Yes" = "seagreen")) +
  coord_flip()
# --- Student-Specific Analysis ---
# The median age of students intending to start a business is slightly higher
# than those who do not.
#
# --- Comparison to Global Data ---
# This is is far lower then for the General Population. However, as the general age group is also much lower
# this makes sense.



plot_intention_by_cat(
  model_data,
  gender,
  title = "Proportion of Intentions by Gender (Global)",
  subtitle = "Males are more likely to intend to Start than Women",
  xlab = "Gender"
)

plot_intention_by_cat(
  student_data,
  gender,
  title = "Proportion of Intentions by Gender (Students Only)",
  subtitle = "Is there a gender gap in entrepreneurial intention among students?",
  xlab = "Gender"
)
# --- Student-Specific Analysis ---
# A gender gap persists within the student population, with male students
# showing a higher proportion of entrepreneurial intention than female students.
#
# --- Comparison to Global Data ---
# DIT needs to specifically focus on encouraging and supporting female student founders,
# because they have more factors holding them back.


#  Note: This is not plotted for students as it's their defining characteristic.
# The original plot reordered factors for emphasis. A standard call is used here for consistency.
ggplot(
  model_data,
  aes(
    x = fct_reorder(GEMOCCU, as.numeric(FUTSUPNO == "Yes"), .fun = mean),
    fill = FUTSUPNO
  )
) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Proportion of Intentions by Occupation Status",
    subtitle = "Entrepreneurial intention varies significantly across occupations.",
    x = "Occupation Status",
    y = "Proportion of Respondents",
    fill = "Intends to Start?"
  ) +
  theme_minimal(base_size = 14) +
  scale_fill_manual(values = c("No" = "firebrick", "Yes" = "seagreen")) +
  # Flip coordinates to make the x-axis labels readable
  coord_flip()
# --- Analysis ---
# Self-employed, Not Working, Part-time Employee Only & Students have the highest Proportion of FUTSUPNO
# -> They don't have the initial barrier of leaving traditional employment.
# -> They likely have more time that is self-managed. Therefore if they want to start a business, then
# they probably actually intend to start one in the next three years.


ggplot(
  model_data,
  aes(
    x = fct_reorder(GEMEDUC, as.numeric(FUTSUPNO == "Yes"), .fun = mean),
    fill = FUTSUPNO
  )
) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Proportion of Intentions by Education Level (Global)",
    x = "Education Level",
    y = "Proportion",
    fill = "Intends to Start?"
  ) +
  theme_minimal(base_size = 14) +
  scale_fill_manual(values = c("No" = "firebrick", "Yes" = "seagreen")) +
  coord_flip()

plot_intention_by_cat(
  student_data,
  GEMEDUC,
  title = "Intentions by Education Level (Students Only)",
  subtitle = "The relationship between education and intention is not linear.",
  xlab = "Education Level"
)

# --- Student-Specific Analysis ---
# The relationship is not linear. The highest intention rates are found among
# students in transitional phases: "Some Secondary" and "Post-Secondary".
# Students who have completed a formal degree ("Secondary Degree", "Graduate
# Experience") show lower intention rates.
#
# --- Comparison to Global Data ---
# This is a key difference from the global data, where the relationship was
# less clear. For students, it seems the specific stage of their education is
# critical. The peak intention during transitional phases suggests students
# are most open to entrepreneurship while actively exploring career paths.
# Once a degree is finished, the pull of traditional employment may be stronger.
# This is a vital insight for DIT, suggesting they should target students
# during these key transitional periods.


plot_intention_by_cat(
  model_data,
  GEMHHINC,
  title = "Proportion of Intentions by Household Income (Global)",
  subtitle = "Income in Households seems to correlate with Intention to start a business",
  xlab = "Household Income (in Thirds)"
)

plot_intention_by_cat(
  student_data,
  GEMHHINC,
  title = "Intentions by Household Income (Students Only)",
  subtitle = "Household income shows a weaker correlation for students.",
  xlab = "Household Income (in Thirds)"
)
# --- Student-Specific Analysis ---
# The relationship between household income and intention is much flatter
# for students compared to the general population.
#
# --- Comparison to Global Data ---
# This is a key insight. It suggests that for students, personal factors
# like skills, mindset, and vision are more important than their family's
# financial background.


# --- Survey Behavior as a Predictor ---
# This section covers the fascinating finding that how a person interacts with the survey is itself
# a powerful predictive signal, likely acting as a proxy for unobserved traits or complex survey design logic.

plot_intention_by_cat(
  model_data,
  Mindset_Asked,
  title = "Proportion of Intentions by Survey Group ('Mindset' Questions) (Global)",
  subtitle = "Checking for bias introduced by the split-sample design.",
  xlab = "Was Respondent Asked the Mindset Questions?"
)

plot_intention_by_cat(
  student_data,
  Mindset_Asked,
  title = "Proportion of Intentions by Survey Group ('Mindset' Questions) (Students Only)",
  subtitle = "Checking for bias from the split-sample design.",
  xlab = "Was Respondent Asked the Mindset Questions?"
)
# --- Student-Specific Analysis ---
# The correlation between being asked the mindset questions and having a
# higher intention rate persists within the student group.
#
# --- Comparison to Global Data ---
# Initial analysis suggested a simple split-sample design (MCAR). However, this plot reveals
# that the missingness of these "Mindset" variables is strongly correlated with other predictors
# (e.g., OPPORTyy). This indicates a more complex Missing at Random (MAR) mechanism, where the decision
# to ask these questions is dependent on prior answers. This makes our engineered 'Mindset_Asked'
# flag a powerful proxy for this underlying survey logic.


plot_intention_by_cat(
  model_data,
  age_is_missing,
  title = "Proportion of Intentions by Age Reporting Status (Global)",
  subtitle = "Does the act of not reporting age correlate with intention?",
  xlab = "Was the Respondent's Age Missing?"
)

plot_intention_by_cat(
  student_data,
  age_is_missing,
  title = "Intentions by Age Reporting Status (Students Only)",
  subtitle = "Does the act of not reporting age correlate with intention?",
  xlab = "Was the Respondent's Age Missing?"
)

# --- Analysis ---
# The result is counter-intuitive. People who did not provide their age are more likely to start a business.
# Maybe this is also just noise (statistical insignificant), because the "No" group is so much bigger then the "Yes" group.
# Let's check statistical Significance using Chi-squared test of independence

contingency_table <- table(model_data$age_is_missing, model_data$FUTSUPNO)
chi_test_result <- chisq.test(contingency_table)
print(chi_test_result)

# --- Analysis ---
# The age_is_missing feature is statistically highly significant and not just random chance.
# The real reason for this relationship is still unknown and requires further investigation.


cat("--- EDA PLOTS GENERATED ---\n")


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
# ctryalp: MANY categories = much noise. Most categories have very low feature importance
# Mindset_Asked: Based on survey design - not applicable to DIT Startup Campus
student_data <- student_data %>%
  select(-any_of(c("ctryalp", "Mindset_Asked")))

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
  
  # --- 1. LOGISTIC REGRESSION: LASSO with Tunable SMOTE ---
  list(
    name = "Experiment_LogReg_LASSO_SMOTE",
    model_spec = logistic_reg(penalty = tune(), mixture = 1) %>% 
      set_engine("glmnet") %>% 
      set_mode("classification"),
    recipe_func = function(data) {
      recipe(FUTSUPNO ~ ., data = data) %>%
        step_dummy(all_nominal_predictors()) %>%
        step_zv(all_predictors()) %>%
        step_smote(FUTSUPNO, over_ratio = tune()) # <<< Tunable over_ratio
    },
    grid_func = function() {
      grid_space_filling(penalty(), over_ratio(), size = 20)
    }
  ),
  
  # --- 2. LOGISTIC REGRESSION: Splines for 'age' ---
  list(
    name = "Experiment_LogReg_Splines",
    model_spec = logistic_reg(penalty = tune(), mixture = 1) %>% 
      set_engine("glmnet") %>% 
      set_mode("classification"),
    recipe_func = function(data) {
      recipe(FUTSUPNO ~ ., data = data) %>%
        step_dummy(all_nominal_predictors()) %>%
        step_zv(all_predictors()) %>%
        step_smote(FUTSUPNO) %>%
        step_ns(age, deg_free = tune()) # <<< Note the added spline step
    },
    grid_func = function() { 
      grid_regular(penalty(), deg_free(range = c(2L, 5L)), levels = 5) 
    }
  ),

  # --- 3. RANDOM FOREST: Baseline (for comparison) ---
  list(
    name = "Experiment_RF_Baseline",
    model_spec = rand_forest(mtry = tune(), min_n = tune()) %>% 
      set_engine("ranger", importance = "permutation") %>% 
      set_mode("classification"),
    recipe_func = function(data) {
      recipe(FUTSUPNO ~ ., data = data) %>%
        step_dummy(all_nominal_predictors()) %>%
        step_zv(all_predictors())
    },
    grid_func = function() { 
      grid_space_filling(mtry(range = c(2, 25)), min_n(range = c(2, 20)), size = 15) 
    }
  ),
  
  # --- 4. RANDOM FOREST: With SMOTE ---
  list(
    name = "Experiment_RF_SMOTE",
    model_spec = rand_forest(mtry = tune(), min_n = tune()) %>% 
      set_engine("ranger", importance = "permutation") %>% 
      set_mode("classification"),
    recipe_func = function(data) {
      recipe(FUTSUPNO ~ ., data = data) %>%
        step_dummy(all_nominal_predictors()) %>%
        step_zv(all_predictors()) %>%
        step_smote(FUTSUPNO, over_ratio = tune()) # <<< SMOTE with tunable ratio
    },
    grid_func = function() { 
      grid_space_filling(mtry(range = c(2, 25)), min_n(range = c(2, 20)), over_ratio(), size = 15) 
    }
  ),
  
  # --- 5. RANDOM FOREST: Manual Class Weights (THE WINNING MODEL) ---
  list(
    name = "Experiment_RF_ManualWeights",
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
    grid_func = function() { 
      grid_space_filling(mtry(range = c(2, 25)), min_n(range = c(2, 20)), size = 15) 
    }
  ),
  
  # --- 6. XGBOOST: Baseline (for comparison) ---
  list(
    name = "Experiment_XGBoost_Baseline",
    model_spec = boost_tree(min_n = tune(), tree_depth = tune(), learn_rate = tune()) %>% 
      set_engine("xgboost") %>%
      set_mode("classification"),
    recipe_func = function(data) {
      recipe(FUTSUPNO ~ ., data = data) %>%
        step_dummy(all_nominal_predictors()) %>%
        step_zv(all_predictors())
    },
    grid_func = function() { 
      grid_space_filling(min_n(), tree_depth(), learn_rate(), size = 15) 
    }
  ),
  
  # --- 7. XGBOOST: Manual Case Weights ---
  list(
    name = "Experiment_XGBoost_ManualWeights",
    model_spec = boost_tree(min_n = tune(), tree_depth = tune(), learn_rate = tune()) %>%
      set_engine("xgboost", scale_pos_weight = !!xgb_weight_value) %>%
      set_mode("classification"),
    recipe_func = function(data) {
      recipe(FUTSUPNO ~ ., data = data) %>%
        step_dummy(all_nominal_predictors()) %>%
        step_zv(all_predictors())
    },
    grid_func = function() { 
      grid_space_filling(min_n(), tree_depth(), learn_rate(), size = 15) 
    }
  ),
  
  # --- 8. XGBOOST: With SMOTE ---
  list(
    name = "Experiment_XGBoost_SMOTE",
    model_spec = boost_tree(min_n = tune(), tree_depth = tune(), learn_rate = tune()) %>% 
      set_engine("xgboost") %>% 
      set_mode("classification"),
    recipe_func = function(data) {
      recipe(FUTSUPNO ~ ., data = data) %>%
        step_dummy(all_nominal_predictors()) %>%
        step_zv(all_predictors()) %>%
        step_smote(FUTSUPNO, over_ratio = tune())
    },
    grid_func = function() { 
      grid_space_filling(min_n(), tree_depth(), learn_rate(), over_ratio(), size = 15) 
    }
  )
)


output_base_dir <- "output_final"
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
  current_grid <- config$grid_func()
  
  current_workflow <- workflow() %>%
    add_recipe(current_recipe) %>%
    add_model(current_model_spec)

  set.seed(42)
  tune_results <- tune_grid(
    current_workflow,
    resamples = cv_folds,
    grid = current_grid,
    metrics = metric_set_sens_spec,
    control = control_grid(save_pred = TRUE, verbose = FALSE)
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



# @TODO
# Make informed decision for best model and retrain on train+val dataset and evaluate on test.