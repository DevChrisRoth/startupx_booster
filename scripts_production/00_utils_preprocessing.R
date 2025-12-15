library(tidyverse)
library(dplyr)
library(data.table)
library(haven)


# --- A. COLUMN DEFINITIONS (Single Source of Truth) ---

# 1. Columns to drop because they leak the target
get_leakage_columns <- function() {
  c(
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
}

# --- Stage 1.4: Remove Raw, Redundant Disaggregated & Metadata Variables ---
# Objective: Use the official GEM harmonized variables (...yy) for modeling,
# as they represent the official methodology. They don't give the raw Likert Scale answers
# This avoids multicollinearity and simplifies the model, as Multicollinearity
# destabilizes the coefficients, i.e. a small change in the
# dataset might have a big impact on the value of the coefficients.
# Consideration: Use the more granular Binary Flags for more
# information as an experiment to improve performance evaluation metrics.
get_redundant_columns <- function() {
  c(
    # Core Activity Filter Questions (Replaced by ...yy flags)
    "bstart", "bjobst", "ownmge", "omjobst", "discent", "futsup", "busang",

    # Raw Perceptual & Attitude Variables (Replaced by ...yy flags)
    "knowentR", "opportL", "opportR", "suskillL", "suskillR", "fearfailL",
    "fearfailR", "easystartL", "equalincL", "nbgoodcL", "nbstatusL",
    "nbmediaL",

    # 5-point Likert Scales (Replaced by ...yy flags)
    "oppism", "proact", "creativ", "vision", "nbsocentL",

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
    "WBincREV",

    # IDs and redundant country codes
    "setid", "ID", "country_name", "country", "REGION",

    # Redundant binned age columns (we use the numeric 'age')
    "age7c", "age9c",

    # Survey process metadata
    "dtsurv", "MSURV", "contact", "callback", "incontact", "doublecount",

    # Historical demographic questions (pre-COVID)
    "bc_occufull", "bc_occupart", "bc_occuself", "bc_occuseek"
  )
}

# 3. Columns identified as "Attitudes" (High Partial Missingness)
get_attitude_columns <- function() {
  c("CULSUPyy", "EQUALIyy", "NBGOODyy", "NBSTATyy", "NBMEDIyy")
}

# 4. Columns identified as "Mindset" (Split-Sample)
get_mindset_columns <- function() {
  c("OPPISMyy", "PROACTyy", "CREATIVyy", "VISIONyy")
}

# 5. Remaining columns which have too high missingness
get_high_missingness_columns <- function() {
  c("FRFAILOP")
}

# 6. Columns not necessary for Students
get_unnecessary_student_columns <- function() {
  c(
    # Survey weights (not used for this classification task)
    # These are applied so that the sample is representative to the national population
    # Could be explored for real world predictive power across the population, or comparing national statistics.
    # (e.g., "25% of the German population intends to start a business",
    # whereas the goal here is predicting the outcome for a given individual)
    # Are still used for EDA_Global, so remove after
    "weight", "WEIGHT_L", "WEIGHT_A",
    "weight", "WEIGHT_L", "WEIGHT_A",

    # Analysis focuses on Germany, because ctryalp would lead to overfitting
    "ctryalp",

    # The occupation of "Student" is a key characteristic and is the same for our training subset of students
    "GEMOCCU"
  )
}



# --- B. LOADING LOGIC ---

# Performs the initial loading, SAV conversion
load_data <- function(data_dir = "data/raw") {
  # Define file name
  file_name <- "GEM2021APSGlobalIndividualLevelData_15Feb2023"

  # Define file paths for the root project directory
  sav_file_path <- file.path(data_dir, paste0(file_name, ".sav"))
  csv_file_path <- file.path(data_dir, paste0(file_name, ".csv"))

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
  gem_data <- fread(csv_file_path)

  cat("--- Stage 1.0: Data loaded successfully from the root directory.\n")

  return (gem_data)
}


#' Scope Data
#' 
#' filtering for Age/Target,
#' and removal of Leakage/Redundant columns.
#' 
#' @return A dataframe ready for Missing Value Analysis or Imputation
scope_data <- function(gem_data) {

  # 2. Initial Scoping (Target & Age)
  model_data <- gem_data %>% filter(!is.na(FUTSUPNO))
  model_data <- model_data %>% filter((age >= 18 & age <= 64) | is.na(age))
  cat("--- Stage 1.1: Data scoped successfully. Rows with known target variable:", nrow(model_data), "---\n\n")


  # 3. Drop Leakage & Redundant Columns
  # We combine the lists here
  cols_to_drop <- c(get_leakage_columns(), get_redundant_columns())
  
  model_data <- model_data %>% select(-any_of(cols_to_drop))

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

  return (model_data)
}