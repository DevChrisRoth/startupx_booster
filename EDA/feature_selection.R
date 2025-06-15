# ===================================================================
# --- STAGED DATA CLEANING ---
# ===================================================================

# ===================================================================
# --- Training Objective ---
# Target Variable:
#   An individual's intention to start a business within the next three years (a binary variable).
# Features:
#   Demographics (age, gender, education level), perceptual variables (perceived opportunities, perceived capabilities, fear of failure), and socio-economic factors.
# Verification:
#   Your model's findings on the importance of factors like fear of failure can be compared to the global and national report findings.
# ===================================================================

library(dplyr)
library(data.table)
library(caret)

setwd("C:\\Users\\timor\\Desktop\\Hochschule\\Machine_Learning")
gem_data <- fread("data/GEM2021APSGlobalIndividualLevelData_15Feb2023.csv")


# --- Stage 1: Initial Scoping ---
# Objective: Filter the dataset to only include respondents where the target
# variable is known. This defines our modeling population.
model_data <- gem_data %>% filter(!is.na(FUTSUPNO)) # nrow(gem_data)=154_397 ---> nrow(model_data)=117_369
cat("--- Data loaded. Rows with known target variable:", nrow(model_data), "---\n\n")


# --- Stage 2: Remove Target Leakage Variables ---
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
cat("--- Stage 2: Leakage variables removed. Columns remaining:", ncol(model_data), "---\n\n")

# --- Stage 3: Drop Raw Variables in Favor of Harmonized Versions ---
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
cat("--- Stage 3: Raw variables removed. Columns remaining:", ncol(model_data), "---\n\n")

# --- Stage 4: Remove Redundant Disaggregated Variables ---
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
  "--- Stage 4: Redundant disaggregated variables removed. Columns remaining:",
  ncol(model_data), "---\n\n"
)

# --- Stage 5: Remove Non-Predictor & Metadata Columns ---
# Objective: Remove columns that are not features of the individual, such as
# IDs, redundant codes, survey weights, and survey process metadata.
columns_to_drop_stage5 <- c(
  # IDs and redundant country codes
  "setid", "ID", "country_name", "country", "REGION",

  # Redundant binned age columns (we use the numeric 'age')
  "age7c", "age9c",

  # Survey weights (not used for this classification task)
  # These are applied so that the sample is representative to the national population
  # Could be explored for real world predictive power across the population, or comparing national statistics
  "weight", "WEIGHT_L", "WEIGHT_A",

  # Survey process metadata
  "dtsurv", "MSURV", "contact", "callback", "incontact", "doublecount",

  # Historical demographic questions (pre-COVID)
  "bc_occufull", "bc_occupart", "bc_occuself", "bc_occuseek"
)
model_data <- model_data %>% select(-any_of(columns_to_drop_stage5))
cat("--- Stage 5: Metadata removed. Columns remaining:", ncol(model_data), "---\n\n")


# --- Stage 6: Automated Data Hygiene ---
# Objective: Use automated checks as a final safety net to find and remove
# columns with no variance and to flag other potential issues.
cat("--- Running Automated Data Sanity Checks ---\n")
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
cat("--- Finished Automated Data Sanity Checks. Columns remaining:", ncol(model_data), "---\n\n")


# --- Stage 7: Correct Datatypes ---
# Objective: Define the correct data type for each remaining feature
# before modeling. This is the final pre-processing step.
cat("--- Stage 7: Correcting data types for modeling ---\n")

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
