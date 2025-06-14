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


# Target Variable: FUTSUPNO ("Entrepreneurial intentions (in 18-64 sample that is not involved in entrepreneurial activity)")
# Derived from: FUTSUP("Are you, alone or with others, expecting to start a new business... within the next three years?")

library(dplyr)
library(data.table)

setwd("C:\\Users\\timor\\Desktop\\Hochschule\\Machine_Learning")
gem_data <- fread("data/GEM2021APSGlobalIndividualLevelData_15Feb2023.csv")


# get the clean subset of people, where the Intention is known
model_data <- gem_data %>% filter(!is.na(FUTSUPNO)) # nrow(gem_data)=154_397 ---> nrow(model_data)=117_369



# --- Stage 1 Cleanup: Drop Raw Columns with Harmonized Replacements ---
columns_to_drop_stage1 <- c(
  # 1. Core Activity Filter Questions (Raw Inputs)
  # These are the initial screening questions. Their logic is fully captured
  # in the final derived flags like TEAYY, DISCENyy, FUTSUPyy, etc.
  "bstart",     # Use SUBOANW, TEAYY
  "bjobst",     # Use BJOBSTyy
  "ownmge",     # Use OWNMGEyy, BABYBUSO, ESTBBUSO
  "omjobst",    # Use OMJOBSTyy
  "discent",    # Use DISCENyy
  "futsup",     # Use FUTSUPyy, FUTSUPNO
  "busang",     # Use BUSANGyy

  # 2. Raw Perceptual & Attitude Variables
  # These are the most direct raw -> harmonized pairs.
  "knowentR",   # Use KNOWENyy
  "opportL",    # Use OPPORTyy
  "opportR",    # Use OPPORTyy
  "suskillL",   # Use SUSKILyy
  "suskillR",   # Use SUSKILyy
  "fearfailL",  # Use FRFAILyy
  "fearfailR",  # Use FRFAILyy
  "easystartL", # Use EASYSTyy
  "equalincL",  # Use EQUALIyy
  "nbgoodcL",   # Use NBGOODyy
  "nbstatusL",  # Use NBSTATyy
  "nbmediaL",   # Use NBMEDIyy

  # 3. Raw Detailed Follow-up Questions (from SU and OM sections)
  # The logic from these questions is used to create the final activity flags
  # and derived characteristic variables (e.g., TEAOPP, TEAYYHJG).
  "suacts",     # Logic is in TEAYY
  "suown",      # Logic is in TEAYY
  "suwage",     # Logic is in TEAYY
  "suwageyr",   # Logic is in BABYBUSO/ESTBBUSO
  "suwhen",     # Logic is in activity flags
  "omown",      # Logic is in TEAYY
  "omwage",     # Logic is in TEAYY
  "omwageyr",   # Logic is in BABYBUSO/ESTBBUSO
  "omwhen",     # Logic is in activity flags
  "suoppor",    # Use TEAOPP, TEANEC
  "omopport",   # Use TEAOPP, TEANEC
  "suyr5job",   # Use TEAYYHJG, TEAJOB5Y
  "omyr5job",   # Use TEAYYHJG, TEAJOB5Y
  "sunewcst",   # Use TEACUST, TEAYYNPM
  "omnewcst",   # Use TEACUST, TEAYYNPM
  "sucompet",   # Use TEACOMP, TEAYYNPM
  "omcompet",   # Use TEACOMP, TEAYYNPM
  "sunewtec",   # Use TEAYYNTC
  "omnewtec",   # Use TEAYYNTC
  "suexport",   # Use TEAYYINT, TEAEXP5C
  "omexport"    # Use TEAYYINT, TEAEXP5C
)
model_data <- model_data %>% select(-any_of(columns_to_drop_stage1))

# Verify the remaining columns
cat("--- Columns remaining after Stage 1 cleanup ---\n")
print(names(model_data))


# --- Stage 2 Cleanup: Remove Columns not required for this classification task ---
columns_to_drop_stage_2 <- c(
  # ÍD columns harmful - Model could just remember ID -> FUTSUPNO value
  "setid", "ID",
  
  # Redundant columns (we keep the alphabetic 'CTRYALP' code)
  "country_name", "country",

  # Redundant binned age columns (we use the numeric 'age')
  "age7c", "age9c",

  # Survey weights (not used for this classification task)
  # (However, MUST USE for calculating National Stastistics, or comparing different countries)
  "weight", "WEIGHT_L", "WEIGHT_A"

)
model_data <- model_data %>% select(-any_of(columns_to_drop_stage_2))

# Verify the remaining columns
cat("--- Columns remaining after Stage 2 cleanup ---\n")
print(names(model_data))


# --- Stage 3 Cleanup: Automated Data Hygiene & detecting possibly problematic columns requiring further manual investigation  ---
cat("--- Running Automated Data Sanity Checks ---\n")

# --- 1. Columns with No Information ---
# Only one unique value (Zero Variance) - this includes columns that are 100% NA
zv_cols <- names(which(sapply(model_data, function(x) length(unique(x)) == 1)))
cat("Found", length(zv_cols), "zero-variance columns:", paste(zv_cols, collapse=", "), "\n")
# These are likely answers to questions asked to people who have already started a business 
# Further Investigation would be required to check this hypothesis, but they will be removed for now
model_data <- model_data %>% select(-any_of(zv_cols))
cat("\n---------- Automatically removing zero variance columns... ---------- \n\n")

# --- 2. Columns that are Likely Problematic ---
# High-Cardinality Character Columns (likely free text or useless categories)
char_cols <- names(which(sapply(model_data, is.character)))
# Set a reasonable threshold, e.g., more than 100 unique text values
high_card_cat_cols <- char_cols[sapply(model_data[, ..char_cols], function(x) length(unique(x))) > 100]
cat("Found", length(high_card_cat_cols), "high-cardinality text columns:", paste(high_card_cat_cols, collapse=", "), "\n")


# --- 3. Columns with Low Information (caret's Near-Zero Variance) ---
library(caret)
nzv_cols <- nearZeroVar(model_data, names = TRUE)
cat("Found", length(nzv_cols), "near-zero-variance columns:", paste(nzv_cols, collapse=", "), "\n")


cat("\n--- Finished Automated Data Sanity Checks ---\n")

# Verify the remaining columns
cat("--- Columns remaining after Stage 3 cleanup ---\n")
print(names(model_data))


# --- Stage 4 Cleanup: Informed Removal of Near-Zero Variance Columns ---

# Based on analysis, the 8 high_card_cat & 48 NZV columns can be categorized.
# We will remove those that are methodologically unsound to include as predictors for FUTSUPNO.

columns_to_drop_stage4 <- c(
  # Group A: Other Outcome Variables (Risk of Target Leakage)
  # These describe other ENTREPRENEURIAL ACTIVITIES.
  # We want to analyze people who are not currently involved in TEA, but plan to do so in the next three years.
  # Examples: subustype & ombustype ("What kind of business is this?"), exreason ("OPPORTUNITY TO SELL THE BUSINESS")
  "BJOBSTyy", "OWNMGEyy", "OMJOBSTyy", "DISCENyy", "SUBA", "SUBO", "SUBOA", 
  "BABYBUSM", "ESTBBUSM", "ENTRACTALL", "ENTRACTRED", "ENTRACTEA", 
  "ENTRACTEA2", "ENTRACTEA3", "subustype", "ombustype", "SUTYPE_R",
  "OMTYPE_R", "eng_subustype", "eng_ombustype",

  # Group B: Aggregate / Gender-Specific Rates
  # These are not individual-level features and do not belong in this model.
  "TEAyyMAL", "TEAyyFEM", "TEAyyMMOT1", "TEAyyFMOT1", "TEAyyMMOT2", 
  "TEAyyFMOT2", "TEAyyMMOT3", "TEAyyFMOT3", "TEAyyMMOT4", "TEAyyFMOT4"
)

# Removing these columns for clear, documented reasons.
model_data <- model_data %>% select(-any_of(columns_to_drop_stage4))

# Verify the remaining columns
cat("--- Columns remaining after Stage 4 cleanup ---\n")
print(names(model_data))


# ===================================================================
# --- Stage 5: Final Feature Removal (Preventing Target Leakage) ---
# ===================================================================
# Objective: Remove all columns that are not suitable predictors for `FUTSUPNO`.
# The primary reason is to prevent **target leakage**, where the model learns
# from data that would not be available at the time of prediction. Our target
# population are people who *intend* to start a business but are **not currently**
# involved in early-stage activity (TEA).

columns_to_drop_stage5 <- c(
  # --- Group 1: Start-up (SU) Section Follow-ups ---
  # These questions are ONLY asked to nascent entrepreneurs (TEAYY=1).
  # Our target population (FUTSUPNO=1) has TEAYY=0 by definition.
  "suowners",       # "How many people... will own and manage this new business?"
  "sucstloc",       # "Will you have any customers in... the area where you live?"
  "sucstnat",       # "Will you have any customers in... elsewhere in your country?"
  "sucstexp",       # "Will you have any customers... outside your country?"
  "sunewprod",      # "Are any of your products or services new to people...?"
  "sunewproc",      # "Are any of the technologies or procedures used... new?"
  "sunowjob",       # "How many people are currently working for this business?"
  "sucrstart",      # "Compared to one year ago... is starting a business more difficult?"
  "sucrgrow",       # "Compared to one year ago... are your expectations for growth...?"
  "sumotiv1",       # SU Motive: "To make a difference in the world."
  "sumotiv2",       # SU Motive: "To build great wealth..."
  "sumotiv3",       # SU Motive: "To continue a family tradition."
  "sumotiv4",       # SU Motive: "To earn a living because jobs are scarce."
  "sucpnewopp",     # SU COVID: "The pandemic has provided new opportunities..."
  "sucpgovres",     # SU COVID: "The government has... effectively responded..."
  "sucptech1",      # SU COVID: "The pandemic led my business to use more digital tech..."
  "sucptech2",      # SU COVID: "My customers use more digital tech..."
  "susdg_soc",      # SU SDG: "This business prioritizes social impact..."
  "susdg_env",      # SU SDG: "This business prioritizes environmental impact..."
  "susdg_pri",      # SU SDG: "This business prioritizes sustainability..."
  "susdg_steps1",   # SU SDG: "This business has taken steps to minimize env. impact..."
  "susdg_steps2",   # SU SDG: "This business has taken steps to maximize social impact..."
  "susdg_aware1",   # SU SDG: "I am aware of government programs for social businesses."
  "susdg_aware2",   # SU SDG: "I am aware of government programs for green businesses."
  "su_om",          # "Is this the same business as...?" (links SU and OM sections)
  "su_om_ee",       # "Is this the same business as...?" (links SU and EEA sections)
  "SUTYPE_ISIC4",   # SU: "Business type ISIC version 4..."
  "subustype_code", # SU: "TEAM CODE ISIC 4.0. What kind of business is this?"

  # --- Group 2: Owner-Manager (OM) Section Follow-ups ---
  # These questions are ONLY asked to current business owners (TEAYY=1).
  "omowners",       # "How many people... both own and manage this business?"
  "ompay2",         # "Did the founders... receive any wages... before Jan 1, 2017?"
  "omcstloc",       # OM Customers: "In the area where you live?"
  "omcstnat",       # OM Customers: "Elsewhere in your country?"
  "omcstexp",       # OM Customers: "Outside your country?"
  "omnewprod",      # OM Innovation: "Are any of your products or services new...?"
  "omnewproc",      # OM Innovation: "Are any of the technologies... new?"
  "omnowjob",       # "How many people are currently working for this business?"
  "omcrstart",      # OM: "Compared to one year ago... is starting a business more difficult?"
  "omcrgrow",       # OM: "Compared to one year ago... are your expectations for growth...?"
  "ommotiv1",       # OM Motive: "To make a difference in the world."
  "ommotiv2",       # OM Motive: "To build great wealth..."
  "ommotiv3",       # OM Motive: "To continue a family tradition."
  "ommotiv4",       # OM Motive: "To earn a living because jobs are scarce."
  "omrstart",       # "Did you start this business?"
  "omcrnewopp",     # OM COVID: "The pandemic has led to new business opportunities..."
  "omcrgovres",     # OM COVID: "The government has... effectively responded..."
  "omcrtech1",      # OM COVID: "The pandemic led my business to use more digital tech..."
  "omcrtech2",      # OM COVID: "My customers use more digital tech..."
  "omsdg_soc",      # OM SDG: "This business prioritizes social impact..."
  "omsdg_env",      # OM SDG: "This business prioritizes environmental impact..."
  "omsdg_pri",      # OM SDG: "This business prioritizes sustainability..."
  "omsdg_steps1",   # OM SDG: "This business has taken steps to minimize env. impact..."
  "omsdg_steps2",   # OM SDG: "This business has taken steps to maximize social impact..."
  "omsdg_aware1",   # OM SDG: "I am aware of government programs for social businesses."
  "omsdg_aware2",   # OM SDG: "I am aware of government programs for green businesses."
  "OMTYPE_ISIC4",   # OM: "Business type ISIC version 4..."
  "ombustype_code", # OM: "TEAM CODE ISIC 4.0. What kind of business is this?"
  "omwgyr_c",       # OM: "First year salaries, profits etc, compressed"

  # --- Group 3: Business Exit (Discontinuation) Follow-ups ---
  # These questions are ONLY asked to those who have exited a business (DISCENyy=1).
  "exbuscon",         # "Did the business continue... after you quit?"
  "exreason",         # "What was the most important reason for quitting...?"
  "exreason_oth",     # "Other reason for quitting..."
  "eng_exreason_oth", # English translation of other reason
  "EXREAS_O",         # Coded "Other reason for exit"
  "EXIT_RS",          # "Reason of exit, 8 categories"
  "EXIT_CTD",         # "Discontinued a business... but business continued"
  "EXIT_ENT",         # "Discontinued a business... (includes continued businesses)"

  # --- Group 4: Informal Investor (Business Angel) Follow-ups ---
  # These questions are ONLY asked to informal investors (BUSANGyy=1).
  "bafund",           # "Approximately how much... have you personally provided?"
  "BAFUNDUS",         # Informal funds value in US$
  "barel",            # "What was your relationship with the person...?"
  "barel_oth",        # "Other type of relationship..."
  "eng_barel_oth",    # English translation of other relationship
  "BAREL_O",          # Coded "Other relationship to entrepr"
  "BUSANGyy",         # Flag for being an informal investor (an outcome, not a predictor)
  "BUSANGVL",         # Flag for being an informal investor who provided a value

  # --- Group 5: Intrapreneurship (EEA) Follow-ups ---
  # These questions are for employees developing activities for their employer.
  "ipactive",       # "In the last three years, have you been involved in... new activities?"
  "ipactivenow",    # "And are you currently involved in... new activity?"
  "ipphase1",       # "Phase 1: Have you been actively involved in idea development...?"
  "ipphase1role",   # "And could you tell me whether you had a leading or a supporting role...?"
  "ipphase2",       # "Phase 2: Have you been actively involved in preparation...?"
  "ipphase2role",   # "And could you tell me whether you had a leading or a supporting role...?"
  "ipinit",         # "Was the... new activity... initiated by yourself, your employer, or...?"
  "ipteamsize",     # "How many people have... a leading role in developing this new activity?"
  "ipjob5yr",       # "How many people do you expect to be working on the new activity...?"
  "ipcstloc",       # EEA Customers: "In the area where you live?"
  "ipcstnat",       # EEA Customers: "Elsewhere in your country?"
  "ipcstexp",       # EEA Customers: "Outside your country?"
  "ipexport",       # "What percentage of your annual sales revenues for this project...?"
  "ipnewprod",      # EEA Innovation: "Are any of your products or services... new?"
  "ipnewproc",      # EEA Innovation: "Are any of the technologies... new?"
  "ipcrnewopp",     # EEA COVID: "The pandemic has led to new business opportunities..."
  "ipcrgovres",     # EEA COVID: "The government has... effectively responded..."
  "IPACT_ALL",      # Harmonized flag: "Active as intrapreneur in past three years"
  "IPACT_EMP",      # Harmonized flag: "... (base: employment population)"
  "IPACTNOW_ALL",   # Harmonized flag: "Active as intrapreneur now"
  "IPACTNOW_EMP",   # Harmonized flag: "... (base: employment population)"
  "IPACTLD_ALL",    # Harmonized flag: "Active and leading as intrapreneur..."
  "IPACTLD_EMP",    # Harmonized flag: "... (base: employment population)"
  "IPACTLDNOW_ALL", # Harmonized flag: "Active and leading as intrapreneur now"
  "IPACTLDNOW_EMP", # Harmonized flag: "... (base: employment population)"
  "IP_MKSC",        # EEA: "Market Scope"
  "IPNOW_MKSC",     # EEA Now: "Market Scope"
  "IP_NEWPROD",     # EEA: "Any products or services... new?"
  "IPNOW_NEWPROD",  # EEA Now: "Any products or services... new?"
  "IP_NEWPROC",     # EEA: "Are any of the technologies... new?"
  "IPNOW_NEWPROC",  # EEA Now: "Are any of the technologies... new?"
  "IPIMPACT1",      # EEA: "at least national scope for market and new product..."
  "IPNOWIMPACT1",   # EEA Now: "at least national scope..."
  "IPIMPACT2",      # EEA: "at least international scope..."
  "IPNOWIMPACT2",   # EEA Now: "at least international scope..."
  "IP_yyEXP",       # EEA: "export intensity"
  "IPNOW_yyEXP",    # EEA Now: "export intensity"
  "IP_yyKIX",       # EEA: "signif export intensity (>10%)"
  "IPNOW_yyKIX",    # EEA Now: "signif export intensity (>10%)"
  "ip_yyhix",       # EEA: "high export intensity (>50%)"
  "ipnow_yyhix",    # EEA Now: "high export intensity (>50%)"
  "IP_JOB5Y",       # EEA: "expected number of jobs in 5 years"
  "IPNOW_JOB5Y",    # EEA Now: "expected number of jobs in 5 years"
  "ip_yyj5y",       # EEA: "expected number of jobs, 4 categories"
  "ipnow_yyj5y",    # EEA Now: "expected number of jobs, 4 categories"
  "IP_yyHJG",       # EEA: "expects more than 19 jobs in 5 years"
  "IPNOW_yyHJG",    # EEA Now: "expects more than 19 jobs in 5 years"
  "IP_EXP4C",       # EEA: "export intensity, 4 categories"
  "IPNOW_EXP4C",    # EEA Now: "export intensity, 4 categories"
  "EEA_NEWOPP",     # EEA COVID: "has coronavirus pandemic led to new business opportunities"
  "EEA_GOVRES",     # EEA COVID: "has government so far effectively responded"

  # --- Group 6: Other Outcome Variables & Redundant/Index Predictors ---
  "Bstartyy",     # An outcome flag for starting a business.
  "SUB",          # An outcome flag: "Reports new start-up effort (independent or job)"
  "FUTSUPyy",     # Direct predecessor to the target variable FUTSUPNO.
  "FRFAILOP",     # "Fear of failure (in ... sample perceiving good opportunities...)" - conditional variable
  "INDSUPyy",     # "Individual perception to entrepreneurship Index" - composite of other features
  "CULSUPyy",     # "Cultural support for entrepreneurship Index" - composite of other features
  # The following are just different aggregations/codings of the base indices
  "INDSPyyA", "INDSPyyB", "INDSPyyC", "INDSPyyD",
  "CULSPyyA", "CULSPyyB", "CULSPyyC", "CULSPyyD",
  # The following are different binary/tertiary codings of the 5-point Likert scales you already have
  "OPPIS_1", "OPPIS_2", "OPPIS_3",
  "PROAC_1", "PROAC_2", "PROAC_3",
  "CREATI_1", "CREATI_2", "CREATI_3",
  "VISIO_1", "VISIO_2", "VISIO_3",
  "SOCEN_1", "SOCEN_2", "SOCEN_3",

  # --- Group 7: Survey Process & Metadata ---
  # These describe the survey process, not the respondent's profile.
  "dtsurv",       # "Date of Survey"
  "MSURV",        # "Month of survey"
  "contact",      # "how the respondent was interviewed"
  "callback",     # "what number call-back attempt"
  "incontact",    # "which initial contact attempt a household was reached"
  "doublecount",  # "Do you also have a fixed-line/mobile telephone?"

  # --- Group 8: Historical / Redundant Demographics ---
  # These are pre-COVID work status questions. The current GEMOCCU variables are sufficient.
  "bc_occufull", "bc_occupart", "bc_occuself", "bc_occuseek"
)

# Execute the final cleanup
model_data <- model_data %>%
  select(-any_of(columns_to_drop_stage5))

# Verify the final, clean set of columns
cat("--- Columns remaining after FINAL Stage 5 cleanup ---\n")
print(names(model_data))



# ===================================================================
# --- Stage 6 Cleanup: Correct Datatypes (numeric, ordinal, factor) ---
# ===================================================================

numeric_cols <- c(
  "age",    # "B. What is your current age (in years)?"
  "hhsize"  # "E. How many members make up your permanent household...?"
)

ordinal_cols <- c(
)

factor_cols <- c(
)
