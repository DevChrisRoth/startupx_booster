library(tidyverse)
library(dplyr)
library(data.table)
library(haven)


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
    "Refused_Answer",
    "Not_Applicable"
  )

  factor(
    case_when(
      variable == "1" ~ "Strongly Disagree",
      variable == "2" ~ "Somewhat Disagree",
      variable == "3" ~ "Neither",
      variable == "4" ~ "Somewhat Agree",
      variable == "5" ~ "Strongly Agree",
      variable == "Refused_Answer" ~ "Refused_Answer",
      variable == "Not_Applicable" ~ "Not_Applicable",
      # Add a fallback just in case, though it shouldn't be needed
      TRUE ~ "Refused_Answer"
    ),
    levels = likert_levels,
    ordered = TRUE
  )
}


# Relabeling the data by overwriting original columns 
apply_gem_relabeling <- function(data) {
  data %>%
    mutate(
      # 1. Ensure Numerics are Numeric
      age = as.numeric(age),
      hhsize = as.numeric(hhsize),

      # 2. Perform all Factor relabeling
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
}