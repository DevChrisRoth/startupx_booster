# ==============================================================================
# EDA PART 2: STUDENT SUB-POPULATION ANALYSIS
# Objective: Deep dive into the target demographic (Students)
# ==============================================================================

# --- Setup ---
library(tidyverse)
require(patchwork)
source("00_plotting_utils.R")

# Load Data (The processed student data for modeling)
student_data <- readRDS("../data/processed/student_model_data.rds")


# ==============================================================================
# 1. Target Variable Analysis
# ==============================================================================

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


# ==============================================================================
# 2. Univariate Analysis (Demographics)
# ==============================================================================

# --- Age Distribution ---
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

# --- Household Size (hhsize) ---
ggplot(student_data, aes(x = hhsize)) +
  geom_histogram(binwidth = 1, fill = "darkgreen", color = "white") +
  labs(
    title = "Distribution of Household Size (Students Only)",
    x = "Number of People in Household",
    y = "Count"
  ) +
  theme_minimal(base_size = 14)

# --- Perceived Skills (SUSKILyy) ---
plot_univariate_dist(
  data = student_data,
  feature_col = SUSKILyy,
  title = "Distribution of Perceived Skills (Students Only)",
  xlab = "Perceives Having Skills to Start a Business?",
  bar_color = "coral"
)

# --- Fear of Failure (FRFAILyy) ---
plot_univariate_dist(
  data = student_data,
  feature_col = FRFAILyy,
  title = "Distribution of Fear of Failure (Students Only)",
  xlab = "Would not start a business for fear it might fail",
  bar_color = "steelblue"
)


# ==============================================================================
# 3. Bivariate Analysis (Key Drivers)
# ==============================================================================

# --- Economic & Geographic Context (WBINC) ---
plot_intention_by_cat(
  student_data,
  WBINC,
  title = "Intentions by World Bank Income Level (Students Only)",
  subtitle = "Does a student's background influence their entrepreneurial intention?",
  xlab = "World Bank Income Level of Home Country"
)

# --- Change in Household Income due to Corona Virus (cphhinc) ---
plot_intention_by_cat(
  student_data,
  cphhinc,
  title = "Intentions by Change in Household Income (Students Only)",
  subtitle = "How does income shock due to COVID-19 affect student intentions?",
  xlab = "Change In Household Income due to Corona Virus"
)

# ---The Entrepreneurial Attidues ---
plot_intention_by_cat(
  student_data,
  SUSKILyy,
  title = "Proportion of Intentions by Perceived Skills (Students Only)",
  subtitle = "Perceived skill is a massive driver of intention for students.",
  xlab = "Perceives Having Skills to Start a Business?"
)

plot_intention_by_cat(
  student_data,
  FRFAILyy,
  title = "Proportion of Entrepreneurial Intentions by Fear of Failure (Students Only)",
  subtitle = "Fear of failure remains a significant barrier, even for students.",
  xlab = "Fear of Failure Would Prevent Starting a Business?"
)

plot_intention_by_cat(
  student_data,
  KNOWENyy,
  title = "Proportion of Intentions by Knowing an Entrepreneur (Students Only)",
  subtitle = "Role models are critically important for students.",
  xlab = "Knows an Entrepreneur Personally?"
)

plot_intention_by_cat(
  student_data,
  OPPORTyy,
  title = "Intentions by Perceived Opportunities (Students Only)",
  subtitle = "Perceiving opportunities is strongly correlated with student intention.",
  xlab = "Perceives Good Opportunities to Start a Business?"
)

# --- Mindset ---
plot_intention_by_cat(
  student_data,
  EASYSTyy,
  title = "Proportion of Intentions by Perceived Ease of Starting a Business (Students Only)",
  subtitle = "Does the perceived ease of starting a business matter to students?",
  xlab = "Is it Easy to Start a Business in Your Country?"
)

plot_intention_by_cat(
  student_data,
  OPPISMyy,
  title = "Proportion of Intentions by Optimism (Students Only)",
  subtitle = "Optimism shows a similar U-shaped relationship for students.",
  xlab = "Optimism about the Future"
)

plot_intention_by_cat(
  student_data,
  PROACTyy,
  title = "Proportion of Intentions by \"Even when you spot a profitable opportunity, you rarely act on it\" (Students Only)",
  subtitle = "The non-linear relationship between proactiveness and intention persists.",
  xlab = "Self-Perceived Proactiveness"
)

plot_intention_by_cat(
  student_data,
  CREATIVyy,
  title = "Intentions by Perceived Creativity (Students Only)",
  subtitle = "Question: 'Other people think you are highly innovative?'",
  xlab = "Self-Perceived Creativity"
)

plot_intention_by_cat(
  student_data,
  VISIONyy,
  title = "Proportion of Intentions by \"Every decision you make is part of your long-term career plan\" (Students Only)",
  subtitle = "Having a long-term vision is a powerful driver for students.",
  xlab = "Has a Clear Vision for the Future"
)

plot_intention_by_cat(
  student_data,
  INDSUPyy,
  title = "Intentions by Perceived Societal Support (Students Only)",
  subtitle = "Societal support for independence is linked to higher student intention.",
  xlab = "Society Supports Independent Thought and Action?"
)

# --- Demographic Drivers ---
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

plot_intention_by_cat(
  student_data,
  gender,
  title = "Proportion of Intentions by Gender (Students Only)",
  subtitle = "Is there a gender gap in entrepreneurial intention among students?",
  xlab = "Gender"
)

plot_intention_by_cat(
  student_data,
  GEMHHINC,
  title = "Intentions by Household Income (Students Only)",
  subtitle = "Household income shows a weaker correlation for students.",
  xlab = "Household Income (in Thirds)"
)

# --- Occupation & Education ---
plot_intention_by_cat(
  student_data,
  GEMEDUC,
  title = "Intentions by Education Level (Students Only)",
  subtitle = "The relationship between education and intention is not linear.",
  xlab = "Education Level"
)

# ==============================================================================
# 4. Survey Behavior Analysis
# ==============================================================================

plot_intention_by_cat(
  student_data,
  Mindset_Asked,
  title = "Proportion of Intentions by Survey Group ('Mindset' Questions) (Students Only)",
  subtitle = "Checking for bias from the split-sample design.",
  xlab = "Was Respondent Asked the Mindset Questions?"
)

plot_intention_by_cat(
  student_data,
  age_is_missing,
  title = "Intentions by Age Reporting Status (Students Only)",
  subtitle = "Does the act of not reporting age correlate with intention?",
  xlab = "Was the Respondent's Age Missing?"
)