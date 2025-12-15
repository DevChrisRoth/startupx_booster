# ==============================================================================
# EDA PART 1: GLOBAL POPULATION ANALYSIS
# Objective: Analyze broad trends across the entire dataset (18-64 population)
# ==============================================================================

# --- Setup ---
library(tidyverse)
library(sf)                # For World Map
library(rnaturalearth)     # For World Map
library(rnaturalearthdata) # For World Map
require(patchwork)

source("00_plotting_utils.R") # Load standardized plots

# Load Data (Adjust path if necessary)
# Note: For the map we need the data with weights/country codes. 
# Using intermediate data from the pipeline.
model_data <- readRDS("../data/intermediate/global_cleaned_data.rds")


# ==============================================================================
# 1. Target Variable Analysis
# ==============================================================================

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


# ==============================================================================
# 2. Global Heatmap
# ==============================================================================
plot_global_heatmap(model_data)


# ==============================================================================
# 3. Univariate Analysis (Numeric & Categorical)
# ==============================================================================

# --- Age Distribution ---
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


# --- Household Size (hhsize) ---
ggplot(model_data, aes(x = hhsize)) +
  geom_histogram(binwidth = 1, fill = "darkgreen", color = "white") +
  labs(
    title = "Distribution of Household Size (Global)",
    x = "Number of People in Household",
    y = "Count"
  ) +
  theme_minimal(base_size = 14)


# --- Occupation Status (GEMOCCU) ---
plot_univariate_dist(
    data = model_data,
    feature_col = GEMOCCU,
    title = "Distribution of Occupation Status (Global)",
    xlab = "Occupation",
    bar_color = "purple"
) + coord_flip() # Flipping coordinates for readability of labels


# --- Perceived Skills (SUSKILyy) ---
plot_univariate_dist(
  data = model_data,
  feature_col = SUSKILyy,
  title = "Distribution of Perceived Skills (Global)",
  xlab = "Perceives Having Skills to Start a Business?",
  bar_color = "coral"
)


# --- Fear of Failure (FRFAILyy) ---
plot_univariate_dist(
  data = model_data,
  feature_col = FRFAILyy,
  title = "Distribution of Fear of Failure (Global)",
  xlab = "Would not start a business for fear it might fail",
  bar_color = "steelblue"
)


# ==============================================================================
# 4. Bivariate Analysis (Key Drivers)
# ==============================================================================

# --- Economic & Geographic Context (WBINC) ---
plot_intention_by_cat(
  model_data,
  WBINC,
  title = "Proportion of Intentions by World Bank Income Level",
  subtitle = "Entrepreneurial intention is highest in low-income countries and decreases as national income rises.",
  xlab = "World Bank Income Level"
)

# --- Change in Household Income due to Corona Virus (cphhinc) ---
plot_intention_by_cat(
  model_data,
  cphhinc,
  title = "Proportion of Intentions by Change in Household Income due to Corona Virus (Global)",
  subtitle = "A strong change (both positive & negative) strongly correlates with Intention.",
  xlab = "Change In Household Income due to Corona Virus"
)


# ---The Entrepreneurial Attidues ---
plot_intention_by_cat(
  model_data,
  SUSKILyy,
  title = "Proportion of Intentions by Perceived Skills (Global)",
  subtitle = "Having the perceived skill appears to be a strong driver of intention.",
  xlab = "Perceives Having Skills to Start a Business?"
)

plot_intention_by_cat(
  model_data,
  FRFAILyy,
  title = "Proportion of Entrepreneurial Intentions by Fear of Failure (Global)",
  subtitle = "A clear relationship is visible between fear of failure and intention.",
  xlab = "Fear of Failure Would Prevent Starting a Business?"
)

plot_intention_by_cat(
  model_data,
  KNOWENyy,
  title = "Proportion of Intentions by Knowing an Entrepreneur (Global)",
  subtitle = "Knowing an Entrepreneur personally is a strong driver of intention.",
  xlab = "Knows an Entrepreneur Personally?"
)

plot_intention_by_cat(
  model_data,
  OPPORTyy,
  title = "Proportion of Intentions by Perceived Opportunities (Global)",
  subtitle = "Perceiving opportunities is strongly correlated with intention.",
  xlab = "Perceives Good Opportunities to Start a Business?"
)

# --- Mindset ---
plot_intention_by_cat(
  model_data,
  EASYSTyy,
  title = "Proportion of Intentions by Perceived Ease of Starting a Business (Global)",
  subtitle = "Intention to start is higher for those perceiving startin is easy in their country.",
  xlab = "Is it Easy to Start a Business in Your Country?"
)

plot_intention_by_cat(
  model_data,
  OPPISMyy,
  title = "Proportion of Intentions by Optimism (Global)",
  subtitle = "Optimistic individuals are more likely to intend to start a business.",
  xlab = "Optimism about the Future"
)

plot_intention_by_cat(
  model_data,
  PROACTyy,
  title = "Proportion of Intentions by \"Even when you spot a profitable opportunity, you rarely act on it\" (Global)",
  subtitle = "Counter-Intuitive: self-perceived proactiveness and entrepreneurial intention is non-linear",
  xlab = "Self-Perceived Proactiveness"
)

plot_intention_by_cat(
  model_data,
  CREATIVyy,
  title = "Proportion of Intentions by \"Other people think you are highly innovative\" (Global)",
  subtitle = "Creative individuals are more likely to intend to start a business.",
  xlab = "Self-Perceived Creativity"
)


plot_intention_by_cat(
  model_data,
  VISIONyy,
  title = "Proportion of Intentions by \"Every decision you make is part of your long-term career plan\" (Global)",
  subtitle = "Having a clear vision for the future strongly correlates with entrepreneurial intention.",
  xlab = "Has a Clear Vision for the Future"
)

plot_intention_by_cat(
  model_data,
  INDSUPyy,
  title = "Proportion of Intentions by Perceived Societal Support (Global)",
  subtitle = "Perceiving societal support for independence is linked to higher intention.",
  xlab = "Society Supports Independent Thought and Action?"
)

# --- Demographic Drivers ---
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

plot_intention_by_cat(
  model_data,
  gender,
  title = "Proportion of Intentions by Gender (Global)",
  subtitle = "Males are more likely to intend to Start than Women",
  xlab = "Gender"
)

plot_intention_by_cat(
  model_data,
  GEMHHINC,
  title = "Proportion of Intentions by Household Income (Global)",
  subtitle = "Income in Households seems to correlate with Intention to start a business",
  xlab = "Household Income (in Thirds)"
)

# --- Occupation & Education ---
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
  gem_theme + scale_fill_manual(values = c("No" = "firebrick", "Yes" = "seagreen")) + coord_flip()


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
  gem_theme + scale_fill_manual(values = c("No" = "firebrick", "Yes" = "seagreen")) + coord_flip()



# ==============================================================================
# 5. Survey Behavior Analysis
# ==============================================================================
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
  model_data,
  age_is_missing,
  title = "Proportion of Intentions by Age Reporting Status (Global)",
  subtitle = "Does the act of not reporting age correlate with intention?",
  xlab = "Was the Respondent's Age Missing?"
)


contingency_table <- table(model_data$age_is_missing, model_data$FUTSUPNO)
chi_test_result <- chisq.test(contingency_table)
print(chi_test_result)
