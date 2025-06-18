# ===================================================================
# --- Exploratory Data Analysis (EDA) ---
# ===================================================================

# Load necessary libraries
library(tidyverse)
library(patchwork)

setwd("C:\\Users\\timor\\Desktop\\Hochschule\\Machine_Learning")
source("EDA\\relabel_categories.R")


# ===================================================================
# --- Stage 1: Target Variable Analysis ---
# ===================================================================
# Objective: Understand the distribution of the outcome we want to predict.

# Calculate frequency and percentage for plot labels
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
    title = "Distribution of Entrepreneurial Intentions",
    subtitle = "Target variable (FUTSUPNO) shows significant class imbalance.",
    x = "Intends to Start a Business in Next 3 Years?",
    y = "Number of Respondents"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("No" = "firebrick", "Yes" = "seagreen"))

# Finding: The 'Yes' class (19.4%) is much smaller than the 'No' class (80.6%).
# This imbalance must be addressed during modeling!


# ===================================================================
# --- Stage 2: Univariate Analysis - Numeric Features ---
# ===================================================================
# Objective: Understand the distribution of individual numeric predictors.

# --- 2a. Analysis of 'age' ---
# Note: The data has already been filtered to the 18-64 range.
plot_age_hist <- ggplot(model_data, aes(x = age)) +
  geom_histogram(binwidth = 2, fill = "steelblue", color = "white") +
  labs(title = "Distribution of Respondent Age (18-64)", y = "Count") +
  theme_minimal(base_size = 14)

plot_age_box <- ggplot(model_data, aes(y = age)) +
  geom_boxplot(fill = "steelblue") +
  labs(title = "Boxplot of Age") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

# Display plots side-by-side
plot_age_hist + plot_age_box

# Finding: The age distribution is slightly right-skewed, with a concentration
# of respondents in the 30-50 age range. There are no extreme outliers
# since we have correctly scoped the data to the 18-64 population.


# --- 2b. Analysis of 'hhsize' (Household Size) ---
# Investigate potential outliers before cleaning
summary(model_data$hhsize)

# Finding: The summary shows a max value that seems implausibly high.
# Let's remove these clear data quality issues. A threshold of 20 is chosen
# as a conservative upper limit for a plausible household size.
threshold <- 20
rows_to_remove <- sum(model_data$hhsize > threshold)
cat(
  "Found",
  rows_to_remove,
  "rows with hhsize >",
  threshold,
  "to be removed.\n"
)

# Remove the outliers (affects <0.02% of data)
model_data <- model_data %>%
  filter(hhsize <= threshold)

# Plot the cleaned distribution
ggplot(model_data, aes(x = hhsize)) +
  geom_histogram(binwidth = 1, fill = "darkgreen", color = "white") +
  labs(
    title = "Distribution of Household Size (Cleaned)",
    x = "Number of People in Household",
    y = "Count"
  ) +
  theme_minimal(base_size = 14)

# Finding: After removing a small number of erroneous outliers, the
# distribution is clearly visible and strongly right-skewed, with most
# households having between 1 and 5 members.


# ===================================================================
# --- Stage 3: Univariate Analysis - Key Categorical Features ---
# ===================================================================
# Objective: Understand the distribution of key categorical predictors.

# --- 3a. Occupation Status (GEMOCCU) ---
ggplot(model_data, aes(y = fct_rev(fct_infreq(GEMOCCU)))) +
  geom_bar(fill = "purple") +
  labs(
    title = "Distribution of Occupation Status",
    y = "Occupation",
    x = "Count"
  ) +
  theme_minimal(base_size = 14)
# --- Analysis ---
# Respondents are mostly employed, but with significant subgroups of
# students, self-employed, and non-working individuals.

# --- 3b. Perceived Skills (SUSKILyy) ---
ggplot(model_data, aes(x = SUSKILyy)) +
  geom_bar(fill = "coral") +
  labs(
    title = "Distribution of Perceived Skills",
    x = "Perceives Having Skills to Start a Business?",
    y = "Count"
  ) +
  theme_minimal(base_size = 14)
# --- Analysis ---
# Respondents perceived skills, have a near 50/50 split
# between those who feel capable and those who don't.
# -> surprising high level of perceived skill, but not implausible.

# --- 3b. Perceived Skills (FRFAILyy) ---
ggplot(model_data, aes(x = FRFAILyy)) +
  geom_bar(fill = "steelblue") +
  labs(
    title = "Distribution of Fear of Failure",
    x = "Would not start a business for fear it might fail",
    y = "Count"
  ) +
  theme_minimal(base_size = 14)
# --- Analysis ---
# Respondents Fear of Failure to start a business is rougly 50/50
# with a slight lean towards those who Agree. -> plausible

# --- Analysis - Considerations ---
# This project uses unweighted data for the classification task. The survey weights were omitted,
# meaning our sample is likely not representative of the general population and may over-represent
# individuals interested in entrepreneurship. This is a deliberate trade-off, as our goal is to
# predict individual outcomes within the sample, not to perform population-level inference.



# ===================================================================
# --- Stage 4: Bivariate Analysis: Fear of Failure vs. Intention to Start ---
# ===================================================================

# Helper function for creating consistent bivariate proportion plots
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



# --- 4.1: Economic & Geographic Context ---
plot_intention_by_cat(
  model_data,
  WBINC,
  title = "Proportion of Intentions by World Bank Income Level",
  subtitle = "Entrepreneurial intention is highest in low-income countries and decreases as national income rises.",
  xlab = "World Bank Income Level"
)
# -- Analysis --
# Lines up with the World Heat Map (see ./plots.R).
# For Low-Income Nations, this is a clear sign of "necessety-driven" entrepreneurship.
# In High-Income Nations the Opportunity Cost is higher. People have stable jobs & good salaries.
# -> Consideration: Drop ctryalp in favor of only WBINC

source("eda/world_heatmap.R")
# --- 5e. Create the World Map Plot ---
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

# --- Change in Household Income due to Corona Virus vs. Intention ---
plot_intention_by_cat(
  model_data,
  cphhinc,
  title = "Proportion of Intentions by Change in Household Income due to Corona Virus",
  subtitle = "A strong change (both positive & negative) strongly correlates with Intention.",
  xlab = "Change In Household Income due to Corona Virus"
)


# --- 4.2: The Entrepreneurial Attidues ---
# This section dives into the core, high-level attitudes towards entrepreneurship
# showing a strong correlation with intention.

plot_intention_by_cat(
  model_data,
  SUSKILyy,
  title = "Proportion of Intentions by Perceived Skills",
  subtitle = "Having the perceived skill appears to be a strong driver of intention.",
  xlab = "Perceives Having Skills to Start a Business?"
)

plot_intention_by_cat(
  model_data,
  FRFAILyy,
  title = "Proportion of Entrepreneurial Intentions by Fear of Failure",
  subtitle = "A clear relationship is visible between fear of failure and intention.",
  xlab = "Fear of Failure Would Prevent Starting a Business?"
)

plot_intention_by_cat(
  model_data,
  KNOWENyy,
  title = "Proportion of Intentions by Knowing an Entrepreneur",
  subtitle = "Knowing an Entrepreneur personally is a strong driver of intention.",
  xlab = "Knows an Entrepreneur Personally?"
)

plot_intention_by_cat(
  model_data,
  OPPORTyy,
  title = "Proportion of Intentions by Perceived Opportunities",
  subtitle = "Perceiving opportunities is strongly correlated with intention.",
  xlab = "Perceives Good Opportunities to Start a Business?"
)

# --- 4.3 The Entrepreneurial Mindset ---
# This section explores deeper, more nuanced personality traits, revealing complex relationships.
plot_intention_by_cat(
  model_data,
  EASYSTyy,
  title = "Proportion of Intentions by Perceived Ease of Starting a Business",
  subtitle = "Intention to start is higher for those perceiving startin is easy in their country.",
  xlab = "Is it Easy to Start a Business in Your Country?"
)

plot_intention_by_cat(
  model_data,
  OPPISMyy,
  title = "Proportion of Intentions by Optimism",
  subtitle = "Optimistic individuals are more likely to intend to start a business.",
  xlab = "Optimism about the Future"
)
# Low Optimism about the Future also corresponds to a surprising high Intention to Start.
# -> "necessety-driven" entrepreneurship

plot_intention_by_cat(
  model_data,
  PROACTyy,
  title = "Proportion of Intentions by \"Even when you spot a profitable opportunity, you rarely act on it\"",
  subtitle = "Counter-Intuitive: self-perceived proactiveness and entrepreneurial intention is non-linear",
  xlab = "Self-Perceived Proactiveness"
)
# --- Analysis ---
# The "Doers" (Disagree): This group fits the classic entrepreneurial archetype. They see themselves
# as action-oriented, which translates directly into the highest intention to start a business.
# The "Fence-Sitters" (Neither): This group is undecided about their proactiveness and shows the lowest
# entrepreneurial intention, likely representing a more cautious or uncertain population segment.
# The "Aspirational Dreamers" (Agree): Counter-intuitively, this group admits to being passive yet
# still shows high intention. This suggests they possess the desire to be entrepreneurs but are
# self-aware of their own behavioral barriers to acting on opportunities.

plot_intention_by_cat(
  model_data,
  CREATIVyy,
  title = "Proportion of Intentions by \"Other people think you are highly innovative\"",
  subtitle = "Creative individuals are more likely to intend to start a business.",
  xlab = "Self-Perceived Creativity"
)

plot_intention_by_cat(
  model_data,
  VISIONyy,
  title = "Proportion of Intentions by \"Every decision you make is part of your long-term career plan\"",
  subtitle = "Having a clear vision for the future strongly correlates with entrepreneurial intention.",
  xlab = "Has a Clear Vision for the Future"
)

plot_intention_by_cat(
  model_data,
  INDSUPyy,
  title = "Proportion of Intentions by Perceived Societal Support",
  subtitle = "Perceiving societal support for independence is linked to higher intention.",
  xlab = "Society Supports Independent Thought and Action?"
)


# --- 4.3: Demographic Drivers ---
# This section explores how personal circumstances and
# background influence intention, revealing nuanced relationships.

ggplot(model_data, aes(x = FUTSUPNO, y = age, fill = FUTSUPNO)) +
  geom_boxplot() +
  labs(
    title = "Age Distribution by Intention to Start a Business",
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
  title = "Proportion of Intentions by Gender",
  subtitle = "Males are more likely to intend to Start than Women",
  xlab = "Gender"
)

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
    title = "Proportion of Intentions by Education Level",
    x = "Education Level",
    y = "Proportion",
    fill = "Intends to Start?"
  ) +
  theme_minimal(base_size = 14) +
  scale_fill_manual(values = c("No" = "firebrick", "Yes" = "seagreen")) +
  coord_flip()
# --- Analysis ---
# High Education Level does not directly correlate with a higher Intention to start a business
# Although counter-intuitive, this could suggest a component of "necessety-driven" entrepreneurship
# Individuals with no formal education might not have a lot of employment options making starting their
# own business a more attractive option.


plot_intention_by_cat(
  model_data,
  GEMHHINC,
  title = "Proportion of Intentions by Household Income",
  subtitle = "Income in Households seems to correlate with Intention to start a business",
  xlab = "Household Income (in Thirds)"
)
# --- Analysis ---
# Lower Third still has unintuitively high percentage. This could be due to "necessety-driven" entrepreneurship


# --- Stage 4.4: Survey Behavior as a Predictor ---
# This section covers the fascinating finding that how a person interacts with the survey is itself
# a powerful predictive signal, likely acting as a proxy for unobserved traits or complex survey design logic.
plot_intention_by_cat(
  model_data,
  Mindset_Asked,
  title = "Proportion of Intentions by Survey Group ('Mindset' Questions)",
  subtitle = "Checking for bias introduced by the split-sample design.",
  xlab = "Was Respondent Asked the Mindset Questions?"
)
# --- Analysis ---
# Initial analysis suggested a simple split-sample design (MCAR). However, this plot reveals
# that the missingness of these "Mindset" variables is strongly correlated with other predictors
# (e.g., OPPORTyy). This indicates a more complex Missing at Random (MAR) mechanism, where the decision
# to ask these questions is dependent on prior answers. This makes our engineered 'Mindset_Asked'
# flag a powerful proxy for this underlying survey logic.

plot_intention_by_cat(
  model_data,
  age_is_missing,
  title = "Proportion of Intentions by Age Reporting Status",
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
