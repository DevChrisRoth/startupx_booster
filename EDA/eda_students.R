# ===================================================================
# --- Exploratory Data Analysis (EDA) - STUDENT SUB-POPULATION ---
# ===================================================================
#
# PURPOSE:
# This script performs a detailed exploratory data analysis specifically on the
# student sub-population from the GEM dataset. The goal is to uncover insights
# relevant to the DIT StartUp Center use case.
#
# METHODOLOGY:
# We first analyze the global dataset to understand broad trends. Then, we
# filter for respondents whose occupation is 'Student' and re-run the key
# analyses. By comparing the student-specific results to the global results,
# we can identify the unique characteristics, motivations, and barriers of
# entrepreneurial students.

library(tidyverse)
library(patchwork)

output_dir <- "output/EDA_Students_Plots"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}
cat("Saving plots to:", output_dir, "\n")


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


# --- Load and Prepare Data ---
setwd("C:\\Users\\timor\\Desktop\\Hochschule\\Machine_Learning")
source("EDA\\relabel_categories.R")

# --- Pre-Filter Data Cleaning ---
# This must be done BEFORE filtering for students to ensure the underlying
# data is clean.
threshold <- 20
rows_to_remove <- sum(model_data$hhsize > threshold)
cat(
  "Found",
  rows_to_remove,
  "rows with hhsize >",
  threshold,
  "to be removed from global dataset.\n"
)
# Remove the outliers (affects ~0.023% of data)
model_data <- model_data %>%
  filter(hhsize <= threshold)


# ===================================================================
# --- STAGE 0: FILTER FOR STUDENT POPULATION ---
# ===================================================================
cat("\n--- Filtering data for Student-Specific Analysis ---\n")
student_data <- model_data %>%
  filter(GEMOCCU == "Student") %>% 
  select(-any_of(c("GEMOCCU")))

# ===================================================================
# --- Stage 1: Target Variable Analysis (Students) ---
# ===================================================================
# Objective: Understand the distribution of entrepreneurial intention among students.

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

# ===================================================================
# --- Stage 2: Univariate Analysis - Numeric Features (Students) ---
# ===================================================================

# --- 2a. Analysis of 'age' ---
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

# --- 2b. Analysis of 'hhsize' (Household Size) ---
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


# ===================================================================
# --- Stage 3: Univariate Analysis - Categorical Features (Students) ---
# ===================================================================
# Note: The 'Occupation Status' plot is removed as all individuals here are students.

# --- 3a. Perceived Skills (SUSKILyy) ---
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

# --- 3b. Fear of Failure (FRFAILyy) ---
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



# ===================================================================
# --- Stage 4: Bivariate Analysis (Students) ---
# ===================================================================
# Helper function is inherited from the environment.
plot_intention_by_cat <- function(data, feature_col, title, subtitle, xlab) {
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

# --- 4.2: The Entrepreneurial Attitudes ---
plot_intention_by_cat(
  student_data,
  SUSKILyy,
  title = "Intentions by Perceived Skills (Students Only)",
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
  student_data,
  FRFAILyy,
  title = "Intentions by Fear of Failure (Students Only)",
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
  student_data,
  KNOWENyy,
  title = "Intentions by Knowing an Entrepreneur (Students Only)",
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

# --- 4.3 The Entrepreneurial Mindset ---
plot_intention_by_cat(
  student_data,
  EASYSTyy,
  title = "Intentions by Perceived Ease of Starting (Students Only)",
  subtitle = "Does the perceived ease of starting a business matter to students?",
  xlab = "Is it Easy to Start a Business in Your Country?"
)
# --- Student-Specific Analysis ---
# The Split appears to be roughly 50/ 50.
# 
# --- Comparison to Global Data ---
# The correlation between Perceived Ease of Starting and the actual Intention to start is more balanced out.
# This could imply students not being aware of how easy it is to start a business.

contingency_table_easyst <- student_data %>%
  count(EASYSTyy, FUTSUPNO)
print(contingency_table_easyst)
# The Output confirms that there are indeed zero students who both "Refused" to answer and said "Yes"
# to starting a business. We will NOT remove this category, as for the 4 people that did answer "No", this
# is a powerful Behavioral Signal.


plot_intention_by_cat(
  student_data,
  OPPISMyy,
  title = "Intentions by Optimism (Students Only)",
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
  student_data,
  PROACTyy,
  title = "Intentions by Proactiveness (Students Only)",
  subtitle = "The non-linear relationship between proactiveness and intention persists.",
  xlab = "Self-Perceived Proactiveness"
)
# --- Student-Specific Analysis ---
# The "Doers" (Disagree) and "Aspirational Dreamers" (Agree) show higher
# intention than the "Fence-Sitters" (Neither).
#
# --- Comparison to Global Data ---
# This complex psychological pattern is not unique to the general population;
# it's also present in students. DIT needs to cater to both those who are
# ready to act and those who have the dream but need a push.

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
  student_data,
  VISIONyy,
  title = "Intentions by Having a Clear Vision (Students Only)",
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


# --- 4.4: Demographic Drivers ---
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

# --- 4.5: Survey Behavior as a Predictor ---
plot_intention_by_cat(
  student_data,
  Mindset_Asked,
  title = "Intentions by Survey Group (Students Only)",
  subtitle = "Checking for bias from the split-sample design.",
  xlab = "Was Respondent Asked the Mindset Questions?"
)
# --- Student-Specific Analysis ---
# The correlation between being asked the mindset questions and having a
# higher intention rate persists within the student group.
#
# --- Comparison to Global Data ---
# The effect is still present, confirming that this engineered feature is a
# powerful predictor that captures underlying survey logic, even within a
# more specific sub-population.

plot_intention_by_cat(
  student_data,
  age_is_missing,
  title = "Intentions by Age Reporting Status (Students Only)",
  subtitle = "Does the act of not reporting age correlate with intention?",
  xlab = "Was the Respondent's Age Missing?"
)
# --- Student-Specific Analysis ---
# The counter-intuitive finding holds: students who did not provide their age
# are more likely to intend to start a business.
#
# --- Comparison to Global Data ---
# The persistence of this effect within a more homogeneous group (students)
# suggests it's a robust behavioral signal, not just an artifact of the
# broader population demographics.