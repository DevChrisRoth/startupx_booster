# Exploratory Data Analysis Findings

## 1. Target Variable Distribution
*   **Student Specifics:** Among students, the 'Yes' class (Intention to start) is **19.3%** and the 'No' class is **80.7%**.
*   **Comparison:** The intention rate for students is really similar to the general population (19.4%).
*   **Implication:** Students are a high-potential group for entrepreneurial initiatives as they usually have fewer obligations than the rest of the population. This validates the need for more specific support by the DIT StartUp Campus.

## 2. Demographics

### Age
*   **Distribution:** The age distribution for students is heavily left-skewed, with the vast majority concentrated between **18 and 25 years old**. This is drastically different from the general population (centered around 30-50).
*   **Intention:** Within the student group, the median age of those intending to start a business is slightly higher than those who do not.
*   **Comparison:** However, this median age is **far lower than for the General Population**. This makes sense given the general age group of students is much lower.
*   **Validation:** This validates our filter and highlights the youth of our target demographic for the DIT use case.

### Household Size
*   **Observation:** The household size for students is right-skewed, but the peak appears to be slightly lower (around 3-5 members) than the general population.
*   **Context:** This could reflect students often living in small student apartments instead of at home. This is a minor difference but adds context to their living situations.

### Gender
*   **Gap:** A gender gap persists within the student population, with male students showing a higher proportion of entrepreneurial intention than female students.
*   **Action:** DIT needs to specifically focus on encouraging and supporting female student founders, because they have more factors holding them back.

### Education Level
*   **Non-Linearity:** The relationship is not linear. The highest intention rates are found among students in transitional phases: "Some Secondary" and "Post-Secondary". Students who have completed a formal degree ("Secondary Degree", "Graduate Experience") show lower intention rates.
*   **Comparison:** This is a key difference from the global data, where the relationship was less clear.
*   **Insight:** The peak intention during transitional phases suggests students are most open to entrepreneurship while actively exploring career paths. Once a degree is finished, the pull of traditional employment may be stronger. DIT should target students during these key transitional periods.

### International Background
*   **Trend:** The trend of higher intention among students from lower-income countries is even more pronounced than in the general population.
*   **Insight:** This strongly suggests that "necessity-driven" entrepreneurship is a powerful motivator for international students from developing economies.
*   **Action:** DIT can use this to tailor their messaging, offering entrepreneurship as a viable and empowering career path.

### Income
*   **Household Income:** The relationship between household income and intention is much flatter for students compared to the general population.
*   **Insight:** For students, personal factors like skills, mindset, and vision are more important than their family's financial background.
*   **COVID Impact:** The pattern of high intention at the extremes (Strongly Decrease/Increase in income due to COVID) holds for students. Economic shocks, whether positive (creating opportunity) or negative (creating necessity), are a powerful catalyst for entrepreneurial thought.

## 3. Entrepreneurial Attitudes & Skills

### Perceived Skills
*   **Confidence Gap:** Among students, **63.0% 'Disagree'** and 37.0% 'Agree' that they have the necessary skills. Students show a drastically smaller level of confidence compared to the general population.
*   **Impact:** The link between perceived skills and intention is extremely strong for students (even stronger than global).
*   **Hypothesis:** The Center's marketing should heavily emphasize their practical, skills-based workshops (e.g., coding bootcamps, marketing seminars, finance 101). This directly addresses the most significant lever we can pull.
*   **Note:** *This hypothesis needs to be confirmed after modeling is complete.*

### Fear of Failure
*   **Observation:** For students, the split is 49.7% 'Disagree' and 50.3% 'Agree'. Students who agree that fear would stop them have a much lower intention rate.
*   **Comparison:** The pattern is the same as globally, but **students are generally less fearful**.
*   **Hypothesis:** Addressing this barrier for the minority who *are* afraid could be a very high-impact intervention. The DIT StartUp Center should host workshops specifically on 'Overcoming Founder Anxiety' or feature successful alumni who can talk about their own early failures.

### Role Models (Knowing an Entrepreneur)
*   **Impact:** Knowing an entrepreneur personally dramatically increases a student's intention to start a business. The effect is just as strong, if not stronger, than in the general population.
*   **Action:** This provides a clear mandate for DIT to create mentorship programs, alumni networking events, and guest lectures from founders.

### Perceived Opportunities
*   **Impact:** For students, perceiving opportunities is a very strong predictor of intention. The relationship is just as clear and strong as in the general population, confirming its status as a core driver.

### Ease of Starting
*   **Distribution:** The Split appears to be roughly **50/50**.
*   **Correlation:** The correlation between Perceived Ease of Starting and the actual Intention to start is more balanced out.
*   **Implication:** This could imply students are not aware of how easy (or hard) it actually is to start a business.

## 4. The Entrepreneurial Mindset

### Optimism
*   **Pattern:** The relationship is not a simple U-shape. The highest intention rate is found among students who "Somewhat Disagree" with being optimistic, possibly representing "Pragmatic Realists" motivated to solve problems. The classic "Opportunity-Driven Optimists" ("Strongly Agree") have the second-highest rate.
*   **Insight:** A degree of realism or dissatisfaction is a stronger entrepreneurial driver than pure pessimism. This is a key insight for DIT's marketing and program design.

### Proactiveness
*   **The "Doers" (Disagree):** Fit the classic entrepreneurial archetype. Action-oriented, highest intention.
*   **The "Fence-Sitters" (Neither):** Undecided, lowest intention.
*   **The "Aspirational Dreamers" (Agree):** Counter-intuitively, this group admits to being passive yet still shows high intention. They possess the desire but are self-aware of their behavioral barriers.
*   **Action:** DIT needs to cater to both those who are ready to act and those who have the dream but need a push.

### Creativity / Innovation
*   **Pattern:** J-shaped relationship. Intention rises as students agree they are innovative, BUT there is also a high intention rate among those who "Strongly Disagree".
*   **Two Archetypes:**
    1.  **The "Validated Innovator":** Those who feel their novel ideas are recognized.
    2.  **The "Pragmatic Executor":** Those who do not define success by innovation but by execution.
*   **Action:** The StartUp Center should support both types: Hackathons for Innovators, Operational Excellence workshops for Executors.

### Vision
*   **Impact:** Students who agree they have a long-term career plan are far more likely to have entrepreneurial intentions.
*   **Action:** DIT can leverage this by framing entrepreneurship as a viable, long-term career path, not just a short-term project.

### Societal Support
*   **Impact:** There is a strong, clear positive relationship: as students' perception of societal support for independence increases, so does their entrepreneurial intention. This effect is even more pronounced among students than in the general population.
*   **Action:** Fostering a campus culture that champions independence can directly increase the number of student founders.

## 5. Survey Behavior & Methodology Findings

### The "Mindset" Missingness
*   **Finding:** The correlation between being asked the mindset questions and having a higher intention rate persists within the student group.
*   **Technical Insight:** Initial analysis suggested a simple split-sample design (MCAR). However, this reveals that the missingness is strongly correlated with other predictors (e.g., OPPORTyy). This indicates a complex Missing at Random (MAR) mechanism. Our engineered `Mindset_Asked` flag is a powerful proxy for this underlying survey logic.

### Missing Age
*   **Finding:** The `age_is_missing` feature is statistically highly significant. People who did not provide their age are more likely to start a business.
*   **Status:** The real reason for this relationship is still unknown and requires further investigation.

### Occupation Status
*   **Finding:** Self-employed, Not Working, Part-time Employee Only & Students have the highest Proportion of Intention.
*   **Reasoning:** They don't have the initial barrier of leaving traditional employment and likely have more self-managed time. Therefore, if they want to start a business, they probably actually intend to start one in the next three years.