# ===================================================================
# --- Geographic Analysis - World Map Heatmap ---
# ===================================================================
# Objective: Visualize the geographic distribution of entrepreneurial intention.

# --- 5a. Install and Load Necessary Libraries ---
# for reproduceablity
set.seed(42)

library(dplyr)
library(data.table)
library(caret)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(dplyr)
library(ggplot2)


setwd("C:\\Users\\timor\\Desktop\\Hochschule\\Machine_Learning")
gem_data <- fread("data/GEM2021APSGlobalIndividualLevelData_15Feb2023.csv")

model_data <- gem_data %>% filter(!is.na(FUTSUPNO))
model_data <- model_data %>% filter((age >= 18 & age <= 64) | is.na(age))

# --- 5b. (WEIGHTED) Calculate Intention Proportion by Country ---
# We group by country and use weighted.mean() with the WEIGHT_L variable.
# This adjusts the sample to be representative of the 18-64 population in each country.
country_intention_weighted <- model_data %>%
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

# --- 5d. Join Survey Data with Map Data ---
# Join our weighted summary data with the map's spatial data by country code.
world_map_with_data <- world_map_sf %>%
  left_join(country_intention_weighted, by = c("iso_a2" = "ctryalp"))

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