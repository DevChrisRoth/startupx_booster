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

gem_data <- gem_data %>% filter(!is.na(FUTSUPNO))
gem_data <- gem_data %>% filter((age >= 18 & age <= 64) | is.na(age))

# --- 5b. (WEIGHTED) Calculate Intention Proportion by Country ---
# We group by country and use weighted.mean() with the WEIGHT_L variable.
# This adjusts the sample to be representative of the 18-64 population in each country.
country_intention_weighted <- gem_data %>%
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