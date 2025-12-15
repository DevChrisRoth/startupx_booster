# ==============================================================================
# PLOTTING UTILITIES
# Purpose: Helper functions for standardized EDA plots
# ==============================================================================

library(ggplot2)
library(dplyr)
library(scales)
library(forcats)
library(sf)                # For World Map
library(rnaturalearth)     # For World Map
library(rnaturalearthdata) # For World Map
library(viridis)           # For Map Colors

# 1. Define the theme once (Standardizing your theme_minimal(base_size=14))
gem_theme <- theme_minimal(base_size = 14) +
  theme(
    panel.background = element_rect(fill = "grey15", color = NA),
    plot.background = element_rect(fill = "grey15", color = NA),
    panel.grid.major = element_line(color = "grey35", linewidth = 0.4),
    panel.grid.minor = element_line(color = "grey25", linewidth = 0.2),
    text = element_text(color = "grey85"),
    axis.text = element_text(color = "grey85"),
    axis.title = element_text(color = "grey95"),
    plot.title = element_text(color = "white", face = "bold"),
    plot.subtitle = element_text(color = "grey85"),
    legend.position = "bottom"
  )

# 2. Your Helper Function: Univariate Distribution
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
    geom_text(aes(label = label), vjust = -0.5, size = 4, color = "grey85") +
    labs(
      title = title,
      subtitle = subtitle,
      x = xlab,
      y = "Count"
    ) +
    gem_theme + 
    scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
}

# 3. Your Helper Function: Intention by Category
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
    gem_theme
}

# 4. Helper: World Heatmap
# Note: 'data' must contain 'ctryalp', 'WEIGHT_L', and 'FUTSUPNO'
plot_global_heatmap <- function(data) {
  
  # Check if required columns exist
  if(!all(c("ctryalp", "WEIGHT_L", "FUTSUPNO") %in% names(data))) {
    stop("Data missing required columns for map: ctryalp, WEIGHT_L, or FUTSUPNO")
  }

  # Aggregation Logic
  country_intention_weighted <- data %>%
    filter(!is.na(WEIGHT_L)) %>%
    group_by(ctryalp) %>%
    summarise(
      proportion_yes = weighted.mean(FUTSUPNO == "Yes", w = WEIGHT_L, na.rm = TRUE),
      n_respondents = n()
    ) %>%
    ungroup()

  # Spatial Join
  world_map_sf <- ne_countries(scale = "medium", returnclass = "sf")
  world_map_with_data <- world_map_sf %>%
    left_join(country_intention_weighted, by = c("iso_a2" = "ctryalp"))

  # Plotting
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
      subtitle = "Estimated proportion of the 18-64 population intending to start a business.",
      caption = "Source: GEM 2021 APS. Weights (WEIGHT_L) applied."
    ) +
    theme_void() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 18, face = "bold", color = "black"), # Map usually looks better with standard colors or specific overrides
      plot.subtitle = element_text(hjust = 0.5, size = 12),
      legend.position = "bottom",
      legend.key.width = unit(2, "cm")
    )
}