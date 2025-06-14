# ===================================================================
# --- 3. Explorative Data Analysis ---
# ===================================================================


# ---------- Copied over from preprocessing.R ------------
library(dplyr)
library(data.table)

setwd("C:\\Users\\timor\\Desktop\\Hochschule\\Machine_Learning")
gem_data <- fread("data/GEM2021APSGlobalIndividualLevelData_15Feb2023.csv")


model_data <- gem_data %>% filter(!is.na(FUTSUPNO))
# ------------ end of preprocessing.R ------------------------


nrow(model_data)

par(mar = c(7, 7, 7, 7))  # bottom, left, top, right


# Find out Target Variable Imbalances
freq <- table(model_data$FUTSUPNO)
labels <- paste(names(freq), " (", round(100 * freq/sum(freq), 1), "%)", sep = "")

# Plot bar chart with annotations
barplot(freq,
        main = "Expecting to start a new business (18-64 and not currently involved)", 
        xlab = "Class (0 = NO, 1 = YES)",
        ylab = "Frequency",
        col = "steelblue",
        names.arg = labels,
        cex.names = 2,
        cex.main = 2,
        cex.lab = 2,
        cex.axis = 2,
        space = 0.5)
# 81.2% for No, 18.8% for Yes ---- Imbalance in Target Variable, consider Undersampling

