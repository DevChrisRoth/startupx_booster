# ===================================================================
# --- helper_functions.R (COMPLETE REFACTORED SCRIPT) ---
# ===================================================================

# Required libraries for the functions below
library(ggplot2)
library(tidymodels)
library(vip)
library(dplyr)
library(scales)
library(stringr)
library(purrr)

plot_theme <- theme(
  panel.background = element_rect(fill = "grey15", color = NA),
  plot.background = element_rect(fill = "grey15", color = NA),
  panel.grid.major = element_line(color = "grey35", linewidth = 0.4),
  panel.grid.minor = element_line(color = "grey25", linewidth = 0.2),
  axis.line = element_line(color = "grey70", linewidth = 0.5),
  axis.ticks = element_line(color = "grey70"),
  axis.text = element_text(color = "grey85"),
  axis.title = element_text(color = "grey95"),
  plot.title = element_text(color = "white", face = "bold"),
  plot.subtitle = element_text(color = "grey85")
)


#' Evaluate and Report Performance on the Validation Set
#'
#' Takes a fitted workflow from the validation split and produces a comprehensive
#' summary, including plots and a detailed report.txt.
#'
#' @param validation_fit The output object from `last_fit()` on the validation split.
#' @param tune_results The output from `tune_grid()`.
#' @param best_params The data frame of best hyperparameters.
#' @param optimal_threshold The numeric decision threshold found from the validation set.
#' @param experiment_name A string name for the current experiment.
#' @param output_dir The directory where plots and reports are saved.
evaluate_and_report_validation <- function(validation_fit,
                                           tune_results,
                                           best_params,
                                           optimal_threshold,
                                           experiment_name,
                                           output_dir) {

  # --- 1. SETUP: PREPARE ALL METRICS AND CONFIG INFO ---
  core_metrics <- collect_metrics(validation_fit)
  predictions <- collect_predictions(validation_fit)
  
  predictions_reclassified <- predictions %>%
    mutate(.pred_class_optimal = factor(ifelse(.pred_Yes >= optimal_threshold, "Yes", "No"), levels = c("No", "Yes")))
  
  conf_matrix <- conf_mat(predictions_reclassified, truth = FUTSUPNO, estimate = .pred_class_optimal)

  tn <- conf_matrix$table[1, 1]; fn <- conf_matrix$table[1, 2]; fp <- conf_matrix$table[2, 1]; tp <- conf_matrix$table[2, 2]
  sensitivity <- tp / (tp + fn); specificity <- tn / (tn + fp); precision <- tp / (tp + fp); accuracy <- (tp + tn) / (tp + tn + fp + fn)

  recipe_summary <- extract_recipe(validation_fit) %>% summary()
  model_spec <- extract_spec_parsnip(validation_fit)
  model_type <- class(model_spec)[1]

  # --- 2. CONSOLE OUTPUT (VALIDATION SET) ---
  cat("===============================================================\n")
  cat("--- VALIDATION SET EVALUATION: ", experiment_name, " ---\n")
  cat("===============================================================\n\n")
  cat("--- Validation Set Performance Metrics ---\n")
  print(core_metrics)
  cat("\n")
  cat(paste0("--- Confusion Matrix (Validation Set) at Optimal Threshold: ", round(optimal_threshold, 3), " ---\n"))
  print(conf_matrix)
  cat("\n")
  cat("--- Key Performance Rates (Validation Set) ---\n")
  cat(sprintf("Sensitivity: %.3f  (How many of the true founders did we find?)\n", sensitivity))
  cat(sprintf("Specificity: %.3f  (How well did we avoid contacting non-founders?)\n", specificity))
  cat(sprintf("Precision  : %.3f  (When we predict 'founder', how often are we right?)\n", precision))
  cat(sprintf("Accuracy   : %.3f  (Overall, what proportion of students did we classify correctly?)\n\n", accuracy))

  # --- 3. PLOTTING (from Validation Set) ---
  roc_data <- predictions %>% roc_curve(FUTSUPNO, .pred_Yes, event_level = "second")
  
  roc_plot <- ggplot(roc_data, aes(x = 1 - specificity, y = sensitivity)) +
    geom_abline(linetype = "dashed", color = "grey70", linewidth = 0.8) +
    geom_line(color = "#00F2FF", linewidth = 1.2, alpha = 0.8) +
    labs(
      title = "ROC Curve (Validation Set)", 
      subtitle = paste("Validation Set AUC =", round(core_metrics$.estimate[core_metrics$.metric == "roc_auc"], 3)),
      x = "1 - Specificity",
      y = "Sensitivity"
    ) +
    coord_equal() + 
    plot_theme

  vip_plot <- validation_fit %>% 
    extract_fit_parsnip() %>% 
    vip(num_features = 20, geom = "col") +
    labs(title = "Top 20 Most Important Predictors") +
    theme_minimal() + 
    plot_theme

  print(roc_plot)
  print(vip_plot)

  ggsave(filename = file.path(output_dir, "roc_curve_validation.png"), plot = roc_plot, width = 8, height = 8)
  ggsave(filename = file.path(output_dir, "variable_importance_plot.png"), plot = vip_plot, width = 10, height = 7)
  cat(paste0("Plots saved to: ", output_dir, "\n"))

  # --- 4. REPORTING (from Validation Set) ---
  report_path <- file.path(output_dir, "report.txt")
  file_conn <- file(report_path, "w") # Create and write to the file
  
  write_line <- function(text, ...) { writeLines(sprintf(text, ...), file_conn) }
  
  write_line("===============================================================")
  write_line("                 EXPERIMENT REPORT: %s", experiment_name)
  write_line("===============================================================")
  write_line("Report generated on: %s", Sys.time())
  write_line("\n--- CONFIGURATION ---")
  write_line("Model Type: %s", model_type)
  write_line("Recipe Steps Included: %s", str_c(unique(recipe_summary$type), collapse = ", "))
  
  write_line("\n--- HYPERPARAMETER TUNING (on Training Set CV) ---")
  write_line("Tuning Metric: roc_auc")
  best_params_cleaned <- best_params %>% select(!starts_with("."))
  params_list <- purrr::map2_chr(names(best_params_cleaned), best_params_cleaned, ~ paste(.x, "=", if(is.numeric(.y)) round(.y, 4) else as.character(.y)))
  best_params_str <- paste(params_list, collapse = ", ")
  write_line("Best Hyperparameters Found: %s", best_params_str)
  
  write_line("\n--- VALIDATION SET PERFORMANCE ---")
  write_line("Optimal Decision Threshold Applied: %.4f", optimal_threshold)
  write_line("\n** Core Metrics (Threshold Independent) **")
  core_metrics_df <- data.frame(Metric = core_metrics$.metric, Estimate = round(core_metrics$.estimate, 4))
  writeLines(capture.output(print(core_metrics_df, row.names = FALSE)), file_conn)
  
  write_line("\n** Performance at Optimal Threshold (with use-case descriptions) **")
  write_line("Accuracy   : %.4f  (Overall, what proportion of students did we classify correctly?)", accuracy)
  write_line("Sensitivity: %.4f  (How many of the true founders did we find?)", sensitivity)
  write_line("Specificity: %.4f  (How well did we avoid contacting non-founders?)", specificity)
  write_line("Precision  : %.4f  (When we predict 'founder', how often are we right?)", precision)
  
  write_line("\n** Confusion Matrix (Validation Set) **")
  writeLines(capture.output(print(conf_matrix)), file_conn)
  
  close(file_conn)
  cat(paste0("Report saved to: ", report_path, "\n"))
}


#' Reports the final, unbiased performance on the held-out test set.
#'
#' This function appends the final test set results to an existing report.txt.
#'
#' @param test_predictions A data frame of predictions on the test set.
#' @param optimal_threshold The threshold determined from the validation set.
#' @param output_dir The directory where the report is saved.
report_final_test_performance <- function(test_predictions, optimal_threshold, output_dir) {
    
  # Calculate final metrics based on test predictions
  predictions_reclassified <- test_predictions %>%
    mutate(.pred_class_optimal = factor(ifelse(.pred_Yes >= optimal_threshold, "Yes", "No"), levels = c("No", "Yes")))
  
  conf_matrix <- conf_mat(predictions_reclassified, truth = FUTSUPNO, estimate = .pred_class_optimal)
  
  tn <- conf_matrix$table[1, 1]; fn <- conf_matrix$table[1, 2]; fp <- conf_matrix$table[2, 1]; tp <- conf_matrix$table[2, 2]
  sensitivity <- tp / (tp + fn); specificity <- tn / (tn + fp); precision <- tp / (tp + fp); accuracy <- (tp + tn) / (tp + tn + fp + fn)
  roc_auc <- roc_auc(test_predictions, truth = FUTSUPNO, .pred_Yes, event_level = "second")$.estimate
  
  # --- CONSOLE OUTPUT (TEST SET) ---
  cat("\n--- UNBIASED TEST SET PERFORMANCE ---\n")
  cat(paste0("--- Confusion Matrix (Test Set) at Threshold: ", round(optimal_threshold, 3), " ---\n"))
  print(conf_matrix)
  cat("\n--- Final Test Set Rates ---\n")
  cat(sprintf("Test Set AUC: %.4f\n", roc_auc))
  cat(sprintf("Sensitivity : %.3f\n", sensitivity))
  cat(sprintf("Specificity : %.3f\n", specificity))
  cat(sprintf("Precision   : %.3f\n", precision))
  cat(sprintf("Accuracy    : %.3f\n\n", accuracy))

  # --- APPEND TO REPORT ---
  report_path <- file.path(output_dir, "report.txt")
  file_conn <- file(report_path, "a") # Open in append mode
  
  write_line <- function(text, ...) { writeLines(sprintf(text, ...), file_conn) }
  
  write_line("\n\n===============================================================")
  write_line("--- FINAL UNBIASED PERFORMANCE ON HELD-OUT TEST SET ---")
  write_line("===============================================================")
  write_line("Threshold determined from validation set (%.4f) was applied.", optimal_threshold)
  write_line("\n** Final Test Set Metrics **")
  write_line("Test Set AUC : %.4f", roc_auc)
  write_line("Accuracy     : %.4f", accuracy)
  write_line("Sensitivity  : %.4f", sensitivity)
  write_line("Specificity  : %.4f", specificity)
  write_line("Precision    : %.4f", precision)
  write_line("\n** Final Test Set Confusion Matrix **")
  writeLines(capture.output(print(conf_matrix)), file_conn)
  
  close(file_conn)
  cat(paste0("Final test results appended to: ", report_path, "\n"))
}



#' Plot the Sensitivity vs. Specificity Tradeoff Curve
#'
#' This function takes prediction results, plots the tradeoff curve, and returns
#' the optimal crossover point.
#'
#' @param model_results The output from `last_fit()` or predictions data frame.
#' @param plot_title_suffix A string to append to the plot title (e.g., "(Validation Set)").
#' @return A data frame containing the optimal threshold.
plot_sensitivity_specificity_tradeoff <- function(model_results,
                                                  truth_col,
                                                  prob_col,
                                                  output_dir,
                                                  event_level = "second",
                                                  plot_title_suffix = "") {
  roc_data <- model_results %>%
    collect_predictions() %>%
    roc_curve(
      {{ truth_col }},
      {{ prob_col }},
      event_level = event_level
    )

  crossover_point <- roc_data %>%
    mutate(difference = abs(sensitivity - specificity)) %>%
    filter(difference == min(difference)) %>%
    arrange(desc(sensitivity)) %>%
    slice(1)

  tradeoff_data <- roc_data %>%
    select(.threshold, sensitivity, specificity) %>%
    pivot_longer(
      cols = c(sensitivity, specificity),
      names_to = "metric",
      values_to = "value"
    )

  tradeoff_plot <- ggplot(tradeoff_data, aes(x = .threshold, y = value, color = metric)) +
    geom_line(linewidth = 1.2, alpha = 0.8) +
    geom_vline(xintercept = crossover_point$.threshold, linetype = "dotted", color = "grey70") +
    geom_point(data = crossover_point, aes(x = .threshold, y = sensitivity), size = 4, color = "white") +
    geom_point(data = crossover_point, aes(x = .threshold, y = sensitivity), size = 2.5, color = "darkcyan") +
    geom_text(data = crossover_point, aes(x = .threshold, y = sensitivity, label = paste0("Optimal Threshold: ", round(.threshold, 2), "\nSens = ", round(sensitivity, 2), "\nSpec = ", round(specificity, 2))),
              hjust = -0.1, vjust = 0.5, nudge_x = 0.02, color = "white", size = 3.5) +
    geom_vline(xintercept = 0.5, linetype = "dashed", color = "firebrick") +
    annotate("text", x = 0.5, y = 0.1, label = "Default 0.5 Threshold", color = "firebrick", angle = 90, vjust = -0.5, size = 3.5) +
    scale_color_manual(values = c("sensitivity" = "darkcyan", "specificity" = "darkorange")) +
    labs(
      title = paste("Sensitivity-Specificity Tradeoff", plot_title_suffix),
      subtitle = "Shows model performance across all possible decision points",
      x = "Decision Threshold", y = "Rate", color = "Metric"
    ) +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
    theme_minimal() + plot_theme

  # Create a unique filename for the plot
  plot_filename <- paste0("sensitivity_specificity_tradeoff", str_replace_all(plot_title_suffix, "[^[:alnum:]]", ""), ".png")
  
  print(tradeoff_plot)
  ggsave(
    filename = file.path(output_dir, plot_filename),
    plot = tradeoff_plot, width = 9, height = 6, dpi = 300
  )
  
  return(crossover_point)
}