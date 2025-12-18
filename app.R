required_files <- c(
  "output_production/final_model.rds",
  "output_production/final_threshold.rds",
  "output_production/shap_background.rds"
)

missing_files <- required_files[!file.exists(required_files)]

if (length(missing_files) > 0) {
  # Create a clean error message
  msg <- paste0(
    "\n\n====================================================================\n",
    "CRITICAL ERROR: Missing Production Artifacts\n",
    "====================================================================\n",
    "The following required files are missing from 'output_production/':\n",
    paste0(" - ", missing_files, collapse = "\n"),
    "\n\n",
    "ACTION REQUIRED:\n",
    "Please run the training script to generate these files before starting the GUI.\n",
    "Run: source('scripts_production/02_train_final_model.R')\n",
    "====================================================================\n"
  )
  
  # Stop execution so the app doesn't crash halfway through
  stop(msg)
}

cat("--- Pre-flight check passed: All model artifacts found. ---\n")

# 1. LOAD (Once at startup)
final_model <- readRDS("output_production/final_model.rds")
shap_bg     <- readRDS("output_production/shap_background.rds") 

# 2. REACTIVE EVENT (User clicks predict)
observeEvent(input$predict_btn, {
    
    # Create tuple from inputs
    new_data <- data.frame(...) 
    
    # Compute SHAP (Fast! No re-baking background)
    shap_res <- compute_single_shap(final_model, shap_bg, new_data)
    
    # Plot
    output$shapPlot <- renderPlot({
        plot_shap_contribution(shap_res)
    })
})