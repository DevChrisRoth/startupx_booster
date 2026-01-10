library(shiny)

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
source("scripts_production/00_utils_shap.R")

# --- 2. UI DEFINITION ---

# Helper for a styled "Next" button
nextButton <- function(id, label = "Weiter") {
  actionButton(id, label, class = "btn-primary", style = "margin-top: 20px; width: 100px;")
}
# Helper for a styled "Back" button
backButton <- function(id, label = "Zurück") {
  actionButton(id, label, class = "btn-default", style = "margin-top: 20px; margin-right: 10px; width: 100px;")
}

# Likert Levels (Matches Training Data Schema)
likert_levels <- c("Strongly Disagree", "Somewhat Disagree", "Neither", 
                   "Somewhat Agree", "Strongly Agree", "Refused_Answer", "Not_Applicable")

# Likert Choices for Sliders (Mapped to 1-5)
likert_map_val <- function(val) {
  if (val == 1) return("Strongly Disagree")
  if (val == 2) return("Somewhat Disagree")
  if (val == 3) return("Neither")
  if (val == 4) return("Somewhat Agree")
  if (val == 5) return("Strongly Agree")
  return("Neither")
  if (val == 5) return("Strongly Agree")
  return("Neither")
}

# Recommendation Logic
get_recommendations <- function(shap_res) {
  if (is.null(shap_res)) {
    return("Keine Daten für Empfehlungen verfügbar.")
  }
  
  # 1. shap_res is already a tidy data.frame from 00_utils_shap.R
  # Values: feature, shap_value, actual_value
  
  # Filter out non-numeric or special columns if any
  # Arrange by shap_value ascending (most negative first)
  lowest_var <- shap_res %>%
    arrange(shap_value) %>%
    slice(1) %>%
    pull(feature)
  
  # Dictionary of recommendations
  recs <- list(
    "FRFAILyy" = c(
      "1. Betrachte Fehler als wertvolle Lernchance.",
      "2. Starte mit kleinen, kalkulierbaren Risiken.",
      "3. Sprich mit erfahrenen Mentoren über ihre Rückschläge."
    ),
    "SUSKILyy" = c(
      "1. Nutze Online-Kurse zur gezielten Weiterbildung.",
      "2. Suche nach einem Mitgründer mit ergänzenden Skills.",
      "3. Übe das Pitchen deiner Idee vor Freunden."
    ),
    "OPPORTyy" = c(
      "1. Netzwerke aktiver in deiner lokalen Startup-Szene.",
      "2. Untersuche aktuelle Marktrends und Nischen.",
      "3. Brainstorme Lösungen für Alltagsprobleme, die dich stören."
    ),
    "KNOWENyy" = c(
      "1. Besuche Startup-Events und Meetups.",
      "2. Vernetze dich gezielt auf LinkedIn mit Gründern.",
      "3. Tritt Online-Gründer-Communities bei."
    ),
    "EASYSTyy" = c(
      "1. Informiere dich bei lokalen Behörden über Gründungsschritte.",
      "2. Such nach Vereinfachungen oder digitalen Gründungswegen.",
      "3. Hol dir Unterstützung bei der IHK oder Gründungszentren."
    ),
    "GEMHHINC" = c(
      "1. Erstelle einen soliden Finanzplan.",
      "2. Prüfe Möglichkeiten für Nebenberufliche Gründungen.",
      "3. Suche nach kostengünstigen Ressourcen (Bootstrapping)."
    ),
    "GEMEDUC" = c(
      "1. Nutze spezialisierte Weiterbildungen für Gründer.",
      "2. Suche Mentoren mit Branchenerfahrung.",
      "3. Lerne 'Learning by Doing' durch kleine Projekte."
    ),
    "cphhinc" = c(
      "1. Fokussiere auf kosteneffiziente Geschäftsmodelle.",
      "2. Nutze staatliche Hilfen für Gründer.",
      "3. Baue finanzielle Reserven langsam wieder auf."
    ),
    "OPPISMyy" = c(
      "1. Trainiere deinen Blick für positive Chancen.",
      "2. Lies Erfolgsgeschichten zur Inspiration.",
      "3. Umgib dich mit optimistischen Menschen."
    ),
    "PROACTyy" = c(
      "1. Setze dir täglich kleine, erreichbare Ziele.",
      "2. Etabliere eine 'Tu es jetzt'-Mentalität.",
      "3. Zerlege große Aufgaben in kleine Schritte."
    ),
    "CREATIVyy" = c(
      "1. Mache Brainstorming-Sessions im Team.",
      "2. Wechsle öfter die Perspektive bei Problemen.",
      "3. Nutze Kreativitätstechniken wie Design Thinking."
    ),
    "VISIONyy" = c(
      "1. Definiere deine langfristigen Ziele schriftlich.",
      "2. Erstelle ein Vision Board für dein Unternehmen.",
      "3. Reflektiere deine persönlichen Werte."
    ),
    "age" = c(
      "1. Nutze deine Lebenserfahrung als Stärke.",
      "2. Bleibe neugierig und offen für neue Technologien.",
      "3. Vernetze dich mit Gründern anderer Generationen."
    ),
    "gender" = c(
      "1. Nutze spezifische Förderprogramme (z.B. für Gründerinnen).",
      "2. Suche dir Vorbilder in deiner Peer-Group.",
      "3. Baue ein diverses Netzwerk auf."
    ),
    "hhsize" = c(
      "1. Organisiere dein Zeitmanagement strikt.",
      "2. Involviere Familie/Partner in deine Pläne.",
      "3. Schaffe dir feste Arbeitszeiten und -räume."
    ),
    "ctryalp" = c(
      "1. Untersuche das lokale Startup-Ökosystem genau.",
      "2. Vernetze dich international.",
      "3. Nutze lokale Vorteile und Förderungen."
    )
  )
  
  # Default recommendation if variable not in list
  default_rec <- c(
    "1. Überprüfe deinen Businessplan auf Schwachstellen.",
    "2. Hole dir allgemeines Feedback von potenziellen Kunden ein.",
    "3. Analysiere den Markt erneut auf Bedürfnisse."
  )
  
  selected_recs <- if (lowest_var %in% names(recs)) recs[[lowest_var]] else default_rec
  
  # Format output
  HTML(paste0(
    "<strong>Der stärkste negative Einflussfaktor war '", lowest_var, "'.</strong><br><br>",
    "Wir empfehlen dir folgende Schritte:<br>",
    "<ul>",
    paste0("<li>", selected_recs, "</li>", collapse = ""),
    "</ul>"
  ))
}



ui <- fluidPage(
  titlePanel("StartupX Booster"),
  
  # Custom minimal CSS for styling bars and layout
  tags$head(
    tags$style(HTML("
      /* Remove default shiny navbar padding/margin to look like a wizard */
      .navbar { min-height: 0; margin-bottom: 0; border: none; }
      .navbar-brand { display: none; } /* Hide default brand */
      
      /* Slide container */
      .slide-container {
        max-width: 800px;
        margin: 0 auto;
        padding: 20px;
        background: #f9f9f9;
        border-radius: 8px;
        box-shadow: 0 0 10px rgba(0,0,0,0.1);
        min-height: 600px;
      }
      
      /* Custom Slider styling if needed (minimal) */
      
      /* Bar chart colors are handled in plot generation, but we can style text here */
      .result-title {
        font-weight: bold;
        font-size: 1.5em;
        margin-bottom: 10px;
      }
    "))
  ),
  
  # Using hidden navbar for wizard steps
  div(class = "slide-container",
      tabsetPanel(
        id = "wizard",
        type = "hidden", # Hide the tabs at the top
        
        # --- SLIDE 1: DEMOGRAPHICS ---
        tabPanel("p1",
                 h2("Teil 1: Demografische Daten"),
                 p("Bitte gib uns ein paar Informationen zu deiner Person."),
                 hr(),
                 
                 # Inputs
                 selectInput(inputId= "ctryalp", label = "In welchem Land lebst du?", choices = list("Deutschland" = "Germany (DE)", "Spanien" = "Spain (ES)", "Polen" = "Poland (PL)", "Chile" = "Chile (CL)", "Frankreich" = "France (FR)",  "Saudi Arabien" = "Saudi Arabia (SA)", "Andere" = "Other")),
                 selectInput(inputId= "gender", label = "Was ist dein Geschlecht?", choices = list("Männlich" = "Male", "Weiblich" = "Female")),
                 numericInput(inputId= "age", label = "Wie alt bist du?", value = 21, min = 18, max = 64),
                 numericInput(inputId= "hhsize", label = "Wie viele Personen leben in deinem Haushalt (mit dir eingeschlossen):", value = 2, min = 1, max = 10),
                 
                 div(style = "text-align: right;", nextButton("btn_p1_next"))
        ),
        
        # --- SLIDE 2: PERCEPTIONS PART 1 ---
        tabPanel("p2",
                 h2("Teil 2: Deine Arbeitssituation und Haushaltseinkommen"),
                 p("Wie schätzt du dein Umfeld und deine Fähigkeiten ein? (1 = Trifft gar nicht zu, 5 = Trifft voll zu)"),
                 hr(),
                 
                 selectInput(inputId= "GEMOCCU", "Was beschreibt deinen aktuelle Arbeitssituation?", choices = list("Student" = "Student", "Vollzeit Beschäftigt" = "Full/Part-time Employee", "Teilzeit Beschäftigt" = "Part-time Employee Only", "Selbstständiger" = "Self-employed", "Arbeitslos" = "Not working", "Hausfrau/Hausmann" = "Homemaker", "Andere" = "Other")),
                 selectInput(inputId = "GEMHHINC", "In welche der folgenden Kategorien passt dein Haushaltseinkommen?", choices = list("Unteres Drittel" = "Lowest Third", "Mittleres Drittel" = "Middle Third", "Oberes Drittel" = "Upper Third", "Unbekannt" = "Unknown")),
                 selectInput(inputId = "GEMEDUC", "Was ist deine höchste schulische Bildung?", choices = list("Kein Abschluss" = "None", "Schule ohne Abschluss" = "Some Secondary", "Mittlere Reife / Abitur" = "Secondary Degree", "Berufsausbildung / Lehre" = "Post-Secondary", "Hochschulabschluss" = "Graduate Experience", "Andere" = "Other/Unknown")),

                 selectInput(inputId = "cphhinc", "Wie hat die Corona Pandemie dein Haushaltseinkommen im Jahr 2020 verringert?", 
                             choices = list("Stark verringert" = "Strongly Decrease", 
                                            "Etwas verringert" = "Somewhat Decrease", 
                                            "Keine Veränderung" = "No Change", 
                                            "Etwas erhöht" = "Somewhat Increase", 
                                            "Stark erhöht" = "Strongly Increase")),
                 div(style = "display: flex; justify-content: space-between;",
                     backButton("btn_p2_back"),
                     nextButton("btn_p2_next")
                 )
        ),
        
        # --- SLIDE 3: PERCEPTIONS PART 2 ---
        tabPanel("p3",
                 h2("Teil 3: Deine unternehmerische Wahrnehmung"),
                 p("Deine Einstellung zu Risiko und Erfolg."),
                 hr(),
                 
                 selectInput("KNOWENyy", "Kennst du jemanden, der in den letzten 2 Jahren ein Unternehmen gegründet hat?",
                             choices = c("Nein" = "None", "Ja" = "At least one")),
                 
                 selectInput("OPPORTyy", "In den nächsten sechs Monaten wird es in der Gegend, in der du lebst, gute Gelegenheiten geben, ein Unternehmen zu gründen.",
                             choices = c("Stimme nicht zu" = "Disagree", "Stimme zu" = "Agree", "Unbekannt" = "Unknown")),
                 
                 selectInput("SUSKILyy", "Ich habe das Wissen, die Fähigkeiten und die Erfahrung, um ein Unternehmen zu gründen.", 
                             choices = c("Stimme nicht zu" = "Disagree", "Stimme zu" = "Agree", "Unbekannt" = "Unknown")),
                 
                 selectInput("FRFAILyy", "Die Angst vor dem Scheitern würde mich davon abhalten, ein Unternehmen zu gründen.",
                             choices = c("Stimme nicht zu" = "Disagree", "Stimme zu" = "Agree")),

                selectInput("EASYSTyy", "Es ist leicht in meinem Land ein Unternehmen zu gründen.",
                             choices = c("Stimme nicht zu" = "Disagree", "Stimme zu" = "Agree", "Keine Angabe" = "Refused", "Unbekannt" = "Unknown")),
                 
                 div(style = "display: flex; justify-content: space-between;",
                     backButton("btn_p3_back"),
                     nextButton("btn_p3_next")
                 )
        ),
        
        # --- SLIDE 4: MINDSET ---
        tabPanel("p4",
                 h2("Teil 4: Deine Einstellung"),
                 p("Wie würdest du deine Persönlichkeit beschreiben?"),
                 hr(),
                 
                 # OPPISMyy
                 selectInput("OPPISMyy", "Selbst wenn ich mich in einem Bereich sehr gut auskenne, sehe ich selten Geschäftsmöglichkeiten.",
                             choices = c("Stimme gar nicht zu" = "Strongly Disagree", "Stimme weniger zu" = "Somewhat Disagree", "Weder noch" = "Neither", "Stimme zu" = "Somewhat Agree", "Stimme gar zu" = "Strongly Agree")),
                 
                 # PROACTyy
                 selectInput("PROACTyy", "Selbst wenn ich eine gewinnbringende Gelegenheit erkenne, werde ich selten aktiv.",
                             choices = c("Stimme gar nicht zu" = "Strongly Disagree", "Stimme weniger zu" = "Somewhat Disagree", "Weder noch" = "Neither", "Stimme zu" = "Somewhat Agree", "Stimme gar zu" = "Strongly Agree")),
                 
                 # CREATIVyy
                 selectInput("CREATIVyy", "Andere denken über mich, dass ich innovativ bin.",
                             choices = c("Stimme gar nicht zu" = "Strongly Disagree", "Stimme weniger zu" = "Somewhat Disagree", "Weder noch" = "Neither", "Stimme zu" = "Somewhat Agree", "Stimme gar zu" = "Strongly Agree")),
                 
                 # VISIONyy
                 selectInput("VISIONyy", "Jede Entscheidung, die ich treffe, ist Teil meines langfristigen Karriereplans.",
                             choices = c("Stimme gar nicht zu" = "Strongly Disagree", "Stimme weniger zu" = "Somewhat Disagree", "Weder noch" = "Neither", "Stimme zu" = "Somewhat Agree", "Stimme gar zu" = "Strongly Agree")),
                 
                 div(style = "display: flex; justify-content: space-between;",
                     backButton("btn_p4_back"),
                     actionButton("btn_finish", "Analyse Starten", class = "btn-success", style = "margin-top: 20px; width: 150px;")
                 )
        ),
        
        # --- SLIDE 5: RESULTS ---
        tabPanel("results",
                 h2("Dein Ergebnis", style = "text-align: center;"),
                 p("Basierend auf deinen Antworten haben wir folgende Schlüsselfaktoren identifiziert", style = "text-align: center; color: #666;"),
                 hr(),
                 
                 # Results Split Layout
                 fluidRow(
                   column(6, 
                          h4("Positive Faktoren (Booster)", style = "color: #2ca02c; text-align: center;"),
                          plotOutput("plot_positive", height = "300px")
                   ),
                   column(6,
                          h4("Negative Faktoren (Hemmnisse)", style = "color: #d62728; text-align: center;"),
                          plotOutput("plot_negative", height = "300px")
                   )
                 ),
                 
                 hr(),
                 h4("Handlungsempfehlungen"),
                 wellPanel(
                   htmlOutput("recommendation_text")
                 ),
                 
                 div(style = "text-align: center;",
                     actionButton("btn_restart", "Neu Starten", class = "btn-default")
                 )
        )
      )
  )
)

# --- 3. SERVER LOGIC ---

server <- function(input, output, session) {
  
  # Navigation Logic
  observeEvent(input$btn_p1_next, { updateTabsetPanel(session, "wizard", selected = "p2") })
  observeEvent(input$btn_p2_back, { updateTabsetPanel(session, "wizard", selected = "p1") })
  observeEvent(input$btn_p2_next, { updateTabsetPanel(session, "wizard", selected = "p3") })
  observeEvent(input$btn_p3_back, { updateTabsetPanel(session, "wizard", selected = "p2") })
  observeEvent(input$btn_p3_next, { updateTabsetPanel(session, "wizard", selected = "p4") })
  observeEvent(input$btn_p4_back, { updateTabsetPanel(session, "wizard", selected = "p3") })
  
  observeEvent(input$btn_finish, { 
    updateTabsetPanel(session, "wizard", selected = "results") 
  })
  
  observeEvent(input$btn_restart, {
    updateTabsetPanel(session, "wizard", selected = "p1")
    # Optional: Reset inputs manually if needed, but Shiny retains them by default which might be desired behavior
  })
  
  # Reactive Data Construction
  # Reactive Data Construction
  # Values from inputs now match the model factors directly thanks to UI choices.

  
  current_data <- reactive({
    data.frame(
      # Demographics
      WBINC = factor("High", levels = c("Low", "Lower Middle", "Upper Middle", "High")),
      gender = factor(input$gender, levels = c("Male", "Female")),
      age = as.numeric(input$age),
      hhsize = as.numeric(input$hhsize),
      GEMHHINC = factor(input$GEMHHINC, levels = c("Lowest Third", "Middle Third", "Upper Third", "Unknown")),
      GEMEDUC = factor(input$GEMEDUC, levels = c("None", "Some Secondary", "Secondary Degree", "Post-Secondary", "Graduate Experience", "Other/Unknown")),
      cphhinc = factor(input$cphhinc, levels = c("Strongly Decrease", "Somewhat Decrease", "No Change", "Somewhat Increase", "Strongly Increase"), ordered = TRUE),
      
      # Perceptions
      KNOWENyy = factor(input$KNOWENyy, levels = c("None", "At least one")),
      OPPORTyy = factor(input$OPPORTyy, levels = c("Disagree", "Agree", "Unknown")),
      SUSKILyy = factor(input$SUSKILyy, levels = c("Disagree", "Agree", "Unknown")),
      FRFAILyy = factor(input$FRFAILyy, levels = c("Disagree", "Agree")),
      EASYSTyy = factor(input$EASYSTyy, levels = c("Disagree", "Agree", "Refused", "Unknown")),
      
      # Mindset
      OPPISMyy = factor(input$OPPISMyy, levels = likert_levels, ordered = TRUE),
      PROACTyy = factor(input$PROACTyy, levels = likert_levels, ordered = TRUE),
      CREATIVyy = factor(input$CREATIVyy, levels = likert_levels, ordered = TRUE),
      VISIONyy = factor(input$VISIONyy, levels = likert_levels, ordered = TRUE),
      
      # Engineering Flags
      Mindset_Asked = factor("Asked", levels = c("Asked", "Not_Asked")),
      age_is_missing = factor("No", levels = c("No", "Yes"))
    )
  })
  
  # --- Model Calculation ---
  shap_results <- eventReactive(input$btn_finish, {
    req(final_model)
    # Compute SHAP
    # We use compute_single_shap from utils
    tryCatch({
      res <- compute_single_shap(final_model, shap_bg, current_data())
      res
    }, error = function(e) {
      showNotification(paste("Error in model:", e$message), type = "error")
      return(NULL)
    })
  })
  
  # --- Plotting ---
  
  output$plot_positive <- renderPlot({
    validate(need(shap_results(), "Die Analyse wird gestartet... Bitte warten."))
    
    # Extract contribution table from shap object
    # The utils script likely returns a specific format. 
    # Based on app.R: plots <- plot_shap_contribution(shap_res, top_n = 5)
    # We can rely on that existing function or build custom ggplot if layout differs.
    # The user asked for "3 strongest factors".
    
    # Let's use the utility function to get formatted data if possible, or do it manually
    # Assuming plot_shap_contribution returns a list(positive = gg, negative = gg)
    
    plots <- plot_shap_contribution(shap_results(), top_n = 3)
    plots$positive + 
      theme_minimal() + 
      theme(axis.text.y = element_text(size = 12),
            plot.title = element_blank()) # Remove title as we have HTML headers
  })
  
  output$plot_negative <- renderPlot({
    validate(need(shap_results(), "Die Analyse wird gestartet... Bitte warten."))
    
    plots <- plot_shap_contribution(shap_results(), top_n = 3)
    plots$negative + 
      theme_minimal() + 
      theme(axis.text.y = element_text(size = 12),
            plot.title = element_blank())
  })
  
  # --- Recommendations ---
  output$recommendation_text <- renderUI({
    req(shap_results())
    get_recommendations(shap_results())
  })
  
}

shinyApp(ui, server)
