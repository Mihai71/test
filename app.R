library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)
library(ggplot2)
library(reticulate)

# ============================================================
# PASUL 1: Import Resurse (Standarde R și Logică Python)
# ============================================================
source("R/standards.R")
source_python("logic.py")

ui <- dashboardPage(
  dashboardHeader(title = "Bias Detection Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Încărcare date", tabName = "data", icon = icon("upload")),
      menuItem("Analiză bias", tabName = "bias", icon = icon("balance-scale")),
      menuItem("Vizualizare", tabName = "viz", icon = icon("chart-bar")),
      menuItem("Export", tabName = "export", icon = icon("download"))
    ),
    fileInput("file", "Încarcă set de date (CSV)", accept = c(".csv")),
    selectInput("sensitive", "Atribut sensibil", choices = NULL),
    selectInput("target", "Variabilă analizată", choices = NULL),
    actionButton("run", "Rulează analiza", icon = icon("play"))
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "data", 
              h2("Datele încărcate"), 
              uiOutput("data_summary"), 
              DTOutput("table")),
      tabItem(tabName = "bias", 
              h2("Metrici de bias (FR-04, FR-06)"), 
              verbatimTextOutput("metrics")),
      tabItem(tabName = "viz", 
              h2("Vizualizare Distribuții (FR-07)"), 
              plotOutput("plot")),
      tabItem(tabName = "export", 
              h2("Export rezultate"), 
              p("Funcționalitate în curs de implementare."))
    )
  )
)

server <- function(input, output, session) {
  
  data_raw <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })
  
  data_info <- reactive({
    req(input$file)
    profile_data(input$file$datapath)
  })
  
  data_final <- reactive({
    req(data_raw())
    df <- data_raw()
    age_col <- names(df)[str_detect(tolower(names(df)), "v[âa]rst|age")]
    
    if (length(age_col) > 0) {
      df <- df %>%
        mutate(across(all_of(age_col), ~ cut(., 
                                             breaks = age_bins, 
                                             labels = age_labels, 
                                             include.lowest = TRUE)))
    }
    return(df)
  })
  
  observe({
    req(data_info())
    info <- data_info()
    updateSelectInput(session, "sensitive", 
                      choices = info$columns, 
                      selected = if(length(info$sensitive_candidates) > 0) info$sensitive_candidates[1] else NULL)
    
    num_cols <- names(info$types[info$types == "Numerică"])
    best_target <- if(length(info$financial_candidates) > 0) info$financial_candidates[1] else num_cols[1]
    
    updateSelectInput(session, "target", 
                      choices = info$columns, 
                      selected = best_target)
  })
  
  output$data_summary <- renderUI({
    req(data_info())
    info <- data_info()
    missing_text <- map_chr(names(info$missing), ~ {
      if(info$missing[[.x]] > 0) paste0(.x, ": ", info$missing[[.x]], " lipsă") else ""
    }) %>% keep(~.x!= "")
    
    if(length(missing_text) > 0) {
      box(title = "Alerte Calitate Date (Valori Lipsă)", status = "warning", solidHeader = TRUE, width = 12,
          p("Sistemul a detectat celule goale:"),
          tags$ul(lapply(missing_text, tags$li))
      )
    }
  })
  
  output$table <- renderDT({
    req(data_final())
    datatable(data_final(), options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$metrics <- renderPrint({
    req(input$run)
    # Validare: asigurăm că inputurile sunt selectate corect înainte de calcul
    validate(
      need(input$sensitive!= "", "Vă rugăm să selectați un atribut sensibil."),
      need(input$target!= "", "Vă rugăm să selectați o variabilă target.")
    )
    
    df <- data_final()
    info <- data_info()
    target_type <- info$types[[input$target]]
    
    cat("Analiză efectuată pe:", input$target, "raportat la", input$sensitive, "\n")
    cat("------------------------------------------------------------\n\n")
    
    if (target_type == "Numerică") {
      res_stats <- df %>%
        group_by(.data[[input$sensitive]]) %>%
        summarise(Media = mean(.data[[input$target]], na.rm = TRUE),
                  SD = sd(.data[[input$target]], na.rm = TRUE),
                  N = n())
      print(res_stats)
      
      if (nrow(res_stats) == 2) {
        # Verificăm dacă ambele grupuri au mai mult de 1 element pentru SD valid
        if (all(res_stats$N > 1)) {
          m1 <- res_stats$Media[1]; m2 <- res_stats$Media[2]
          s1 <- res_stats$SD[1]; s2 <- res_stats$SD[2]
          n1 <- res_stats$N[1]; n2 <- res_stats$N[2]
          
          s_pooled <- sqrt(((n1 - 1) * s1^2 + (n2 - 1) * s2^2) / (n1 + n2 - 2))
          cohen_d <- abs(m1 - m2) / s_pooled
          
          cat("\n>>> Mărimea efectului (Cohen's d):", round(cohen_d, 4), "\n")
          
          # Fix pentru eroarea de TRUE/FALSE: verificăm dacă cohen_d nu este NA
          if (!is.na(cohen_d)) {
            interpretare <- if(cohen_d < 0.2) "Neglijabil" else if(cohen_d < 0.5) "Mic" else "Ridicat"
            cat("Interpretare:", interpretare, "\n")
          }
        } else {
          cat("\n[Atenție] Unul dintre grupuri are un singur eșantion. Cohen's d nu poate fi calculat.\n")
        }
      }
    }
    # (Logica pentru Binară rămâne neschimbată)
  })
  
  output$plot <- renderPlot({
    req(data_final(), input$sensitive, input$target)
    ggplot(data_final(), aes_string(x = input$sensitive, y = input$target, fill = input$sensitive)) +
      geom_boxplot() + theme_minimal()
  })
}

shinyApp(ui, server)