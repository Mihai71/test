library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)
library(ggplot2)
library(reticulate)

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
    info <- profile_data(input$file$datapath)
    
    # Validare structurala (FR-01)
    if (!is.null(info$error)) {
      showNotification(info$error, type = "error", duration = 10)
      validate(need(is.null(info$error), info$error))
    }
    
    return(info)
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
    validate(
      need(input$sensitive!= "", "Vă rugăm să selectați un atribut sensibil."),
      need(input$target!= "", "Vă rugăm să selectați o variabilă target.")
    )
    
    df <- data_final()
    info <- data_info()
    target_type <- info$types[[input$target]]
    
    cat("Analiză efectuată pe:", input$target, "raportat la", input$sensitive, "\n")
    cat("Tip țintă detectat:", target_type, "\n")
    cat("------------------------------------------------------------\n\n")
    
    if (target_type == "Numerică") {
      # --- LOGICĂ PENTRU VALORI NUMERICE (COHEN'S D) ---
      res_stats <- df %>%
        group_by(.data[[input$sensitive]]) %>%
        summarise(Media = mean(.data[[input$target]], na.rm = TRUE),
                  SD = sd(.data[[input$target]], na.rm = TRUE),
                  N = n())
      print(res_stats)
      
      if (nrow(res_stats) == 2 && all(res_stats$N > 1)) {
        m1 <- res_stats$Media[1]; m2 <- res_stats$Media[2]
        s1 <- res_stats$SD[1]; s2 <- res_stats$SD[2]
        n1 <- res_stats$N[1]; n2 <- res_stats$N[2]
        s_pooled <- sqrt(((n1 - 1) * s1^2 + (n2 - 1) * s2^2) / (n1 + n2 - 2))
        cohen_d <- abs(m1 - m2) / s_pooled
        cat("\n>>> Mărimea efectului (Cohen's d):", round(cohen_d, 4), "\n")
        interpretare <- if(cohen_d < 0.2) "Neglijabil" else if(cohen_d < 0.5) "Mic" else "Ridicat"
        cat("Interpretare:", interpretare, "\n")
      } else if (nrow(res_stats) > 2) {
        cat("\n[Notă] Cohen's d se calculează standard între 2 grupuri. Pentru mai multe grupuri (ex: Vârstă), analizați diferențele mediilor în tabelul de mai sus.")
      }
      
    } else {
      # --- LOGICĂ PENTRU BINARĂ / CATEGORICĂ (SPD & PARITY) ---
      # Identificăm valoarea de succes (ex: "Angajat", "Da", sau 1)
      vals <- sort(unique(df[[input$target]]))
      success_label <- vals[length(vals)] # Luăm ultima valoare alfabetic drept succes
      
      res_parity <- df %>%
        group_by(.data[[input$sensitive]]) %>%
        summarise(
          Total = n(),
          Succese = sum(.data[[input$target]] == success_label, na.rm = TRUE),
          Rata_Succes = round(Succese / Total, 4)
        )
      
      cat("Frecvențe și Proporții (Cazul de succes: '", as.character(success_label), "')\n", sep="")
      print(res_parity)
      
      if (nrow(res_parity) == 2) {
        p1 <- res_parity$Rata_Succes[1]
        p2 <- res_parity$Rata_Succes[2]
        
        spd <- p1 - p2
        di <- if(p2!= 0) p1 / p2 else NA
        
        cat("\n>>> Statistical Parity Difference (SPD):", round(spd, 4), "\n")
        cat(">>> Disparate Impact (DI):", round(di, 4), "\n")
        
        if (!is.na(di)) {
          # Regula de 80% (standard internațional conform Das et al. 2021)
          interpretare <- if(di >= 0.8 && di <= 1.25) "Echitabil (Bias Neglijabil)" else "Risc ridicat de discriminare"
          cat("Interpretare (Regula 80%):", interpretare, "\n")
        }
      } else {
        cat("\n[Notă] Analiza automată SPD/DI este optimă pentru 2 grupuri (ex: Gen).")
        cat("\nPentru atribute multiple (ex: Vârstă), comparați ratele de succes din tabelul de mai sus.")
      }
    }
  })
  
  output$plot <- renderPlot({
    req(data_final(), input$sensitive, input$target)
    df_plot <- data_final()
    info <- data_info()
    
    if(info$types[[input$target]] == "Numerică") {
      ggplot(df_plot, aes(x =.data[[input$sensitive]], y =.data[[input$target]], fill =.data[[input$sensitive]])) +
        geom_boxplot() + theme_minimal() + labs(title = "Distribuția Valorilor Numerice")
    } else {
      # Pentru variabile binare, folosim un bar chart cu proporții (sugestiv pentru Parity)
      ggplot(df_plot, aes(x =.data[[input$sensitive]], fill = as.factor(.data[[input$target]]))) +
        geom_bar(position = "fill") + theme_minimal() +
        labs(y = "Proporție", fill = input$target, title = "Proporția Rezultatelor pe Grupuri")
    }
  })
}

shinyApp(ui, server)