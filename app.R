# app.R - Dashboard Interactiv pentru Detectarea Disparităților
# Arhitectură: R Shiny (UI + server reactiv) + Python via reticulate (procesare date)
# Cerințe implementate: FR-01 .. FR-07
# reticulate::use_python("C:/Users/pelle/AppData/Local/Programs/Python/Python312/python.exe", required = TRUE)
library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(reticulate)

# Plotly pentru grafice interactive – FR-07
if (requireNamespace("plotly", quietly = TRUE)) library(plotly)

# readxl pentru suport Excel – FR-01
if (requireNamespace("readxl", quietly = TRUE)) library(readxl)

source("R/standards.R")
source_python("logic.py")

# ---------------------------------------------------------------------------
# Helpere R
# ---------------------------------------------------------------------------

has_plotly <- requireNamespace("plotly", quietly = TRUE)

chart_output <- function(id) {
  if (has_plotly) plotly::plotlyOutput(id) else plotOutput(id)
}

py_to_r_safe <- function(x) {
  tryCatch(reticulate::py_to_r(x), error = function(e) x)
}

fmt_p <- function(p) {
  if (is.na(p) || is.null(p)) return("N/A")
  p <- as.numeric(p)
  if (p < 0.001) "< 0.001" else round(p, 4)
}

bias_color <- function(score) {
  score <- as.numeric(score)
  if (score < 0.20) "#27ae60" else if (score < 0.50) "#f39c12" else "#e74c3c"
}

bias_label <- function(score) {
  score <- as.numeric(score)
  if (score < 0.20) "Neglijabil" else if (score < 0.50) "Moderat" else "Ridicat"
}

# ---------------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------------

ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(title = "Bias Detection Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Date",             tabName = "tab_data",   icon = icon("table")),
      menuItem("Analiză Generală", tabName = "tab_bias",   icon = icon("balance-scale")),
      menuItem("Socio-Demografic", tabName = "tab_socio",  icon = icon("users")),
      menuItem("Vizualizare",      tabName = "tab_viz",    icon = icon("chart-bar")),
      menuItem("Export",           tabName = "tab_export", icon = icon("download"))
    ),
    tags$hr(),
    fileInput("file", "Încarcă fișier (CSV / Excel)",
              accept = c(".csv", ".xlsx", ".xls"),
              buttonLabel = "Alege fișier",
              placeholder = "Niciun fișier selectat"),
    
    selectInput("sensitive", "Atribut sensibil", choices = NULL),
    selectInput("target",    "Variabilă analizată (target)", choices = NULL),
    
    tags$details(
      tags$summary(style = "color:#aaa; cursor:pointer; font-size:12px;",
                   "Setare manuală tip coloană"),
      selectInput("override_col",  "Coloana", choices = NULL),
      selectInput("override_type", "Tip nou",
                  choices = c("Numerică"="Numerica","Binară"="Binara","Categorică"="Categorica")),
      actionButton("apply_override", "Aplică",
                   class = "btn-xs btn-warning", icon = icon("edit"))
    ),
    
    tags$hr(),
    div(style = "padding: 0 15px 10px 15px;",
        actionButton("run", "Rulează analiza", icon = icon("play"),
                     class = "btn-primary btn-block", style = "width: 100%;")
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .bias-gauge { font-size:2.5em; font-weight:bold; text-align:center;
                      padding:10px; border-radius:8px; }
        .alert-box  { border-left:5px solid; padding:10px; margin:6px 0;
                      border-radius:4px; }
        .alert-red    { border-color:#e74c3c; background:#fdf3f3; }
        .alert-orange { border-color:#f39c12; background:#fef9f0; }
        .alert-green  { border-color:#27ae60; background:#f0faf4; }
        .metric-table th { background:#2980b9; color:white; }
        .info-box .info-box-icon { font-size:28px; }
        .sidebar-form .btn-block, .main-sidebar .btn-block {
          margin-left:0 !important; margin-right:0 !important; }
        .table-toolbar { display:flex; justify-content:flex-end;
                         gap:6px; margin-bottom:8px; }
        .preproc-sep   { border-top:1px solid #eee; margin:8px 0; }
      "))
    ),
    
    tabItems(
      
      # -----------------------------------------------------------------------
      # TAB DATE
      # -----------------------------------------------------------------------
      tabItem(tabName = "tab_data",
              
              fluidRow(
                box(title = "Sumar fișier", status = "primary", solidHeader = TRUE, width = 12,
                    uiOutput("ui_file_summary")
                )
              ),
              
              # --- PREPROCESARE ---
              fluidRow(
                box(
                  title = tagList(icon("tools"), " Preprocesare Date"),
                  status = "warning", solidHeader = TRUE, width = 12,
                  
                  fluidRow(
                    
                    # Coloana 1 – curățare
                    column(4,
                           tags$b(icon("eraser"), " Curățare"),
                           tags$br(), tags$br(),
                           checkboxInput("remove_na",
                                         label = "Elimină rândurile cu valori lipsă (NA)",
                                         value = FALSE),
                           tags$div(class = "preproc-sep"),
                           uiOutput("ui_dup_info"),
                           checkboxInput("remove_duplicates",
                                         label = "Elimină rândurile duplicate",
                                         value = FALSE)
                    ),
                    
                    # Coloana 2 – filtrare
                    column(5,
                           tags$b(icon("filter"), " Filtrare suplimentară"),
                           tags$br(), tags$br(),
                           selectInput("filter_col",
                                       label    = "Selectează coloana de filtrare:",
                                       choices  = c("(fără filtru)" = ""),
                                       width    = "100%"),
                           uiOutput("ui_filter_value")
                    ),
                    
                    # Coloana 3 – status
                    column(3,
                           tags$b(icon("info-circle"), " Status date"),
                           tags$br(), tags$br(),
                           uiOutput("ui_preprocess_status")
                    )
                  )
                )
              ),
              
              fluidRow(
                box(title = "Tipurile detectate per coloană (FR-01)",
                    status = "info", solidHeader = TRUE, width = 6,
                    DTOutput("tbl_col_types")
                ),
                box(title = "Alerte calitate date – Valori lipsă (FR-05)",
                    status = "warning", solidHeader = TRUE, width = 6,
                    uiOutput("ui_missing_alerts")
                )
              ),
              
              fluidRow(
                box(title = "Previzualizare și editare date",
                    status = "primary", solidHeader = TRUE, width = 12,
                    # toolbar
                    div(class = "table-toolbar",
                        downloadButton("dl_data_csv",
                                       label = tagList(icon("download"), " Descarcă CSV"),
                                       class = "btn-sm btn-info"),
                        actionButton("save_edits",
                                     label = tagList(icon("save"), " Salvează modificările"),
                                     class = "btn-sm btn-success")
                    ),
                    DTOutput("tbl_data_preview")
                )
              )
      ),
      
      # -----------------------------------------------------------------------
      # TAB ANALIZĂ GENERALĂ
      # -----------------------------------------------------------------------
      tabItem(tabName = "tab_bias",
              fluidRow(
                box(title = "Bias Score (FR-06)",
                    status = "primary", solidHeader = TRUE, width = 4,
                    uiOutput("ui_bias_score")
                ),
                box(title = "Alerte Distribuționale (FR-05)",
                    status = "warning", solidHeader = TRUE, width = 8,
                    uiOutput("ui_dist_alerts")
                )
              ),
              fluidRow(
                box(title = "Metrici de Disparitate (FR-04)",
                    status = "info", solidHeader = TRUE, width = 12,
                    uiOutput("ui_metrics_detail")
                )
              ),
              fluidRow(
                box(title = "Tabel sumar pe grupuri",
                    status = "primary", solidHeader = TRUE, width = 12,
                    div(class = "table-toolbar",
                        downloadButton("dl_group_csv",
                                       label = tagList(icon("download"), " Descarcă CSV"),
                                       class = "btn-sm btn-info")
                    ),
                    DTOutput("tbl_group_summary")
                )
              )
      ),
      
      # -----------------------------------------------------------------------
      # TAB SOCIO-DEMOGRAFIC
      # -----------------------------------------------------------------------
      tabItem(tabName = "tab_socio",
              fluidRow(
                box(title = "Configurare Analiză Socio-Demografică",
                    status = "primary", solidHeader = TRUE, width = 12,
                    column(4,
                           selectInput("socio_type", "Tip analiză",
                                       choices = c("Vârstă (grupare standard)"  = "age",
                                                   "Educație (clasificare ISCED)" = "edu",
                                                   "Regiune (NUTS România)"       = "nuts"))
                    ),
                    column(4,
                           selectInput("socio_target_col", "Indicator financiar", choices = NULL)
                    ),
                    column(4,
                           selectInput("socio_ref_country", "Compară cu:",
                                       choices = c("Media României"  = "RO",
                                                   "Media UE"        = "EU",
                                                   "Germania"        = "DE",
                                                   "Franța"          = "FR",
                                                   "Ungaria"         = "HU",
                                                   "Bulgaria"        = "BG",
                                                   "Fără comparație" = "NONE")),
                           actionButton("run_socio", "Analizează",
                                        icon = icon("search"), class = "btn-success")
                    )
                )
              ),
              fluidRow(
                box(title = "Distribuția pe grupuri standardizate",
                    status = "info", solidHeader = TRUE, width = 8,
                    chart_output("plot_socio_dist")
                ),
                box(title = "Comparație cu referința selectată",
                    status = "warning", solidHeader = TRUE, width = 4,
                    uiOutput("ui_socio_comparison")
                )
              ),
              fluidRow(
                box(title = "Tabel detaliat – Analiză Socio-Demografică",
                    status = "primary", solidHeader = TRUE, width = 12,
                    div(class = "table-toolbar",
                        downloadButton("dl_socio_csv",
                                       label = tagList(icon("download"), " Descarcă CSV"),
                                       class = "btn-sm btn-info")
                    ),
                    DTOutput("tbl_socio_summary")
                )
              )
      ),
      
      # -----------------------------------------------------------------------
      # TAB VIZUALIZARE
      # -----------------------------------------------------------------------
      tabItem(tabName = "tab_viz",
              fluidRow(
                tabBox(title = "Grafice", width = 12,
                       tabPanel("Boxplot",
                                p("Distribuția valorilor numerice pe grupuri (FR-07)"),
                                chart_output("plot_boxplot")
                       ),
                       tabPanel("Density Plot",
                                p("Suprapunerea distribuțiilor per grup (FR-07)"),
                                chart_output("plot_density")
                       ),
                       tabPanel("Barplot Diferențe",
                                p("Diferențele mediei față de media globală (FR-07)"),
                                chart_output("plot_barplot")
                       ),
                       tabPanel("Proporții (target binar)",
                                p("Proporția outcome-ului pozitiv pe grupuri (FR-07)"),
                                chart_output("plot_parity")
                       )
                )
              )
      ),
      
      # -----------------------------------------------------------------------
      # TAB EXPORT
      # -----------------------------------------------------------------------
      tabItem(tabName = "tab_export",
              fluidRow(
                box(title = "Export rezultate",
                    status = "primary", solidHeader = TRUE, width = 12,
                    p("Descarcă graficele și raportul de analiză."),
                    tags$br(),
                    fluidRow(
                      column(3, downloadButton("dl_boxplot", "Boxplot (PNG)",
                                               class = "btn-info btn-block")),
                      column(3, downloadButton("dl_density", "Density Plot (PNG)",
                                               class = "btn-info btn-block")),
                      column(3, downloadButton("dl_barplot", "Barplot (PNG)",
                                               class = "btn-info btn-block")),
                      column(3, downloadButton("dl_report",  "Raport CSV",
                                               class = "btn-success btn-block"))
                    ),
                    tags$br(),
                    uiOutput("ui_export_preview")
                )
              )
      )
    )
  )
)


# ---------------------------------------------------------------------------
# SERVER
# ---------------------------------------------------------------------------

server <- function(input, output, session) {
  
  manual_types <- reactiveVal(list())
  
  # -------------------------------------------------------------------------
  # Citire date brute + profil
  # -------------------------------------------------------------------------
  
  data_info <- reactive({
    req(input$file)
    info <- profile_data(input$file$datapath)
    info <- py_to_r_safe(info)
    
    if (!is.null(info$error)) {
      showNotification(info$error, type = "error", duration = 15)
      validate(need(FALSE, info$error))
    }
    
    ov <- manual_types()
    for (col in names(ov)) info$types[[col]] <- ov[[col]]
    
    info
  })
  
  data_raw <- reactive({
    req(input$file)
    fp <- input$file$datapath
    if (grepl("\\.xlsx?$", input$file$name, ignore.case = TRUE)) {
      if (requireNamespace("readxl", quietly = TRUE)) {
        readxl::read_excel(fp)
      } else {
        showNotification("Instalați pachetul readxl pentru suport Excel.", type = "warning")
        read.csv(fp)
      }
    } else {
      read.csv(fp)
    }
  })
  
  # -------------------------------------------------------------------------
  # Date de lucru (editabile de utilizator)
  # -------------------------------------------------------------------------
  
  # Stochează datele brute + editările utilizatorului
  data_working <- reactiveVal(NULL)
  
  # Resetare la încărcarea unui fișier nou
  # DUPĂ
  observeEvent(data_raw(), {
    df <- as.data.frame(data_raw())
    df[] <- lapply(df, function(x) {
      if (is.character(x)) x[trimws(x) == ""] <- NA
      x
    })
    data_working(df)
  }, ignoreNULL = TRUE)
  
  # Proxy DT pentru actualizări fără re-render complet
  dt_proxy <- DT::dataTableProxy("tbl_data_preview")
  
  # -------------------------------------------------------------------------
  # Date procesate (filtrare reactivă non-destructivă)
  # -------------------------------------------------------------------------
  
  data_processed <- reactive({
    req(data_working())
    df        <- data_working()
    keep_rows <- seq_len(nrow(df))
    
    # 1. Elimină rânduri cu NA
    if (isTRUE(input$remove_na)) {
      keep_rows <- keep_rows[complete.cases(df[keep_rows, , drop = FALSE])]
    }
    
    # 2. Elimină duplicate
    if (isTRUE(input$remove_duplicates)) {
      keep_rows <- keep_rows[!duplicated(df[keep_rows, , drop = FALSE])]
    }
    
    # 3. Filtrare coloană / valoare / specială
    fc <- if (!is.null(input$filter_col)) input$filter_col else ""
    
    if (fc == "__missing__") {
      df_sub <- df[keep_rows, , drop = FALSE]
      keep_rows <- keep_rows[!complete.cases(df_sub)]
      
    } else if (fc == "__duplicates__") {
      df_sub <- df[keep_rows, , drop = FALSE]
      is_dup <- duplicated(df_sub) | duplicated(df_sub, fromLast = TRUE)
      keep_rows <- keep_rows[is_dup]
      
    } else {
      fv <- if (!is.null(input$filter_val)) input$filter_val else ""
      
      if (nchar(fc) > 0 && nchar(fv) > 0 && fc %in% names(df)) {
        df_sub      <- df[keep_rows, , drop = FALSE]
        col_vals    <- df_sub[[fc]]
        col_type_fc <- tryCatch(data_info()$types[[fc]], error = function(e) "Categorica")
        is_num_col  <- !is.null(col_type_fc) && col_type_fc == "Numerica"
        
        if (is_num_col) {
          fv_num <- suppressWarnings(as.numeric(fv))
          if (!is.na(fv_num)) {
            fo <- if (!is.null(input$filter_op)) input$filter_op else "eq"
            row_filter <- switch(fo,
                                 "eq"  = suppressWarnings(as.numeric(col_vals)) == fv_num,
                                 "lt"  = suppressWarnings(as.numeric(col_vals)) <  fv_num,
                                 "gt"  = suppressWarnings(as.numeric(col_vals)) >  fv_num,
                                 "lte" = suppressWarnings(as.numeric(col_vals)) <= fv_num,
                                 "gte" = suppressWarnings(as.numeric(col_vals)) >= fv_num,
                                 rep(TRUE, length(keep_rows))
            )
            row_filter[is.na(row_filter)] <- FALSE
            keep_rows <- keep_rows[row_filter]
          }
        } else {
          valid_vals <- unique(na.omit(as.character(col_vals)))
          if (fv %in% valid_vals) {
            row_filter <- as.character(col_vals) == fv
            row_filter[is.na(row_filter)] <- FALSE
            keep_rows <- keep_rows[row_filter]
          }
        }
      }
    }
    
    result <- df[keep_rows, , drop = FALSE]
    attr(result, "original_rows") <- keep_rows
    result
  })
  
  # data_final: aplică gruparea vârstă peste data_processed (socio + vizualizare)
  data_final <- reactive({
    req(data_processed())
    df <- data_processed()
    
    age_col <- names(df)[str_detect(tolower(names(df)), "v[âa]rst[ăa]|^age$|\\bage\\b")]
    if (length(age_col) > 0) {
      df <- df %>%
        mutate(across(all_of(age_col[1]), ~ suppressWarnings(
          cut(as.numeric(.), breaks = age_bins, labels = age_labels, include.lowest = TRUE)
        )))
    }
    df
  })
  
  # Fișier CSV temporar al datelor procesate → transmis funcțiilor Python
  temp_fp <- reactive({
    req(data_processed())
    tmp <- tempfile(fileext = ".csv")
    write.csv(data_processed(), tmp, row.names = FALSE)
    tmp
  })
  
  # -------------------------------------------------------------------------
  # Editare celule în tabel
  # -------------------------------------------------------------------------
  
  observeEvent(input$tbl_data_preview_cell_edit, {
    info     <- input$tbl_data_preview_cell_edit
    df_work  <- data_working()
    
    # Mapăm rândul vizibil → rândul original din data_working
    df_proc   <- isolate(data_processed())
    orig_rows <- attr(df_proc, "original_rows")
    
    orig_row <- if (!is.null(orig_rows) && info$row <= length(orig_rows)) {
      orig_rows[info$row]
    } else {
      info$row
    }
    
    col_idx <- info$col + 1  # DT e 0-indexed pe coloane
    
    new_val <- tryCatch(
      DT::coerceValue(info$value, df_work[orig_row, col_idx]),
      error = function(e) info$value
    )
    df_work[orig_row, col_idx] <- new_val
    data_working(df_work)
    
    # Actualizăm tabelul fără re-render
    DT::replaceData(dt_proxy, data_final(), resetPaging = FALSE, rownames = FALSE)
  })
  
  observeEvent(input$save_edits, {
    showNotification(
      tagList(icon("check-circle"), " Modificările au fost salvate!"),
      type = "message", duration = 3
    )
  })
  
  # -------------------------------------------------------------------------
  # Override tip coloană + actualizare selectori
  # -------------------------------------------------------------------------
  
  observeEvent(input$apply_override, {
    req(input$override_col, input$override_type)
    ov <- manual_types()
    ov[[input$override_col]] <- input$override_type
    manual_types(ov)
    showNotification(
      paste0("Tipul coloanei '", input$override_col,
             "' setat la '", input$override_type, "'."),
      type = "message", duration = 4
    )
  })
  
  filtered_cols <- reactive({
    req(data_info())
    info     <- data_info()
    types    <- info$types
    all_cols <- info$columns
    list(
      all  = all_cols,
      sens = all_cols[sapply(all_cols, function(c) types[[c]] %in% c("Categorica","Binara"))],
      tgt  = all_cols[sapply(all_cols, function(c) types[[c]] %in% c("Numerica","Binara"))],
      fin  = if (length(info$financial_candidates) > 0) info$financial_candidates else
        all_cols[sapply(all_cols, function(c) types[[c]] == "Numerica")]
    )
  })
  
  observeEvent(input$file, {
    req(filtered_cols(), data_info())
    cols <- filtered_cols()
    info <- data_info()
    
    updateSelectInput(session, "sensitive",
                      choices  = cols$sens,
                      selected = if (length(info$sensitive_candidates) > 0)
                        info$sensitive_candidates[1] else cols$sens[1])
    
    best_tgt <- if (length(info$financial_candidates) > 0)
      info$financial_candidates[1] else cols$tgt[1]
    updateSelectInput(session, "target",           choices = cols$tgt, selected = best_tgt)
    updateSelectInput(session, "override_col",     choices = cols$all)
    updateSelectInput(session, "socio_target_col", choices = cols$fin, selected = cols$fin[1])
    updateSelectInput(session, "filter_col",
                      choices = c(
                        "Niciun filtru"             = "",
                        "Arată rânduri cu valori lipsă" = "__missing__",
                        "Arată rânduri duplicate"        = "__duplicates__",
                        setNames(cols$all, cols$all)
                      ))
  })
  
  observeEvent(input$apply_override, {
    req(filtered_cols())
    cols <- filtered_cols()
    updateSelectInput(session, "sensitive",        choices = cols$sens)
    updateSelectInput(session, "target",           choices = cols$tgt)
    updateSelectInput(session, "override_col",     choices = cols$all)
    updateSelectInput(session, "socio_target_col", choices = cols$fin)
  }, ignoreInit = TRUE)
  # -------------------------------------------------------------------------
  # UI: Sumar fișier
  # -------------------------------------------------------------------------
  
  output$ui_file_summary <- renderUI({
    req(data_info(), data_processed())
    info   <- data_info()
    n_proc <- nrow(data_processed())
    tagList(
      fluidRow(
        infoBox("Rânduri (active)", format(n_proc, big.mark = "."),
                icon = icon("list"),       color = "blue",   width = 3),
        infoBox("Coloane", info$n_cols,
                icon = icon("columns"),    color = "green",  width = 3),
        infoBox("Atribut sensibil detectat",
                if (length(info$sensitive_candidates) > 0)
                  paste(info$sensitive_candidates, collapse = ", ") else "–",
                icon = icon("user-shield"), color = "orange", width = 3),
        infoBox("Target financiar detectat",
                if (length(info$financial_candidates) > 0)
                  paste(info$financial_candidates, collapse = ", ") else "–",
                icon = icon("euro-sign"),   color = "purple", width = 3)
      )
    )
  })
  
  # -------------------------------------------------------------------------
  # UI: Info duplicate
  # -------------------------------------------------------------------------
  
  output$ui_dup_info <- renderUI({
    req(data_working())
    n_dups <- sum(duplicated(data_working()))
    if (n_dups == 0) {
      div(class = "alert-box alert-green", style = "padding:5px 10px; margin:4px 0;",
          icon("check"), tags$small(" Niciun duplicat detectat."))
    } else {
      div(class = "alert-box alert-orange", style = "padding:5px 10px; margin:4px 0;",
          icon("exclamation-triangle"),
          tags$small(paste0(" ", n_dups, " rând(uri) duplicate găsite.")))
    }
  })
  
  # -------------------------------------------------------------------------
  # UI: Filtru valoare (dinamic după tipul coloanei)
  # -------------------------------------------------------------------------
  
  output$ui_filter_value <- renderUI({
    req(input$filter_col, data_working())
    if (is.null(input$filter_col) || 
        input$filter_col %in% c("", "__missing__", "__duplicates__")) return(NULL)
    
    df  <- data_working()
    col <- input$filter_col
    if (!col %in% names(df)) return(NULL)
    
    col_type <- tryCatch(data_info()$types[[col]], error = function(e) "Categorica")
    
    if (!is.null(col_type) && col_type == "Numerica") {
      tagList(
        fluidRow(
          column(5,
                 selectInput("filter_op", "Operator:",
                             choices  = c("=" = "eq", "<" = "lt", ">" = "gt",
                                          "≤" = "lte", "≥" = "gte"),
                             width    = "100%")
          ),
          column(7,
                 textInput("filter_val", "Valoare:",
                           placeholder = "ex: 25", width = "100%")
          )
        )
      )
    } else {
      vals <- sort(unique(na.omit(as.character(df[[col]]))))
      selectInput("filter_val", "Valoare:",
                  choices = c("(toate)" = "", vals),
                  width   = "100%")
    }
  })
  
  # -------------------------------------------------------------------------
  # UI: Status preprocesare
  # -------------------------------------------------------------------------
  
  output$ui_preprocess_status <- renderUI({
    req(data_working())
    n_orig    <- nrow(data_working())
    n_proc    <- tryCatch(nrow(data_processed()), error = function(e) n_orig)
    n_removed <- n_orig - n_proc
    
    tagList(
      div(class = "alert-box alert-green", style = "padding:5px 10px; margin:4px 0;",
          icon("database"),
          tags$small(paste0(" Original: ", n_orig, " rânduri"))),
      div(class = if (n_removed > 0) "alert-box alert-orange" else "alert-box alert-green",
          style = "padding:5px 10px; margin:4px 0;",
          icon(if (n_removed > 0) "filter" else "check"),
          tags$small(paste0(
            " Activ: ", n_proc, " rânduri",
            if (n_removed > 0) paste0(" (−", n_removed, " eliminate)") else ""
          ))
      )
    )
  })
  
  # -------------------------------------------------------------------------
  # UI: Tipuri coloane + Alerte valori lipsă
  # -------------------------------------------------------------------------
  
  output$tbl_col_types <- renderDT({
    req(data_info())
    info    <- data_info()
    df_types <- data.frame(
      Coloana = names(info$types),
      Tip     = unlist(info$types),
      stringsAsFactors = FALSE
    )
    datatable(df_types,
              options = list(pageLength = 15, dom = "t"),
              rownames = FALSE, class = "metric-table")
  })
  
  output$ui_missing_alerts <- renderUI({
    req(data_processed())
    df   <- data_processed()
    miss <- sapply(df, function(x) sum(is.na(x)))
    items <- Filter(function(x) x > 0, miss)
    if (length(items) == 0) {
      div(class = "alert-box alert-green",
          icon("check-circle"), " Nicio valoare lipsă în datele active.")
    } else {
      tagList(
        div(class = "alert-box alert-orange",
            strong("Coloane cu valori lipsă (date active):"),
            tags$ul(lapply(names(items), function(col)
              tags$li(paste0(col, ": ", items[[col]], " celule lipsă"))
            ))
        )
      )
    }
  })
  
  # -------------------------------------------------------------------------
  # Tabel editabil
  # -------------------------------------------------------------------------
  
  output$tbl_data_preview <- renderDT({
    req(data_final())
    datatable(
      data_final(),
      editable = "cell",
      rownames = FALSE,
      options  = list(pageLength = 10, scrollX = TRUE, dom = "lfrtip")
    )
  })
  
  # -------------------------------------------------------------------------
  # Download date procesate
  # -------------------------------------------------------------------------
  
  output$dl_data_csv <- downloadHandler(
    filename = function() paste0("date_procesate_", Sys.Date(), ".csv"),
    content  = function(file) {
      req(data_final())
      write.csv(data_final(), file, row.names = FALSE)
    }
  )
  
  # -------------------------------------------------------------------------
  # Calcul metrici (FR-04) – folosește datele procesate via temp_fp()
  # -------------------------------------------------------------------------
  
  metrics_result <- eventReactive(input$run, {
    req(data_info(), input$sensitive, input$target, temp_fp())
    info <- data_info()
    
    t_type <- info$types[[input$target]]
    s_type <- info$types[[input$sensitive]]
    
    if (!(s_type %in% c("Categorica", "Binara"))) {
      showNotification("Atributul sensibil trebuie să fie Categorică sau Binară.", type = "error")
      return(NULL)
    }
    if (!(t_type %in% c("Numerica", "Binara"))) {
      showNotification("Target-ul trebuie să fie Numeric sau Binar.", type = "error")
      return(NULL)
    }
    
    fp  <- temp_fp()
    res <- if (t_type == "Numerica")
      compute_numeric_metrics(fp, input$sensitive, input$target)
    else
      compute_binary_metrics(fp, input$sensitive, input$target)
    py_to_r_safe(res)
  })
  
  dist_alerts <- eventReactive(input$run, {
    req(input$sensitive, input$target, temp_fp())
    fp <- temp_fp()
    skew_res <- tryCatch(py_to_r_safe(compute_distribution_alerts(fp, input$target)),
                         error = function(e) NULL)
    imb_res  <- tryCatch(py_to_r_safe(compute_group_imbalance(fp, input$sensitive)),
                         error = function(e) list())
    list(skewness = skew_res, imbalance = imb_res)
  })
  
  bias_result <- eventReactive(input$run, {
    req(metrics_result(), dist_alerts())
    mr  <- metrics_result()
    
    effect <- if (!is.null(mr$cohen_d)) as.numeric(mr$cohen_d)
    else if (!is.null(mr$spd)) abs(as.numeric(mr$spd))
    else 0.0
    
    grp_props <- tryCatch({
      tbl <- table(data_processed()[[input$sensitive]])
      as.numeric(tbl / sum(tbl))
    }, error = function(e) c(0.5, 0.5))
    
    py_to_r_safe(compute_bias_score(effect, grp_props))
  })
  
  # -------------------------------------------------------------------------
  # UI: Bias score, alerte, metrici
  # -------------------------------------------------------------------------
  
  output$ui_bias_score <- renderUI({
    req(bias_result())
    br    <- bias_result()
    score <- as.numeric(br$bias_score)
    col   <- bias_color(score)
    lbl   <- bias_label(score)
    tagList(
      div(class = "bias-gauge",
          style = paste0("color:", col, "; background:", col, "22;"),
          round(score, 2)),
      tags$br(),
      tags$p(style = paste0("text-align:center; font-weight:bold; color:", col,
                            "; font-size:1.2em;"), lbl),
      tags$hr(),
      tags$small(
        tags$b("Scală:"), tags$br(),
        span(style = "color:#27ae60;", "0.00 – 0.19: Neglijabil"), tags$br(),
        span(style = "color:#f39c12;", "0.20 – 0.49: Moderat"),    tags$br(),
        span(style = "color:#e74c3c;", "0.50 – 1.00: Ridicat"),    tags$br(),
        tags$br(),
        tags$b("Componente:"), tags$br(),
        paste0("Efect (70%): ",       round(as.numeric(br$effect_component),    3)), tags$br(),
        paste0("Dezechilibru (30%): ", round(as.numeric(br$imbalance_component), 3))
      )
    )
  })
  
  output$ui_dist_alerts <- renderUI({
    req(dist_alerts())
    dal    <- dist_alerts()
    alerts <- tagList()
    
    for (item in dal$imbalance) {
      alerts <- tagList(alerts,
                        div(class = "alert-box alert-red",
                            icon("exclamation-triangle"), strong(" ALERTĂ CRITICĂ: "),
                            paste0("Grupul '", item$group, "' reprezintă doar ",
                                   round(item$pct, 1), "% din date (sub pragul de 20%)."))
      )
    }
    
    sk <- dal$skewness
    if (!is.null(sk) && !is.null(sk$skewness)) {
      sv   <- as.numeric(sk$skewness)
      scls <- if (abs(sv) > 1) "alert-red" else if (abs(sv) > 0.5) "alert-orange" else "alert-green"
      sint <- if (abs(sv) > 1) "Asimetrie puternică – distribuție non-normală"
      else if (abs(sv) > 0.5) "Asimetrie moderată" else "Distribuție aproape simetrică"
      alerts <- tagList(alerts,
                        div(class = paste("alert-box", scls),
                            icon("chart-area"), strong(" Asimetrie (Skewness): "),
                            paste0(sv, " – ", sint))
      )
      op   <- as.numeric(sk$outliers_pct)
      ocls <- if (op > 10) "alert-red" else if (op > 5) "alert-orange" else "alert-green"
      alerts <- tagList(alerts,
                        div(class = paste("alert-box", ocls),
                            icon("dot-circle"), strong(" Valori atipice (Outlieri): "),
                            paste0(sk$outliers_count, " valori (", op, "%) în afara intervalului IQR"))
      )
    }
    
    if (length(alerts) == 0)
      alerts <- div(class = "alert-box alert-green",
                    icon("check-circle"), " Nicio alertă distribuțională detectată.")
    alerts
  })
  
  output$ui_metrics_detail <- renderUI({
    req(metrics_result(), data_info())
    mr     <- metrics_result()
    t_type <- data_info()$types[[input$target]]
    
    if (t_type == "Numerica") {
      tagList(
        fluidRow(
          if (!is.null(mr$mean_diff))
            infoBox("Diferența mediei", mr$mean_diff, icon = icon("arrows-alt-v"),
                    color = if (abs(mr$mean_diff) > 100) "red" else "blue", width = 3),
          if (!is.null(mr$pct_diff))
            infoBox("Diferența %", paste0(mr$pct_diff, "%"), icon = icon("percent"),
                    color = if (abs(mr$pct_diff) > 20) "orange" else "green", width = 3),
          if (!is.null(mr$cohen_d))
            infoBox("Cohen's d", paste0(mr$cohen_d, " (", mr$cohen_d_interpretation, ")"),
                    icon = icon("ruler"), color = "purple", width = 3),
          if (!is.null(mr$p_value_ttest))
            infoBox("p-value (t-test)", fmt_p(mr$p_value_ttest), icon = icon("calculator"),
                    color = if (as.numeric(mr$p_value_ttest) < 0.05) "red" else "green", width = 3)
        ),
        if (!is.null(mr$f_stat))
          fluidRow(
            infoBox("F-stat (ANOVA)", mr$f_stat, icon = icon("chart-line"),
                    color = "light-blue", width = 3),
            infoBox("p-value (ANOVA)", fmt_p(mr$p_value_anova), icon = icon("calculator"),
                    color = if (!is.null(mr$p_value_anova) &&
                                as.numeric(mr$p_value_anova) < 0.05) "red" else "green",
                    width = 3)
          )
      )
    } else {
      tagList(
        fluidRow(
          if (!is.null(mr$spd))
            infoBox("SPD", mr$spd, subtitle = "Statistical Parity Difference",
                    icon = icon("balance-scale"),
                    color = if (abs(mr$spd) > 0.1) "red" else "green", width = 3),
          if (!is.null(mr$disparate_impact))
            infoBox("Disparate Impact", mr$disparate_impact, icon = icon("not-equal"),
                    color = if (mr$disparate_impact < 0.8 ||
                                mr$disparate_impact > 1.25) "orange" else "green", width = 3),
          if (!is.null(mr$risk_ratio))
            infoBox("Risk Ratio", mr$risk_ratio, icon = icon("percentage"),
                    color = "purple", width = 3)
        ),
        if (!is.null(mr$di_interpretation))
          div(class = paste("alert-box",
                            if (grepl("Echitabil", mr$di_interpretation)) "alert-green" else "alert-orange"),
              icon("info-circle"), " ", mr$di_interpretation)
      )
    }
  })
  
  output$tbl_group_summary <- renderDT({
    req(metrics_result())
    mr <- metrics_result()
    if (is.null(mr$summary)) return(NULL)
    df_sum <- as.data.frame(do.call(rbind, lapply(mr$summary, as.data.frame)))
    datatable(df_sum, rownames = FALSE, options = list(dom = "t"),
              class = "metric-table display")
  })
  
  output$dl_group_csv <- downloadHandler(
    filename = function() paste0("sumar_grupuri_", Sys.Date(), ".csv"),
    content  = function(file) {
      req(metrics_result())
      mr <- metrics_result()
      if (!is.null(mr$summary)) {
        df_sum <- as.data.frame(do.call(rbind, lapply(mr$summary, as.data.frame)))
        write.csv(df_sum, file, row.names = FALSE)
      } else {
        write.csv(data.frame(Mesaj = "Nu există date."), file, row.names = FALSE)
      }
    }
  )
  # -------------------------------------------------------------------------
  # TAB SOCIO-DEMOGRAFIC (FR-03)
  # -------------------------------------------------------------------------
  
  socio_result <- eventReactive(input$run_socio, {
    req(data_final(), input$socio_type, input$socio_target_col)
    df   <- data_final()
    type <- input$socio_type
    tcol <- input$socio_target_col
    if (!(tcol %in% names(df))) return(NULL)
    df[[tcol]] <- suppressWarnings(as.numeric(df[[tcol]]))
    
    group_col <- if (type == "age") {
      age_col <- names(df)[str_detect(tolower(names(df)), "v[âa]rst[ăa]|^age$")]
      if (length(age_col) == 0) {
        showNotification("Nu s-a detectat o coloană de vârstă.", type = "warning")
        return(NULL)
      }
      age_col[1]
    } else if (type == "edu") {
      edu_col <- names(df)[str_detect(tolower(names(df)), "educa|studi")]
      if (length(edu_col) == 0) {
        showNotification("Nu s-a detectat o coloană de educație.", type = "warning")
        return(NULL)
      }
      df[[edu_col[1]]] <- classify_education(df[[edu_col[1]]])
      edu_col[1]
    } else {
      reg_col <- names(df)[str_detect(tolower(names(df)), "regiu|jude[tț]|nuts|localit|zona")]
      if (length(reg_col) == 0) {
        showNotification("Nu s-a detectat o coloană de regiune/județ.", type = "warning")
        return(NULL)
      }
      reg_col[1]
    }
    
    df %>%
      filter(!is.na(.data[[group_col]]), !is.na(.data[[tcol]])) %>%
      group_by(Grup = .data[[group_col]]) %>%
      summarise(N = n(),
                Media   = round(mean(.data[[tcol]], na.rm = TRUE), 2),
                Mediană = round(median(.data[[tcol]], na.rm = TRUE), 2),
                SD      = round(sd(.data[[tcol]], na.rm = TRUE), 2),
                .groups = "drop") %>%
      arrange(Grup)
  })
  
  output$plot_socio_dist <- if (has_plotly) plotly::renderPlotly({
    req(socio_result())
    df_s <- socio_result()
    g <- ggplot(df_s, aes(x = reorder(Grup, Media), y = Media, fill = Grup)) +
      geom_col(show.legend = FALSE, color = "white") +
      geom_errorbar(aes(ymin = Media - SD, ymax = Media + SD), width = 0.25, color = "gray40") +
      coord_flip() + theme_minimal(base_size = 13) +
      labs(x = NULL, y = paste("Media –", input$socio_target_col),
           title = "Distribuția indicatorului financiar pe grupuri standardizate")
    plotly::ggplotly(g)
  }) else renderPlot({
    req(socio_result())
    df_s <- socio_result()
    ggplot(df_s, aes(x = reorder(Grup, Media), y = Media, fill = Grup)) +
      geom_col(show.legend = FALSE, color = "white") +
      geom_errorbar(aes(ymin = Media - SD, ymax = Media + SD), width = 0.25, color = "gray40") +
      coord_flip() + theme_minimal(base_size = 13) +
      labs(x = NULL, y = paste("Media –", input$socio_target_col),
           title = "Distribuția indicatorului financiar pe grupuri standardizate")
  })
  
  output$ui_socio_comparison <- renderUI({
    req(socio_result(), input$socio_ref_country)
    df_s        <- socio_result()
    ref_country <- input$socio_ref_country
    if (ref_country == "NONE") return(p("Nicio comparație selectată."))
    
    ref_val      <- tryCatch(get_eurostat_reference("salary", ref_country), error = function(e) NA)
    overall_mean <- mean(df_s$Media, na.rm = TRUE)
    
    if (is.na(ref_val))
      return(div(class = "alert-box alert-orange",
                 "Date de referință indisponibile pentru ", ref_country, "."))
    
    diff_val  <- overall_mean - ref_val
    diff_pct  <- round(diff_val / ref_val * 100, 1)
    sign_lbl  <- if (diff_val >= 0) "mai mare" else "mai mic"
    color_cls <- if (diff_val < 0) "alert-orange" else "alert-green"
    ref_label <- switch(ref_country,
                        RO = "Media României (net)", EU = "Media UE (brut)",
                        DE = "Germania (brut)",      FR = "Franța (brut)",
                        HU = "Ungaria (brut)",       BG = "Bulgaria (brut)", ref_country)
    
    tagList(
      div(class = "alert-box alert-green",
          strong("Media în date: "),
          paste0(format(round(overall_mean, 0), big.mark = "."), " RON")),
      div(class = "alert-box alert-orange",
          strong(paste0("Referință (", ref_label, "): ")),
          paste0(format(ref_val, big.mark = "."), " RON")),
      div(class = paste("alert-box", color_cls),
          strong("Diferență: "),
          paste0(abs(round(diff_val, 0)), " RON (", abs(diff_pct), "% ", sign_lbl, ")")),
      tags$hr(),
      div(style = "font-size:11px; color:#888; line-height:1.4;",
          icon("info-circle"), " ",
          tags$b("Notă metodologică:"), tags$br(),
          "Valorile de referință sunt exprimate în RON, convertite din EUR",
          " la cursul de 1 EUR = 5,2 RON (mai 2026).", tags$br(),
          "România: salariu mediu ", tags$b("net"), " (sursa INS 2023).", tags$br(),
          "UE/DE/FR/HU/BG: salariu mediu ", tags$b("brut"), " (sursa Eurostat 2023).", tags$br(),
          "Comparația net vs. brut este orientativă.")
    )
  })
  
  output$tbl_socio_summary <- renderDT({
    req(socio_result())
    datatable(socio_result(), rownames = FALSE,
              options = list(pageLength = 15, dom = "tip"),
              class = "metric-table display")
  })
  
  output$dl_socio_csv <- downloadHandler(
    filename = function() paste0("analiza_socio_", Sys.Date(), ".csv"),
    content  = function(file) {
      req(socio_result())
      write.csv(socio_result(), file, row.names = FALSE)
    }
  )
  
  # -------------------------------------------------------------------------
  # TAB VIZUALIZARE (FR-07)
  # -------------------------------------------------------------------------
  
  output$plot_boxplot <- if (has_plotly) plotly::renderPlotly({
    req(data_final(), input$sensitive, input$target, input$run)
    df <- data_final()
    df[[input$target]] <- suppressWarnings(as.numeric(df[[input$target]]))
    req(data_info()$types[[input$target]] == "Numerica")
    g <- ggplot(df, aes(x = .data[[input$sensitive]], y = .data[[input$target]],
                        fill = .data[[input$sensitive]])) +
      geom_boxplot(alpha = 0.7, outlier.colour = "red", outlier.shape = 1) +
      theme_minimal(base_size = 13) + theme(legend.position = "none") +
      labs(x = input$sensitive, y = input$target,
           title = paste("Boxplot –", input$target, "după", input$sensitive))
    plotly::ggplotly(g)
  }) else renderPlot({
    req(data_final(), input$sensitive, input$target, input$run)
    df <- data_final()
    df[[input$target]] <- suppressWarnings(as.numeric(df[[input$target]]))
    req(data_info()$types[[input$target]] == "Numerica")
    ggplot(df, aes(x = .data[[input$sensitive]], y = .data[[input$target]],
                   fill = .data[[input$sensitive]])) +
      geom_boxplot(alpha = 0.7, outlier.colour = "red", outlier.shape = 1) +
      theme_minimal(base_size = 13) + theme(legend.position = "none") +
      labs(x = input$sensitive, y = input$target,
           title = paste("Boxplot –", input$target, "după", input$sensitive))
  })
  
  output$plot_density <- if (has_plotly) plotly::renderPlotly({
    req(data_final(), input$sensitive, input$target, input$run)
    df <- data_final()
    df[[input$target]] <- suppressWarnings(as.numeric(df[[input$target]]))
    req(data_info()$types[[input$target]] == "Numerica")
    g <- ggplot(df, aes(x = .data[[input$target]],
                        fill  = as.factor(.data[[input$sensitive]]),
                        color = as.factor(.data[[input$sensitive]]))) +
      geom_density(alpha = 0.35) + theme_minimal(base_size = 13) +
      labs(x = input$target, y = "Densitate",
           fill = input$sensitive, color = input$sensitive,
           title = paste("Distribuțiile suprapuse –", input$target))
    plotly::ggplotly(g)
  }) else renderPlot({
    req(data_final(), input$sensitive, input$target, input$run)
    df <- data_final()
    df[[input$target]] <- suppressWarnings(as.numeric(df[[input$target]]))
    req(data_info()$types[[input$target]] == "Numerica")
    ggplot(df, aes(x = .data[[input$target]],
                   fill  = as.factor(.data[[input$sensitive]]),
                   color = as.factor(.data[[input$sensitive]]))) +
      geom_density(alpha = 0.35) + theme_minimal(base_size = 13) +
      labs(x = input$target, y = "Densitate",
           fill = input$sensitive, color = input$sensitive,
           title = paste("Distribuțiile suprapuse –", input$target))
  })
  
  output$plot_barplot <- if (has_plotly) plotly::renderPlotly({
    req(data_final(), input$sensitive, input$target, input$run)
    df <- data_final()
    df[[input$target]] <- suppressWarnings(as.numeric(df[[input$target]]))
    req(data_info()$types[[input$target]] == "Numerica")
    grand_mean <- mean(df[[input$target]], na.rm = TRUE)
    df_bar <- df %>%
      group_by(Grup = .data[[input$sensitive]]) %>%
      summarise(Media = mean(.data[[input$target]], na.rm = TRUE), .groups = "drop") %>%
      mutate(Diferenta = Media - grand_mean,
             Directie  = if_else(Diferenta >= 0, "Peste medie", "Sub medie"))
    g <- ggplot(df_bar, aes(x = reorder(Grup, Diferenta), y = Diferenta, fill = Directie)) +
      geom_col() + geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
      scale_fill_manual(values = c("Peste medie" = "#27ae60", "Sub medie" = "#e74c3c")) +
      coord_flip() + theme_minimal(base_size = 13) +
      labs(x = input$sensitive,
           y = paste("Diferența față de media globală (", round(grand_mean, 1), ")"),
           title = "Diferențe față de media globală", fill = NULL)
    plotly::ggplotly(g)
  }) else renderPlot({
    req(data_final(), input$sensitive, input$target, input$run)
    df <- data_final()
    df[[input$target]] <- suppressWarnings(as.numeric(df[[input$target]]))
    req(data_info()$types[[input$target]] == "Numerica")
    grand_mean <- mean(df[[input$target]], na.rm = TRUE)
    df_bar <- df %>%
      group_by(Grup = .data[[input$sensitive]]) %>%
      summarise(Media = mean(.data[[input$target]], na.rm = TRUE), .groups = "drop") %>%
      mutate(Diferenta = Media - grand_mean,
             Directie  = if_else(Diferenta >= 0, "Peste medie", "Sub medie"))
    ggplot(df_bar, aes(x = reorder(Grup, Diferenta), y = Diferenta, fill = Directie)) +
      geom_col() + geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
      scale_fill_manual(values = c("Peste medie" = "#27ae60", "Sub medie" = "#e74c3c")) +
      coord_flip() + theme_minimal(base_size = 13) +
      labs(x = input$sensitive,
           y = paste("Diferența față de media globală (", round(grand_mean, 1), ")"),
           title = "Diferențe față de media globală", fill = NULL)
  })
  
  output$plot_parity <- if (has_plotly) plotly::renderPlotly({
    req(data_final(), input$sensitive, input$target, input$run)
    df <- data_final()
    req(data_info()$types[[input$target]] %in% c("Binara", "Categorica"))
    g <- ggplot(df, aes(x = .data[[input$sensitive]],
                        fill = as.factor(.data[[input$target]]))) +
      geom_bar(position = "fill") +
      scale_y_continuous(labels = scales::percent) +
      theme_minimal(base_size = 13) +
      labs(y = "Proporție", fill = input$target, x = input$sensitive,
           title = paste("Proporția categoriilor –", input$target, "pe", input$sensitive))
    plotly::ggplotly(g)
  }) else renderPlot({
    req(data_final(), input$sensitive, input$target, input$run)
    df <- data_final()
    req(data_info()$types[[input$target]] %in% c("Binara", "Categorica"))
    ggplot(df, aes(x = .data[[input$sensitive]],
                   fill = as.factor(.data[[input$target]]))) +
      geom_bar(position = "fill") +
      scale_y_continuous(labels = scales::percent) +
      theme_minimal(base_size = 13) +
      labs(y = "Proporție", fill = input$target, x = input$sensitive,
           title = paste("Proporția categoriilor –", input$target, "pe", input$sensitive))
  })
  
  # -------------------------------------------------------------------------
  # TAB EXPORT
  # -------------------------------------------------------------------------
  
  make_boxplot_gg <- function() {
    req(data_final(), input$sensitive, input$target)
    df <- data_final()
    df[[input$target]] <- suppressWarnings(as.numeric(df[[input$target]]))
    ggplot(df, aes(x = .data[[input$sensitive]], y = .data[[input$target]],
                   fill = .data[[input$sensitive]])) +
      geom_boxplot(alpha = 0.7, outlier.colour = "red") +
      theme_minimal(base_size = 14) + theme(legend.position = "none") +
      labs(x = input$sensitive, y = input$target, title = "Boxplot Distribuție")
  }
  
  make_density_gg <- function() {
    req(data_final(), input$sensitive, input$target)
    df <- data_final()
    df[[input$target]] <- suppressWarnings(as.numeric(df[[input$target]]))
    ggplot(df, aes(x = .data[[input$target]],
                   fill  = as.factor(.data[[input$sensitive]]),
                   color = as.factor(.data[[input$sensitive]]))) +
      geom_density(alpha = 0.35) + theme_minimal(base_size = 14) +
      labs(x = input$target, y = "Densitate",
           fill = input$sensitive, color = input$sensitive)
  }
  
  make_barplot_gg <- function() {
    req(data_final(), input$sensitive, input$target)
    df <- data_final()
    df[[input$target]] <- suppressWarnings(as.numeric(df[[input$target]]))
    grand_mean <- mean(df[[input$target]], na.rm = TRUE)
    df_bar <- df %>%
      group_by(Grup = .data[[input$sensitive]]) %>%
      summarise(Media = mean(.data[[input$target]], na.rm = TRUE), .groups = "drop") %>%
      mutate(Diferenta = Media - grand_mean,
             Directie  = if_else(Diferenta >= 0, "Peste medie", "Sub medie"))
    ggplot(df_bar, aes(x = reorder(Grup, Diferenta), y = Diferenta, fill = Directie)) +
      geom_col() + coord_flip() + theme_minimal(base_size = 14) +
      scale_fill_manual(values = c("Peste medie" = "#27ae60", "Sub medie" = "#e74c3c")) +
      labs(x = input$sensitive, y = "Diferența față de medie", fill = NULL)
  }
  
  output$dl_boxplot <- downloadHandler(
    filename = function() paste0("boxplot_", Sys.Date(), ".png"),
    content  = function(file) ggplot2::ggsave(file, plot = make_boxplot_gg(),
                                              width = 10, height = 6, dpi = 150)
  )
  output$dl_density <- downloadHandler(
    filename = function() paste0("density_", Sys.Date(), ".png"),
    content  = function(file) ggplot2::ggsave(file, plot = make_density_gg(),
                                              width = 10, height = 6, dpi = 150)
  )
  output$dl_barplot <- downloadHandler(
    filename = function() paste0("barplot_", Sys.Date(), ".png"),
    content  = function(file) ggplot2::ggsave(file, plot = make_barplot_gg(),
                                              width = 10, height = 6, dpi = 150)
  )
  
  output$dl_report <- downloadHandler(
    filename = function() paste0("raport_disparitati_", Sys.Date(), ".csv"),
    content  = function(file) {
      req(metrics_result(), bias_result())
      mr <- metrics_result()
      br <- bias_result()
      lines <- c(
        paste0("Data analizei,",   Sys.Date()),
        paste0("Atribut sensibil,", input$sensitive),
        paste0("Target,",           input$target),
        paste0("Bias Score,",       br$bias_score),
        paste0("Severitate,",       br$severity), ""
      )
      if (!is.null(mr$cohen_d))
        lines <- c(lines,
                   paste0("Cohen d,",         mr$cohen_d),
                   paste0("Interpretare,",    mr$cohen_d_interpretation),
                   paste0("Diferenta medie,", mr$mean_diff),
                   paste0("Diferenta %,",     mr$pct_diff),
                   paste0("t-stat,",          mr$t_stat),
                   paste0("p-value t-test,",  mr$p_value_ttest))
      if (!is.null(mr$spd))
        lines <- c(lines,
                   paste0("SPD,",              mr$spd),
                   paste0("Disparate Impact,", mr$disparate_impact),
                   paste0("Risk Ratio,",       mr$risk_ratio))
      writeLines(lines, file)
    }
  )
  
  output$ui_export_preview <- renderUI({
    req(input$run)
    div(class = "alert-box alert-green",
        icon("info-circle"),
        " Rulați analiza mai întâi (butonul din sidebar), apoi descărcați raportul.")
  })
  
}

shinyApp(ui, server)