library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(reticulate)

if (requireNamespace("plotly",  quietly = TRUE)) library(plotly)
if (requireNamespace("readxl",  quietly = TRUE)) library(readxl)
if (requireNamespace("scales",  quietly = TRUE)) library(scales)

source("R/standards.R")
source_python("logic.py")

has_plotly <- requireNamespace("plotly", quietly = TRUE)

render_chart <- function(...) { if (has_plotly) plotly::renderPlotly(...) else renderPlot(...) }
chart_output  <- function(id)  { if (has_plotly) plotly::plotlyOutput(id) else plotOutput(id) }

py_to_r_safe <- function(x) tryCatch(reticulate::py_to_r(x), error = function(e) x)

fmt_p <- function(p) {
  if (is.null(p) || is.na(p)) return("N/A")
  p <- as.numeric(p)
  if (p < 0.001) "< 0.001" else as.character(round(p, 4))
}

bias_color <- function(s) { s <- as.numeric(s); if(s<.20) "#27ae60" else if(s<.50) "#f39c12" else "#e74c3c" }
bias_label <- function(s) { s <- as.numeric(s); if(s<.20) "Neglijabil" else if(s<.50) "Moderat" else "Ridicat" }

# =============================================================================
# UI
# =============================================================================
ui <- dashboardPage(skin = "blue",
                    dashboardHeader(title = "Bias Detection Dashboard"),
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Date",            tabName = "tab_data",   icon = icon("table")),
                        menuItem("Analiza Generala",tabName = "tab_bias",   icon = icon("balance-scale")),
                        menuItem("Socio-Demografic",tabName = "tab_socio",  icon = icon("users")),
                        menuItem("Vizualizare",     tabName = "tab_viz",    icon = icon("chart-bar")),
                        menuItem("Export",          tabName = "tab_export", icon = icon("download"))
                      ),
                      tags$hr(),
                      fileInput("file", "Incarca fisier (CSV / Excel)",
                                accept = c(".csv",".xlsx",".xls"),
                                buttonLabel = "Alege fisier", placeholder = "Niciun fisier selectat"),
                      selectInput("sensitive", "Atribut sensibil",         choices = NULL),
                      selectInput("target",    "Variabila analizata",       choices = NULL),
                      tags$details(
                        tags$summary(style="color:#aaa;cursor:pointer;font-size:12px;", "Setare manuala tip coloana"),
                        selectInput("override_col",  "Coloana", choices = NULL),
                        selectInput("override_type", "Tip nou", choices = c("Numerica","Binara","Categorica")),
                        actionButton("apply_override","Aplica", class="btn-xs btn-warning", icon=icon("edit"))
                      ),
                      tags$hr(),
                      actionButton("run","Ruleaza analiza", icon=icon("play"), class="btn-primary btn-block"),
                      tags$br()
                    ),
                    dashboardBody(
                      tags$head(tags$style(HTML("
      .bias-gauge{font-size:2.5em;font-weight:bold;text-align:center;padding:10px;border-radius:8px;}
      .alert-box{border-left:5px solid;padding:10px;margin:6px 0;border-radius:4px;}
      .alert-red   {border-color:#e74c3c;background:#fdf3f3;}
      .alert-orange{border-color:#f39c12;background:#fef9f0;}
      .alert-green {border-color:#27ae60;background:#f0faf4;}
      .metric-table th{background:#2980b9;color:white;}
    "))),
                      tabItems(
                        
                        # --- TAB DATE ---
                        tabItem(tabName="tab_data",
                                fluidRow(box(title="Sumar fisier",status="primary",solidHeader=TRUE,width=12, uiOutput("ui_file_summary"))),
                                fluidRow(
                                  box(title="Tipuri detectate (FR-01)",status="info",solidHeader=TRUE,width=6, DTOutput("tbl_col_types")),
                                  box(title="Valori lipsa (FR-05)",    status="warning",solidHeader=TRUE,width=6, uiOutput("ui_missing_alerts"))
                                ),
                                fluidRow(box(title="Previzualizare",status="primary",solidHeader=TRUE,width=12, DTOutput("tbl_data_preview")))
                        ),
                        
                        # --- TAB ANALIZA GENERALA ---
                        tabItem(tabName="tab_bias",
                                fluidRow(
                                  box(title="Bias Score (FR-06)",         status="primary",solidHeader=TRUE,width=4, uiOutput("ui_bias_score")),
                                  box(title="Alerte Distributionale (FR-05)",status="warning",solidHeader=TRUE,width=8, uiOutput("ui_dist_alerts"))
                                ),
                                fluidRow(box(title="Metrici Disparitate (FR-04)",status="info",solidHeader=TRUE,width=12, uiOutput("ui_metrics_detail"))),
                                fluidRow(box(title="Tabel sumar grupuri",status="primary",solidHeader=TRUE,width=12, DTOutput("tbl_group_summary")))
                        ),
                        
                        # --- TAB SOCIO-DEMOGRAFIC ---
                        tabItem(tabName="tab_socio",
                                fluidRow(box(title="Configurare (FR-03)",status="primary",solidHeader=TRUE,width=12,
                                             column(4, selectInput("socio_type","Tip analiza",
                                                                   choices=c("Varsta (grupare standard)"="age",
                                                                             "Educatie (ISCED)"="edu",
                                                                             "Regiune (NUTS Romania)"="nuts"))),
                                             column(4, selectInput("socio_target_col","Indicator financiar",choices=NULL)),
                                             column(4,
                                                    selectInput("socio_ref_country","Compara cu:",
                                                                choices=c("Media Romaniei"="RO","Media UE"="EU",
                                                                          "Germania"="DE","Franta"="FR",
                                                                          "Ungaria"="HU","Bulgaria"="BG",
                                                                          "Fara comparatie"="NONE")),
                                                    actionButton("run_socio","Analizeaza",icon=icon("search"),class="btn-success"))
                                )),
                                fluidRow(
                                  box(title="Distributia pe grupuri",status="info",solidHeader=TRUE,width=8, chart_output("plot_socio_dist")),
                                  box(title="Comparatie referinta",  status="warning",solidHeader=TRUE,width=4, uiOutput("ui_socio_comparison"))
                                ),
                                fluidRow(box(title="Tabel detaliat",status="primary",solidHeader=TRUE,width=12, DTOutput("tbl_socio_summary")))
                        ),
                        
                        # --- TAB VIZUALIZARE ---
                        tabItem(tabName="tab_viz",
                                fluidRow(tabBox(title="Grafice (FR-07)",width=12,
                                                tabPanel("Boxplot",      p("Distributia valorilor numerice pe grupuri."), chart_output("plot_boxplot")),
                                                tabPanel("Density Plot", p("Suprapunerea distributiilor per grup."),      chart_output("plot_density")),
                                                tabPanel("Barplot Dif.", p("Diferenta mediei fata de media globala."),    chart_output("plot_barplot")),
                                                tabPanel("Proportii",   p("Proportia outcome pozitiv pe grupuri."),       chart_output("plot_parity"))
                                ))
                        ),
                        
                        # --- TAB EXPORT ---
                        tabItem(tabName="tab_export",
                                fluidRow(box(title="Export rezultate",status="primary",solidHeader=TRUE,width=12,
                                             p("Descarca graficele si raportul dupa ce ai rulat analiza."), tags$br(),
                                             fluidRow(
                                               column(3, downloadButton("dl_boxplot","Boxplot (PNG)",   class="btn-info btn-block")),
                                               column(3, downloadButton("dl_density","Density (PNG)",   class="btn-info btn-block")),
                                               column(3, downloadButton("dl_barplot","Barplot (PNG)",   class="btn-info btn-block")),
                                               column(3, downloadButton("dl_report", "Raport CSV",      class="btn-success btn-block"))
                                             )
                                ))
                        )
                      )
                    )
)

# =============================================================================
# SERVER
# =============================================================================
server <- function(input, output, session) {
  
  manual_types <- reactiveVal(list())
  
  data_info <- reactive({
    req(input$file)
    info <- py_to_r_safe(profile_data(input$file$datapath))
    if (!is.null(info$error)) { showNotification(info$error, type="error"); validate(need(FALSE, info$error)) }
    ov <- manual_types()
    for (col in names(ov)) info$types[[col]] <- ov[[col]]
    info
  })
  
  data_raw <- reactive({
    req(input$file)
    fp <- input$file$datapath
    if (grepl("\\.xlsx?$", input$file$name, ignore.case=TRUE) &&
        requireNamespace("readxl", quietly=TRUE)) {
      readxl::read_excel(fp)
    } else {
      read.csv(fp)
    }
  })
  
  data_final <- reactive({
    req(data_raw())
    df <- as.data.frame(data_raw())
    age_col <- names(df)[str_detect(tolower(names(df)), "varst|^age$|\\bage\\b")]
    if (length(age_col) > 0)
      df <- df %>% mutate(across(all_of(age_col[1]), ~suppressWarnings(
        cut(as.numeric(.), breaks=age_bins, labels=age_labels, include.lowest=TRUE))))
    df
  })
  
  observeEvent(input$apply_override, {
    req(input$override_col, input$override_type)
    ov <- manual_types(); ov[[input$override_col]] <- input$override_type; manual_types(ov)
    showNotification(paste0("Tip '",input$override_col,"' -> '",input$override_type,"'"), type="message")
  })
  
  observe({
    req(data_info())
    info <- data_info(); types <- info$types; cols <- info$columns
    sens_cols <- cols[sapply(cols, function(c) types[[c]] %in% c("Categorica","Binara"))]
    tgt_cols  <- cols[sapply(cols, function(c) types[[c]] %in% c("Numerica","Binara"))]
    fin_cols  <- if (length(info$financial_candidates)>0) info$financial_candidates else
      cols[sapply(cols, function(c) types[[c]]=="Numerica")]
    if (!length(sens_cols)) sens_cols <- cols
    if (!length(tgt_cols))  tgt_cols  <- cols
    if (!length(fin_cols))  fin_cols  <- cols
    updateSelectInput(session,"sensitive", choices=sens_cols,
                      selected=if(length(info$sensitive_candidates)>0) info$sensitive_candidates[1] else sens_cols[1])
    updateSelectInput(session,"target", choices=tgt_cols,
                      selected=if(length(info$financial_candidates)>0) info$financial_candidates[1] else tgt_cols[1])
    updateSelectInput(session,"override_col", choices=cols)
    updateSelectInput(session,"socio_target_col", choices=fin_cols, selected=fin_cols[1])
  })
  
  # --- Sumar date ---
  output$ui_file_summary <- renderUI({
    req(data_info()); info <- data_info()
    tagList(fluidRow(
      infoBox("Randuri",  format(info$n_rows,big.mark="."), icon=icon("list"),        color="blue",   width=3),
      infoBox("Coloane",  info$n_cols,                      icon=icon("columns"),     color="green",  width=3),
      infoBox("Sensibil detectat",
              if(length(info$sensitive_candidates)>0) paste(info$sensitive_candidates,collapse=", ") else "-",
              icon=icon("user-shield"), color="orange", width=3),
      infoBox("Target financiar",
              if(length(info$financial_candidates)>0) paste(info$financial_candidates,collapse=", ") else "-",
              icon=icon("euro-sign"),   color="purple", width=3)
    ))
  })
  
  output$tbl_col_types <- renderDT({
    req(data_info()); info <- data_info()
    datatable(data.frame(Coloana=names(info$types), Tip=unlist(info$types), stringsAsFactors=FALSE),
              options=list(pageLength=15,dom="t"), rownames=FALSE)
  })
  
  output$ui_missing_alerts <- renderUI({
    req(data_info())
    items <- Filter(function(x) x>0, data_info()$missing)
    if (!length(items)) return(div(class="alert-box alert-green", icon("check-circle"), " Nicio valoare lipsa."))
    div(class="alert-box alert-orange", strong("Coloane cu valori lipsa:"),
        tags$ul(lapply(names(items), function(col) tags$li(paste0(col,": ",items[[col]]," lipsa")))))
  })
  
  output$tbl_data_preview <- renderDT({
    req(data_final())
    datatable(data_final(), options=list(pageLength=10,scrollX=TRUE), rownames=FALSE)
  })
  
  # --- Metrici FR-04 ---
  metrics_result <- eventReactive(input$run, {
    req(data_info(), input$sensitive, input$target)
    info   <- data_info()
    t_type <- info$types[[input$target]]
    s_type <- info$types[[input$sensitive]]
    if (!(s_type %in% c("Categorica","Binara"))) { showNotification("Atribut sensibil trebuie sa fie Categorica/Binara (FR-02).", type="error"); return(NULL) }
    if (!(t_type %in% c("Numerica","Binara")))   { showNotification("Target trebuie sa fie Numeric/Binar (FR-02).", type="error"); return(NULL) }
    fp  <- input$file$datapath
    res <- if (t_type=="Numerica") compute_numeric_metrics(fp,input$sensitive,input$target)
    else                    compute_binary_metrics(fp,input$sensitive,input$target)
    py_to_r_safe(res)
  })
  
  # --- Alerte FR-05 ---
  dist_alerts <- eventReactive(input$run, {
    req(input$file, input$sensitive, input$target)
    fp <- input$file$datapath
    list(
      skewness  = tryCatch(py_to_r_safe(compute_distribution_alerts(fp,input$target)), error=function(e) NULL),
      imbalance = tryCatch(py_to_r_safe(compute_group_imbalance(fp,input$sensitive)),  error=function(e) list())
    )
  })
  
  # --- Bias Score FR-06 ---
  bias_result <- eventReactive(input$run, {
    req(metrics_result(), dist_alerts())
    mr  <- metrics_result()
    effect <- if (!is.null(mr$cohen_d)) as.numeric(mr$cohen_d)
    else if (!is.null(mr$spd)) abs(as.numeric(mr$spd)) else 0.0
    grp_props <- tryCatch({
      tbl <- table(read.csv(input$file$datapath)[[input$sensitive]])
      as.numeric(tbl/sum(tbl))
    }, error=function(e) c(0.5,0.5))
    py_to_r_safe(compute_bias_score(effect, grp_props))
  })
  
  output$ui_bias_score <- renderUI({
    req(bias_result()); br <- bias_result()
    score <- as.numeric(br$bias_score); col <- bias_color(score); lbl <- bias_label(score)
    tagList(
      div(class="bias-gauge", style=paste0("color:",col,";background:",col,"22;"), round(score,2)),
      tags$br(),
      tags$p(style=paste0("text-align:center;font-weight:bold;color:",col,";font-size:1.2em;"), lbl),
      tags$hr(),
      tags$small(
        tags$b("Scala:"), tags$br(),
        span(style="color:#27ae60;","0.00-0.19: Neglijabil"), tags$br(),
        span(style="color:#f39c12;","0.20-0.49: Moderat"),    tags$br(),
        span(style="color:#e74c3c;","0.50-1.00: Ridicat"),    tags$br(), tags$br(),
        tags$b("Componente:"), tags$br(),
        paste0("Efect (70%): ",       round(as.numeric(br$effect_component),3)),    tags$br(),
        paste0("Dezechilibru (30%): ", round(as.numeric(br$imbalance_component),3))
      )
    )
  })
  
  output$ui_dist_alerts <- renderUI({
    req(dist_alerts()); dal <- dist_alerts(); alerts <- tagList()
    for (item in dal$imbalance)
      alerts <- tagList(alerts, div(class="alert-box alert-red", icon("exclamation-triangle"),
                                    strong(" ALERTA CRITICA: "), paste0("Grupul '",item$group,"' = ",round(item$pct,1),"% (sub 20%)")))
    sk <- dal$skewness
    if (!is.null(sk) && !is.null(sk$skewness)) {
      sv  <- as.numeric(sk$skewness)
      cls <- if(abs(sv)>1) "alert-red" else if(abs(sv)>0.5) "alert-orange" else "alert-green"
      itp <- if(abs(sv)>1) "Asimetrie puternica" else if(abs(sv)>0.5) "Asimetrie moderata" else "Distributie simetrica"
      alerts <- tagList(alerts,
                        div(class=paste("alert-box",cls), icon("chart-area"), strong(" Skewness: "), paste0(sv," - ",itp)),
                        div(class=paste("alert-box", if(as.numeric(sk$outliers_pct)>10)"alert-red" else if(as.numeric(sk$outliers_pct)>5)"alert-orange" else "alert-green"),
                            icon("dot-circle"), strong(" Outlieri: "), paste0(sk$outliers_count," (",sk$outliers_pct,"%) in afara IQR"))
      )
    }
    if (length(alerts)==0) div(class="alert-box alert-green", icon("check-circle"), " Nicio alerta distributionala.") else alerts
  })
  
  output$ui_metrics_detail <- renderUI({
    req(metrics_result(), data_info())
    mr <- metrics_result(); t_type <- data_info()$types[[input$target]]
    if (t_type=="Numerica") {
      tagList(
        fluidRow(
          if(!is.null(mr$mean_diff)) infoBox("Dif. medie",   mr$mean_diff,                 icon=icon("arrows-alt-v"), color=if(abs(mr$mean_diff)>100)"red" else "blue",  width=3),
          if(!is.null(mr$pct_diff))  infoBox("Dif. %",       paste0(mr$pct_diff,"%"),       icon=icon("percent"),     color=if(abs(mr$pct_diff)>20)"orange" else "green",  width=3),
          if(!is.null(mr$cohen_d))   infoBox("Cohen's d",    paste0(mr$cohen_d," (",mr$cohen_d_interpretation,")"), icon=icon("ruler"), color="purple", width=3),
          if(!is.null(mr$p_value_ttest)) infoBox("p (t-test)", fmt_p(mr$p_value_ttest), icon=icon("calculator"), color=if(as.numeric(mr$p_value_ttest)<0.05)"red" else "green", width=3)
        ),
        if(!is.null(mr$f_stat)) fluidRow(
          infoBox("F-stat (ANOVA)",  mr$f_stat,               icon=icon("chart-line"), color="light-blue", width=3),
          infoBox("p (ANOVA)", fmt_p(mr$p_value_anova), icon=icon("calculator"),
                  color=if(!is.null(mr$p_value_anova)&&as.numeric(mr$p_value_anova)<0.05)"red" else "green", width=3)
        )
      )
    } else {
      tagList(
        fluidRow(
          if(!is.null(mr$spd))              infoBox("SPD",            mr$spd,              icon=icon("balance-scale"), color=if(abs(mr$spd)>0.1)"red" else "green",   width=3),
          if(!is.null(mr$disparate_impact)) infoBox("Disparate Impact",mr$disparate_impact,icon=icon("not-equal"),     color=if(mr$disparate_impact<0.8||mr$disparate_impact>1.25)"orange" else "green", width=3),
          if(!is.null(mr$risk_ratio))       infoBox("Risk Ratio",     mr$risk_ratio,       icon=icon("percentage"),    color="purple", width=3)
        ),
        if(!is.null(mr$di_interpretation))
          div(class=paste("alert-box",if(grepl("Echitabil",mr$di_interpretation))"alert-green" else "alert-orange"),
              icon("info-circle")," ",mr$di_interpretation)
      )
    }
  })
  
  output$tbl_group_summary <- renderDT({
    req(metrics_result()); mr <- metrics_result()
    if (is.null(mr$summary)) return(NULL)
    datatable(as.data.frame(do.call(rbind,lapply(mr$summary,as.data.frame))),
              rownames=FALSE, options=list(dom="t"))
  })
  
  # --- Socio-demografic FR-03 ---
  socio_result <- eventReactive(input$run_socio, {
    req(data_final(), input$socio_type, input$socio_target_col)
    df <- data_final(); type <- input$socio_type; tcol <- input$socio_target_col
    if (!(tcol %in% names(df))) return(NULL)
    df[[tcol]] <- suppressWarnings(as.numeric(df[[tcol]]))
    
    group_col <- if (type=="age") {
      ac <- names(df)[str_detect(tolower(names(df)),"varst|^age$")]; if(!length(ac)){showNotification("Coloana varsta negasita.",type="warning");return(NULL)}; ac[1]
    } else if (type=="edu") {
      ec <- names(df)[str_detect(tolower(names(df)),"educa|studi")]; if(!length(ec)){showNotification("Coloana educatie negasita.",type="warning");return(NULL)}
      df[[ec[1]]] <- classify_education(df[[ec[1]]]); ec[1]
    } else {
      rc <- names(df)[str_detect(tolower(names(df)),"regiu|judet|nuts|zona")]; if(!length(rc)){showNotification("Coloana regiune negasita.",type="warning");return(NULL)}; rc[1]
    }
    
    df %>% filter(!is.na(.data[[group_col]]),!is.na(.data[[tcol]])) %>%
      group_by(Grup=.data[[group_col]]) %>%
      summarise(N=n(), Media=round(mean(.data[[tcol]],na.rm=TRUE),2),
                Mediana=round(median(.data[[tcol]],na.rm=TRUE),2),
                SD=round(sd(.data[[tcol]],na.rm=TRUE),2), .groups="drop") %>% arrange(Grup)
  })
  
  output$plot_socio_dist <- render_chart({
    req(socio_result()); df_s <- socio_result()
    g <- ggplot(df_s, aes(x=reorder(Grup,Media),y=Media,fill=Grup)) +
      geom_col(show.legend=FALSE,color="white") +
      geom_errorbar(aes(ymin=Media-SD,ymax=Media+SD),width=.25,color="gray40") +
      coord_flip() + theme_minimal(base_size=13) +
      labs(x=NULL,y=paste("Media -",input$socio_target_col),title="Indicator financiar pe grupuri standardizate")
    if(has_plotly) plotly::ggplotly(g) else g
  })
  
  output$ui_socio_comparison <- renderUI({
    req(socio_result()); rc <- input$socio_ref_country
    if (rc=="NONE") return(p("Nicio comparatie selectata."))
    ref_val <- tryCatch(get_eurostat_reference("salary",rc), error=function(e) NA)
    om <- mean(socio_result()$Media,na.rm=TRUE)
    if (is.na(ref_val)) return(div(class="alert-box alert-orange","Date indisponibile pentru ",rc,"."))
    dv <- om-ref_val; dp <- round(dv/ref_val*100,1)
    tagList(
      div(class="alert-box alert-green",  strong("Media in date: "), format(round(om,0),big.mark=".")),
      div(class="alert-box alert-orange", strong(paste0("Referinta (",rc,"): ")), format(ref_val,big.mark=".")),
      div(class=paste("alert-box",if(dv<0)"alert-orange" else "alert-green"),
          strong("Diferenta: "), paste0(abs(round(dv,0))," (",abs(dp),"% ",if(dv>=0)"mai mare" else "mai mic",")"))
    )
  })
  
  output$tbl_socio_summary <- renderDT({
    req(socio_result())
    datatable(socio_result(),rownames=FALSE,options=list(pageLength=15,dom="tip"))
  })
  
  # --- Grafice FR-07 ---
  make_gg_box <- function() {
    df <- data_final(); df[[input$target]] <- suppressWarnings(as.numeric(df[[input$target]]))
    ggplot(df, aes(x=.data[[input$sensitive]],y=.data[[input$target]],fill=.data[[input$sensitive]])) +
      geom_boxplot(alpha=.7,outlier.colour="red",outlier.shape=1) +
      theme_minimal(base_size=13)+theme(legend.position="none") +
      labs(x=input$sensitive,y=input$target,title=paste("Boxplot -",input$target))
  }
  make_gg_density <- function() {
    df <- data_final(); df[[input$target]] <- suppressWarnings(as.numeric(df[[input$target]]))
    ggplot(df, aes(x=.data[[input$target]],fill=as.factor(.data[[input$sensitive]]),color=as.factor(.data[[input$sensitive]]))) +
      geom_density(alpha=.35)+theme_minimal(base_size=13)+
      labs(x=input$target,y="Densitate",fill=input$sensitive,color=input$sensitive)
  }
  make_gg_bar <- function() {
    df <- data_final(); df[[input$target]] <- suppressWarnings(as.numeric(df[[input$target]]))
    gm <- mean(df[[input$target]],na.rm=TRUE)
    df %>% group_by(Grup=.data[[input$sensitive]]) %>%
      summarise(Media=mean(.data[[input$target]],na.rm=TRUE),.groups="drop") %>%
      mutate(Dif=Media-gm, Dir=if_else(Dif>=0,"Peste medie","Sub medie")) %>%
      ggplot(aes(x=reorder(Grup,Dif),y=Dif,fill=Dir)) + geom_col() + coord_flip() +
      geom_hline(yintercept=0,linetype="dashed",color="gray50") +
      scale_fill_manual(values=c("Peste medie"="#27ae60","Sub medie"="#e74c3c")) +
      theme_minimal(base_size=13) + labs(x=NULL,y=paste("Dif. fata de media globala (",round(gm,1),")"),fill=NULL)
  }
  
  output$plot_boxplot <- render_chart({ req(input$run,data_info()$types[[input$target]]=="Numerica"); g<-make_gg_box();    if(has_plotly) plotly::ggplotly(g) else g })
  output$plot_density <- render_chart({ req(input$run,data_info()$types[[input$target]]=="Numerica"); g<-make_gg_density();if(has_plotly) plotly::ggplotly(g) else g })
  output$plot_barplot <- render_chart({ req(input$run,data_info()$types[[input$target]]=="Numerica"); g<-make_gg_bar();    if(has_plotly) plotly::ggplotly(g) else g })
  output$plot_parity  <- render_chart({
    req(input$run, data_info()$types[[input$target]] %in% c("Binara","Categorica"))
    df <- data_final()
    g <- ggplot(df,aes(x=.data[[input$sensitive]],fill=as.factor(.data[[input$target]]))) +
      geom_bar(position="fill") + theme_minimal(base_size=13) +
      scale_y_continuous(labels=if(requireNamespace("scales",quietly=TRUE)) scales::percent else waiver()) +
      labs(y="Proportie",fill=input$target,x=input$sensitive)
    if(has_plotly) plotly::ggplotly(g) else g
  })
  
  # --- Export ---
  output$dl_boxplot <- downloadHandler(filename=function() paste0("boxplot_",Sys.Date(),".png"),
                                       content=function(f) ggplot2::ggsave(f, plot=make_gg_box(),     width=10,height=6,dpi=150))
  output$dl_density <- downloadHandler(filename=function() paste0("density_",Sys.Date(),".png"),
                                       content=function(f) ggplot2::ggsave(f, plot=make_gg_density(), width=10,height=6,dpi=150))
  output$dl_barplot <- downloadHandler(filename=function() paste0("barplot_",Sys.Date(),".png"),
                                       content=function(f) ggplot2::ggsave(f, plot=make_gg_bar(),     width=10,height=6,dpi=150))
  output$dl_report  <- downloadHandler(
    filename = function() paste0("raport_disparitati_",Sys.Date(),".csv"),
    content  = function(f) {
      req(metrics_result(), bias_result())
      mr <- metrics_result(); br <- bias_result()
      lines <- c(paste0("Data,",Sys.Date()), paste0("Sensibil,",input$sensitive),
                 paste0("Target,",input$target), paste0("Bias Score,",br$bias_score),
                 paste0("Severitate,",br$severity), "")
      if (!is.null(mr$cohen_d))
        lines <- c(lines, paste0("Cohen d,",mr$cohen_d), paste0("Interpretare,",mr$cohen_d_interpretation),
                   paste0("Dif medie,",mr$mean_diff), paste0("Dif %,",mr$pct_diff),
                   paste0("t-stat,",mr$t_stat), paste0("p t-test,",mr$p_value_ttest),
                   paste0("F-stat,",mr$f_stat), paste0("p ANOVA,",mr$p_value_anova))
      if (!is.null(mr$spd))
        lines <- c(lines, paste0("SPD,",mr$spd), paste0("Disparate Impact,",mr$disparate_impact),
                   paste0("Risk Ratio,",mr$risk_ratio))
      writeLines(lines, f)
    }
  )
}

shinyApp(ui, server)