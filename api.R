# ============================================================
# api.R — API Plumber: Detectarea Disparităților Socio-Economice
# ============================================================

library(plumber)
library(readxl)
library(jsonlite)

# ── Store global: fișierele încărcate trăiesc aici ───────────
file_store <- new.env(parent = emptyenv())

# ── Helper: generează un ID unic de 12 caractere ─────────────
new_file_id <- function() {
  paste0(sample(c(letters, 0:9), 12, replace = TRUE), collapse = "")
}

# ── Helper: detectează formatul după extensie ─────────────────
detect_format <- function(filename) {
  ext <- tolower(tools::file_ext(filename))
  if (ext == "csv")               return("csv")
  if (ext %in% c("xlsx", "xls")) return("excel")
  NULL
}

# ── Helper: citește fișierul indiferent de cum vine din Plumber
#   (unele versiuni dau $datapath, altele dau $value ca raw bytes)
read_uploaded_file <- function(file_obj, format) {
  if (!is.null(file_obj$datapath)) {
    path    <- file_obj$datapath
    cleanup <- FALSE
  } else {
    ext  <- if (format == "csv") "csv" else "xlsx"
    path <- tempfile(fileext = paste0(".", ext))
    writeBin(file_obj$value, path)
    cleanup <- TRUE
  }
  
  df <- tryCatch({
    if (format == "csv") {
      tryCatch(
        read.csv(path, stringsAsFactors = FALSE, encoding  = "UTF-8"),
        error = function(e)
          read.csv(path, stringsAsFactors = FALSE, fileEncoding = "latin1")
      )
    } else {
      as.data.frame(readxl::read_excel(path))
    }
  }, error = function(e) NULL)
  
  if (cleanup) unlink(path)
  df
}

# ── Helper: transformă dataframe în listă JSON-safe (gestionează NA) ──
df_to_json_safe <- function(df) {
  jsonlite::fromJSON(
    jsonlite::toJSON(df, na = "null", auto_unbox = FALSE)
  )
}

# ── Keywords pentru detecție automată ────────────────────────
SENSITIVE_KEYWORDS <- c(
  "gen", "sex", "gender", "varsta", "vârsta", "vîrsta", "age",
  "etnie", "etnia", "ethnicity", "race", "rasa", "religie", "religion",
  "regiune", "region", "nationalitate", "nationality",
  "handicap", "disability", "educatie", "educație", "education"
)

FINANCIAL_KEYWORDS <- c(
  "salariu", "salary", "wage", "salarii",
  "venit", "venituri", "income",
  "pensie", "pensii", "pension",
  "castig", "câștig", "earning",
  "plata", "plată", "pay", "pay_gap",
  "remuneratie", "remunerație", "remuneration",
  "indemnizatie", "indemnizație"
)

# ── Helper: detectează tipul unei coloane ────────────────────
detect_col_type <- function(col_values) {
  non_na <- col_values[!is.na(col_values)]
  if (length(non_na) == 0) return("unknown")
  if (is.numeric(col_values)) return("numeric")
  n_unique <- length(unique(non_na))
  if (n_unique <= 2) return("binary")
  return("categorical")
}

# ── Helper: verifică dacă numele coloanei conține un keyword ──
matches_keywords <- function(col_name, keywords) {
  col_lower <- tolower(col_name)
  any(sapply(keywords, function(kw) grepl(kw, col_lower, fixed = TRUE)))
}

# ── Helper Grup 3: validare și extragere grupuri numerice ─────
get_numeric_groups <- function(file_id, sensitive_col, target_col) {
  entry <- file_store[[file_id]]
  if (is.null(entry))
    return(list(error = sprintf("file_id '%s' nu există sau sesiunea a expirat.", file_id)))
  
  df <- entry$df
  
  if (!sensitive_col %in% colnames(df))
    return(list(error = sprintf("Coloana '%s' nu există în fișier.", sensitive_col)))
  if (!target_col %in% colnames(df))
    return(list(error = sprintf("Coloana '%s' nu există în fișier.", target_col)))
  if (!is.numeric(df[[target_col]]))
    return(list(error = sprintf(
      "Coloana '%s' nu este numerică. Pentru target binar folosiți /metrics/spd sau /metrics/disparate-impact.",
      target_col
    )))
  
  valid_rows <- !is.na(df[[sensitive_col]]) & !is.na(df[[target_col]])
  df_clean   <- df[valid_rows, ]
  
  if (nrow(df_clean) < 4)
    return(list(error = "Date insuficiente după eliminarea valorilor lipsă (minimum 4 rânduri valide)."))
  
  groups <- split(df_clean[[target_col]], as.character(df_clean[[sensitive_col]]))
  groups <- Filter(function(g) length(g) > 0, groups)
  
  list(error = NULL, groups = groups, n_groups = length(groups), df_clean = df_clean)
}

# ── Helper: magnitudinea Cohen's d ───────────────────────────
cohens_d_magnitude <- function(d) {
  d <- abs(d)
  if (d < 0.2) return("neglijabil")
  if (d < 0.5) return("mic")
  if (d < 0.8) return("mediu")
  return("mare")
}

# ============================================================
# METADATA API
# ============================================================

#* @apiTitle Detectarea Disparităților Socio-Economice
#* @apiDescription REST API pentru analiza bias-ului și disparităților în date socio-economice
#* @apiVersion 1.0.0

# ============================================================
# FILTER CORS
# ============================================================

#* @filter cors
function(req, res) {
  res$setHeader("Access-Control-Allow-Origin",  "*")
  res$setHeader("Access-Control-Allow-Methods", "GET, POST, DELETE, OPTIONS")
  res$setHeader("Access-Control-Allow-Headers", "Content-Type")
  if (req$REQUEST_METHOD == "OPTIONS") {
    res$status <- 200
    return(list())
  }
  plumber::forward()
}

# ============================================================
# GRUP 1 — FILE MANAGEMENT
# ============================================================

#* Încarcă un fișier CSV sau Excel și returnează un file_id
#* @tag File Management
#* @post /upload
#* @parser multi
#* @param file:file Fișierul de încărcat (.csv, .xlsx sau .xls)
#* @serializer json
function(req, res) {
  file_obj <- req$body$file
  
  if (is.null(file_obj)) {
    res$status <- 400
    return(list(error = "Câmpul 'file' lipsește. Trimite fișierul ca multipart/form-data."))
  }
  
  filename <- file_obj$filename
  if (is.null(filename) || nchar(trimws(filename)) == 0) {
    res$status <- 400
    return(list(error = "Numele fișierului nu a putut fi detectat."))
  }
  
  format <- detect_format(filename)
  if (is.null(format)) {
    res$status <- 415
    return(list(error = sprintf(
      "Format nesuportat: '.%s'. Acceptăm doar .csv, .xlsx, .xls.",
      tools::file_ext(filename)
    )))
  }
  
  df <- read_uploaded_file(file_obj, format)
  
  if (is.null(df)) {
    res$status <- 422
    return(list(error = "Fișierul nu a putut fi citit. Verificați că nu este corupt."))
  }
  if (nrow(df) == 0) {
    res$status <- 422
    return(list(error = "Fișierul este gol (0 rânduri de date)."))
  }
  if (ncol(df) < 2) {
    res$status <- 422
    return(list(error = "Fișierul trebuie să aibă cel puțin 2 coloane."))
  }
  
  file_id <- new_file_id()
  file_store[[file_id]] <- list(
    df       = df,
    filename = filename,
    format   = format,
    uploaded = as.character(Sys.time())
  )
  
  list(
    file_id   = file_id,
    filename  = filename,
    format    = format,
    rows      = nrow(df),
    cols      = ncol(df),
    col_names = as.list(colnames(df))
  )
}

#* Previzualizare primele N rânduri dintr-un fișier încărcat
#* @tag File Management
#* @get /files/<file_id>/preview
#* @param n:int Număr de rânduri (1-100, default 5)
#* @serializer json
function(file_id, n = 5, res) {
  entry <- file_store[[file_id]]
  if (is.null(entry)) {
    res$status <- 404
    return(list(error = sprintf("file_id '%s' nu există sau sesiunea a expirat.", file_id)))
  }
  
  n          <- max(1L, min(as.integer(n), 100L))
  df_preview <- head(entry$df, n)
  
  list(
    file_id    = file_id,
    filename   = entry$filename,
    uploaded   = entry$uploaded,
    rows_total = nrow(entry$df),
    cols_total = ncol(entry$df),
    rows_shown = nrow(df_preview),
    data       = df_to_json_safe(df_preview)
  )
}

#* Șterge un fișier din memorie
#* @tag File Management
#* @delete /files/<file_id>
#* @serializer json
function(file_id, res) {
  entry <- file_store[[file_id]]
  if (is.null(entry)) {
    res$status <- 404
    return(list(error = sprintf("file_id '%s' nu există.", file_id)))
  }
  
  filename <- entry$filename
  rm(list = file_id, envir = file_store)
  
  list(
    success = TRUE,
    message = sprintf("Fișierul '%s' (id: %s) a fost șters din memorie.", filename, file_id)
  )
}

# ============================================================
# GRUP 2 — DATA PROFILING
# ============================================================

#* Profilează toate coloanele unui fișier încărcat
#* @tag Data Profiling
#* @post /profile
#* @param file_id ID-ul fișierului returnat de /upload
#* @serializer json
function(file_id, res) {
  entry <- file_store[[file_id]]
  if (is.null(entry)) {
    res$status <- 404
    return(list(error = sprintf("file_id '%s' nu există sau sesiunea a expirat.", file_id)))
  }
  
  df      <- entry$df
  columns <- lapply(colnames(df), function(col_name) {
    col_values  <- df[[col_name]]
    non_na      <- col_values[!is.na(col_values)]
    unique_vals <- unique(non_na)
    
    list(
      name          = col_name,
      detected_type = detect_col_type(col_values),
      is_sensitive  = matches_keywords(col_name, SENSITIVE_KEYWORDS),
      is_financial  = matches_keywords(col_name, FINANCIAL_KEYWORDS),
      missing_count = sum(is.na(col_values)),
      missing_pct   = round(sum(is.na(col_values)) / length(col_values) * 100, 2),
      unique_values = length(unique_vals),
      sample_values = as.list(head(as.character(unique_vals), 5))
    )
  })
  
  list(
    file_id  = file_id,
    filename = entry$filename,
    rows     = nrow(df),
    cols     = ncol(df),
    columns  = columns
  )
}

#* Validează atributele selectate pentru analiză
#* @tag Data Profiling
#* @post /validate
#* @param file_id ID-ul fișierului
#* @param sensitive_col Numele coloanei atribut sensibil (trebuie să fie binary sau categorical)
#* @param target_col Numele coloanei target (trebuie să fie numeric sau binary)
#* @serializer json
function(file_id, sensitive_col, target_col, res) {
  entry <- file_store[[file_id]]
  if (is.null(entry)) {
    res$status <- 404
    return(list(error = sprintf("file_id '%s' nu există sau sesiunea a expirat.", file_id)))
  }
  
  df     <- entry$df
  errors <- character(0)
  
  if (!sensitive_col %in% colnames(df))
    errors <- c(errors, sprintf("Coloana '%s' nu există în fișier.", sensitive_col))
  if (!target_col %in% colnames(df))
    errors <- c(errors, sprintf("Coloana '%s' nu există în fișier.", target_col))
  
  if (length(errors) > 0) {
    res$status <- 400
    return(list(valid = FALSE, errors = as.list(errors)))
  }
  
  if (sensitive_col == target_col) {
    res$status <- 400
    return(list(
      valid  = FALSE,
      errors = list("Atributul sensibil și target-ul nu pot fi aceeași coloană.")
    ))
  }
  
  sensitive_type <- detect_col_type(df[[sensitive_col]])
  target_type    <- detect_col_type(df[[target_col]])
  
  if (!sensitive_type %in% c("binary", "categorical"))
    errors <- c(errors, sprintf(
      "Coloana '%s' este '%s'. Atributul sensibil trebuie să fie 'binary' sau 'categorical'.",
      sensitive_col, sensitive_type
    ))
  if (!target_type %in% c("numeric", "binary"))
    errors <- c(errors, sprintf(
      "Coloana '%s' este '%s'. Target-ul trebuie să fie 'numeric' sau 'binary'.",
      target_col, target_type
    ))
  
  list(
    valid          = length(errors) == 0,
    sensitive_col  = sensitive_col,
    sensitive_type = sensitive_type,
    target_col     = target_col,
    target_type    = target_type,
    errors         = as.list(errors)
  )
}

# ============================================================
# GRUP 3 — METRICI PENTRU TARGET NUMERIC
# ============================================================

#* Statistici descriptive per grup (medie, mediană, deviație standard)
#* @tag Metrici Numerice
#* @post /metrics/descriptive
#* @param file_id ID-ul fișierului
#* @param sensitive_col Coloana atribut sensibil (ex: sex, regiune)
#* @param target_col Coloana numerică analizată (ex: salariu)
#* @serializer json
function(file_id, sensitive_col, target_col, res) {
  r <- get_numeric_groups(file_id, sensitive_col, target_col)
  if (!is.null(r$error)) { res$status <- 400; return(list(error = r$error)) }
  
  groups_info <- lapply(names(r$groups), function(g) {
    vals <- r$groups[[g]]
    list(
      group  = g,
      count  = length(vals),
      mean   = round(mean(vals), 4),
      median = round(median(vals), 4),
      std    = round(sd(vals), 4),
      min    = round(min(vals), 4),
      max    = round(max(vals), 4)
    )
  })
  
  list(
    file_id       = file_id,
    sensitive_col = sensitive_col,
    target_col    = target_col,
    n_groups      = r$n_groups,
    groups        = groups_info
  )
}

#* Diferența mediilor între două grupuri (absolută și procentuală)
#* @tag Metrici Numerice
#* @post /metrics/mean-diff
#* @param file_id ID-ul fișierului
#* @param sensitive_col Coloana atribut sensibil (exact 2 grupuri)
#* @param target_col Coloana numerică analizată
#* @serializer json
function(file_id, sensitive_col, target_col, res) {
  r <- get_numeric_groups(file_id, sensitive_col, target_col)
  if (!is.null(r$error)) { res$status <- 400; return(list(error = r$error)) }
  
  if (r$n_groups != 2) {
    res$status <- 400
    return(list(error = sprintf(
      "Această metrică necesită exact 2 grupuri. Coloana '%s' are %d grupuri. Folosiți /metrics/anova pentru mai multe grupuri.",
      sensitive_col, r$n_groups
    )))
  }
  
  g  <- names(r$groups)
  m1 <- mean(r$groups[[g[1]]])
  m2 <- mean(r$groups[[g[2]]])
  
  list(
    file_id       = file_id,
    sensitive_col = sensitive_col,
    target_col    = target_col,
    group1        = g[1],
    group2        = g[2],
    mean1         = round(m1, 4),
    mean2         = round(m2, 4),
    abs_diff      = round(m1 - m2, 4),
    pct_diff      = round((m1 - m2) / abs(m2) * 100, 2)
  )
}

#* Cohen's d — mărimea efectului standardizată între două grupuri
#* @tag Metrici Numerice
#* @post /metrics/cohens-d
#* @param file_id ID-ul fișierului
#* @param sensitive_col Coloana atribut sensibil (exact 2 grupuri)
#* @param target_col Coloana numerică analizată
#* @serializer json
function(file_id, sensitive_col, target_col, res) {
  r <- get_numeric_groups(file_id, sensitive_col, target_col)
  if (!is.null(r$error)) { res$status <- 400; return(list(error = r$error)) }
  
  if (r$n_groups != 2) {
    res$status <- 400
    return(list(error = sprintf(
      "Cohen's d necesită exact 2 grupuri. Coloana '%s' are %d grupuri.",
      sensitive_col, r$n_groups
    )))
  }
  
  g  <- names(r$groups)
  x1 <- r$groups[[g[1]]]; x2 <- r$groups[[g[2]]]
  n1 <- length(x1);       n2 <- length(x2)
  
  sd_pooled <- sqrt(((n1 - 1) * var(x1) + (n2 - 1) * var(x2)) / (n1 + n2 - 2))
  d         <- (mean(x1) - mean(x2)) / sd_pooled
  
  list(
    file_id       = file_id,
    sensitive_col = sensitive_col,
    target_col    = target_col,
    group1        = g[1],
    group2        = g[2],
    cohens_d      = round(d, 4),
    cohens_d_abs  = round(abs(d), 4),
    magnitude     = cohens_d_magnitude(d),
    sd_pooled     = round(sd_pooled, 4),
    thresholds    = list(neglijabil = 0.2, mic = 0.5, mediu = 0.8)
  )
}

#* Welch t-test — t-statistic și p-value pentru 2 grupuri
#* @tag Metrici Numerice
#* @post /metrics/welch-ttest
#* @param file_id ID-ul fișierului
#* @param sensitive_col Coloana atribut sensibil (exact 2 grupuri)
#* @param target_col Coloana numerică analizată
#* @serializer json
function(file_id, sensitive_col, target_col, res) {
  r <- get_numeric_groups(file_id, sensitive_col, target_col)
  if (!is.null(r$error)) { res$status <- 400; return(list(error = r$error)) }
  
  if (r$n_groups != 2) {
    res$status <- 400
    return(list(error = sprintf(
      "Welch t-test necesită exact 2 grupuri. Coloana '%s' are %d grupuri. Folosiți /metrics/anova.",
      sensitive_col, r$n_groups
    )))
  }
  
  g  <- names(r$groups)
  tt <- t.test(r$groups[[g[1]]], r$groups[[g[2]]], var.equal = FALSE)
  
  list(
    file_id             = file_id,
    sensitive_col       = sensitive_col,
    target_col          = target_col,
    group1              = g[1],
    group2              = g[2],
    t_statistic         = round(unname(tt$statistic), 4),
    p_value             = round(tt$p.value, 6),
    degrees_of_freedom  = round(unname(tt$parameter), 2),
    significant         = tt$p.value < 0.05,
    alpha               = 0.05,
    confidence_interval = list(
      lower = round(tt$conf.int[1], 4),
      upper = round(tt$conf.int[2], 4)
    )
  )
}

#* ANOVA Welch — F-statistic și p-value pentru 3 sau mai multe grupuri
#* @tag Metrici Numerice
#* @post /metrics/anova
#* @param file_id ID-ul fișierului
#* @param sensitive_col Coloana atribut sensibil (minim 2 grupuri)
#* @param target_col Coloana numerică analizată
#* @serializer json
function(file_id, sensitive_col, target_col, res) {
  r <- get_numeric_groups(file_id, sensitive_col, target_col)
  if (!is.null(r$error)) { res$status <- 400; return(list(error = r$error)) }
  
  if (r$n_groups < 2) {
    res$status <- 400
    return(list(error = "ANOVA necesită cel puțin 2 grupuri."))
  }
  
  df_anova  <- data.frame(
    value = r$df_clean[[target_col]],
    group = as.factor(r$df_clean[[sensitive_col]])
  )
  anova_res <- oneway.test(value ~ group, data = df_anova, var.equal = FALSE)
  
  list(
    file_id        = file_id,
    sensitive_col  = sensitive_col,
    target_col     = target_col,
    n_groups       = r$n_groups,
    groups         = as.list(names(r$groups)),
    f_statistic    = round(unname(anova_res$statistic), 4),
    p_value        = round(anova_res$p.value, 6),
    df_numerator   = round(unname(anova_res$parameter[1]), 2),
    df_denominator = round(unname(anova_res$parameter[2]), 2),
    significant    = anova_res$p.value < 0.05,
    alpha          = 0.05,
    method         = "Welch one-way ANOVA (variante neegale)"
  )
}