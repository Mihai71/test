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

# ── Helper: transformă dataframe în listă JSON-safe ───────────
df_to_json_safe <- function(df) {
  jsonlite::fromJSON(jsonlite::toJSON(df, na = "null", auto_unbox = FALSE))
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

POSITIVE_KEYWORDS <- c(
  "da", "yes", "true", "1", "aprobat", "promovat", "admis",
  "acceptat", "success", "succes", "pozitiv", "activ", "valid", "ok"
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

# ── Helper: detectează automat valoarea pozitivă ──────────────
detect_positive_value <- function(values) {
  vals_str   <- as.character(values)
  vals_lower <- tolower(vals_str)
  for (kw in POSITIVE_KEYWORDS) {
    match_idx <- which(vals_lower == kw)
    if (length(match_idx) > 0) return(vals_str[match_idx[1]])
  }
  sort(vals_str)[2]
}

# ── Helper: extrage grupuri numerice din fișier ───────────────
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

# ── Helper: filtrează la exact 2 grupuri pentru metrici pereche
filter_two_groups <- function(groups, group1, group2, sensitive_col) {
  available <- paste(names(groups), collapse = ", ")
  
  if (!is.null(group1) && !is.null(group2) &&
      trimws(group1) != "" && trimws(group2) != "") {
    if (!group1 %in% names(groups))
      return(list(error = sprintf(
        "group1 '%s' nu există. Grupuri disponibile: %s", group1, available)))
    if (!group2 %in% names(groups))
      return(list(error = sprintf(
        "group2 '%s' nu există. Grupuri disponibile: %s", group2, available)))
    if (group1 == group2)
      return(list(error = "group1 și group2 trebuie să fie diferite."))
    return(list(error = NULL, g1 = group1, g2 = group2,
                groups = groups[c(group1, group2)]))
  }
  
  if (length(groups) != 2)
    return(list(error = sprintf(
      "Coloana '%s' are %d grupuri (%s). Specificați 'group1' și 'group2' pentru a selecta 2 grupuri de comparat.",
      sensitive_col, length(groups), available
    )))
  
  list(error = NULL, g1 = names(groups)[1], g2 = names(groups)[2], groups = groups)
}

# ── Helper: extrage proporții per grup pentru target binar ────
get_binary_groups <- function(file_id, sensitive_col, target_col,
                              positive_value = NULL,
                              group1 = NULL, group2 = NULL) {
  entry <- file_store[[file_id]]
  if (is.null(entry))
    return(list(error = sprintf("file_id '%s' nu există sau sesiunea a expirat.", file_id)))
  
  df <- entry$df
  
  if (!sensitive_col %in% colnames(df))
    return(list(error = sprintf("Coloana '%s' nu există în fișier.", sensitive_col)))
  if (!target_col %in% colnames(df))
    return(list(error = sprintf("Coloana '%s' nu există în fișier.", target_col)))
  
  valid_rows <- !is.na(df[[sensitive_col]]) & !is.na(df[[target_col]])
  df_clean   <- df[valid_rows, ]
  
  if (nrow(df_clean) < 4)
    return(list(error = "Date insuficiente după eliminarea valorilor lipsă (minimum 4 rânduri valide)."))
  
  target_vals   <- as.character(df_clean[[target_col]])
  unique_target <- unique(target_vals)
  
  if (length(unique_target) != 2)
    return(list(error = sprintf(
      "Coloana '%s' trebuie să fie binară (exact 2 valori unice). Găsite: %d valori (%s).",
      target_col, length(unique_target), paste(head(unique_target, 5), collapse = ", ")
    )))
  
  if (is.null(positive_value) || trimws(positive_value) == "") {
    positive_value <- detect_positive_value(unique_target)
  } else if (!positive_value %in% unique_target) {
    return(list(error = sprintf(
      "Valoarea pozitivă '%s' nu există în coloana '%s'. Valori disponibile: %s",
      positive_value, target_col, paste(unique_target, collapse = ", ")
    )))
  }
  
  sens_vals    <- as.character(df_clean[[sensitive_col]])
  all_groups   <- unique(sens_vals)
  available    <- paste(all_groups, collapse = ", ")
  
  if (!is.null(group1) && !is.null(group2) &&
      trimws(group1) != "" && trimws(group2) != "") {
    if (!group1 %in% all_groups)
      return(list(error = sprintf("group1 '%s' nu există. Grupuri disponibile: %s", group1, available)))
    if (!group2 %in% all_groups)
      return(list(error = sprintf("group2 '%s' nu există. Grupuri disponibile: %s", group2, available)))
    if (group1 == group2)
      return(list(error = "group1 și group2 trebuie să fie diferite."))
    sel_groups <- c(group1, group2)
  } else if (length(all_groups) == 2) {
    sel_groups <- all_groups
  } else {
    return(list(error = sprintf(
      "Coloana '%s' are %d grupuri (%s). Specificați 'group1' și 'group2' pentru a selecta 2 grupuri de comparat.",
      sensitive_col, length(all_groups), available
    )))
  }
  
  props <- lapply(sel_groups, function(g) {
    mask       <- sens_vals == g
    n_total    <- sum(mask)
    n_positive <- sum(target_vals[mask] == positive_value)
    list(group = g, count = n_total, n_positive = n_positive,
         proportion = n_positive / n_total)
  })
  names(props) <- sel_groups
  
  p_vals     <- sapply(props, function(x) x$proportion)
  ord        <- order(p_vals, decreasing = TRUE)
  privileged <- props[[sel_groups[ord[1]]]]
  protected  <- props[[sel_groups[ord[2]]]]
  
  list(error = NULL, privileged = privileged, protected = protected,
       positive_value = positive_value, n_groups = length(sel_groups))
}

# ── Helper: magnitudinea Cohen's d ───────────────────────────
cohens_d_magnitude <- function(d) {
  d <- abs(d)
  if (d < 0.2) return("neglijabil")
  if (d < 0.5) return("mic")
  if (d < 0.8) return("mediu")
  return("mare")
}

# ── Helper: eta-squared (η²) pentru 3+ grupuri numerice ──────
compute_eta_squared <- function(groups) {
  all_vals   <- unlist(groups)
  grand_mean <- mean(all_vals)
  ss_total   <- sum((all_vals - grand_mean)^2)
  if (ss_total == 0) return(0)
  ss_between <- sum(sapply(groups, function(g) length(g) * (mean(g) - grand_mean)^2))
  min(ss_between / ss_total, 1.0)
}

# ── Helper: skewness calculat manual ─────────────────────────
compute_skewness <- function(x) {
  n  <- length(x)
  mu <- mean(x)
  s  <- sd(x)
  if (s == 0 || n < 3) return(0)
  sum((x - mu)^3) / (n * s^3)
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
#* @param sensitive_col Coloana atribut sensibil (binary sau categorical — inclusiv 3+ grupuri)
#* @param target_col Coloana target (numeric sau binary)
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
    return(list(valid = FALSE,
                errors = list("Atributul sensibil și target-ul nu pot fi aceeași coloană.")))
  }
  
  sensitive_type <- detect_col_type(df[[sensitive_col]])
  target_type    <- detect_col_type(df[[target_col]])
  n_sens_groups  <- length(unique(df[[sensitive_col]][!is.na(df[[sensitive_col]])]))
  
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
    n_sens_groups  = n_sens_groups,
    target_col     = target_col,
    target_type    = target_type,
    note           = if (n_sens_groups > 2)
      "Atribut cu 3+ grupuri: pentru metrici pereche specificați 'group1' și 'group2'."
    else NULL,
    errors         = as.list(errors)
  )
}

# ============================================================
# GRUP 3 — METRICI PENTRU TARGET NUMERIC
# ============================================================

#* Statistici descriptive per grup — funcționează cu orice număr de grupuri
#* @tag Metrici Numerice
#* @post /metrics/descriptive
#* @param file_id ID-ul fișierului
#* @param sensitive_col Coloana atribut sensibil (orice număr de grupuri)
#* @param target_col Coloana numerică analizată
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
#* @param sensitive_col Coloana atribut sensibil
#* @param target_col Coloana numerică analizată
#* @param group1 Primul grup de comparat (obligatoriu dacă sensitive_col are 3+ grupuri)
#* @param group2 Al doilea grup de comparat (obligatoriu dacă sensitive_col are 3+ grupuri)
#* @serializer json
function(file_id, sensitive_col, target_col, group1 = NULL, group2 = NULL, res) {
  r <- get_numeric_groups(file_id, sensitive_col, target_col)
  if (!is.null(r$error)) { res$status <- 400; return(list(error = r$error)) }
  
  f <- filter_two_groups(r$groups, group1, group2, sensitive_col)
  if (!is.null(f$error)) { res$status <- 400; return(list(error = f$error)) }
  
  m1 <- mean(f$groups[[f$g1]])
  m2 <- mean(f$groups[[f$g2]])
  
  list(
    file_id       = file_id,
    sensitive_col = sensitive_col,
    target_col    = target_col,
    group1        = f$g1,
    group2        = f$g2,
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
#* @param sensitive_col Coloana atribut sensibil
#* @param target_col Coloana numerică analizată
#* @param group1 Primul grup de comparat (obligatoriu dacă sensitive_col are 3+ grupuri)
#* @param group2 Al doilea grup de comparat (obligatoriu dacă sensitive_col are 3+ grupuri)
#* @serializer json
function(file_id, sensitive_col, target_col, group1 = NULL, group2 = NULL, res) {
  r <- get_numeric_groups(file_id, sensitive_col, target_col)
  if (!is.null(r$error)) { res$status <- 400; return(list(error = r$error)) }
  
  f <- filter_two_groups(r$groups, group1, group2, sensitive_col)
  if (!is.null(f$error)) { res$status <- 400; return(list(error = f$error)) }
  
  x1 <- f$groups[[f$g1]]; x2 <- f$groups[[f$g2]]
  n1 <- length(x1);       n2 <- length(x2)
  
  sd_pooled <- sqrt(((n1 - 1) * var(x1) + (n2 - 1) * var(x2)) / (n1 + n2 - 2))
  d         <- (mean(x1) - mean(x2)) / sd_pooled
  
  list(
    file_id       = file_id,
    sensitive_col = sensitive_col,
    target_col    = target_col,
    group1        = f$g1,
    group2        = f$g2,
    cohens_d      = round(d, 4),
    cohens_d_abs  = round(abs(d), 4),
    magnitude     = cohens_d_magnitude(d),
    sd_pooled     = round(sd_pooled, 4),
    thresholds    = list(neglijabil = 0.2, mic = 0.5, mediu = 0.8)
  )
}

#* Welch t-test — t-statistic și p-value între două grupuri
#* @tag Metrici Numerice
#* @post /metrics/welch-ttest
#* @param file_id ID-ul fișierului
#* @param sensitive_col Coloana atribut sensibil
#* @param target_col Coloana numerică analizată
#* @param group1 Primul grup de comparat (obligatoriu dacă sensitive_col are 3+ grupuri)
#* @param group2 Al doilea grup de comparat (obligatoriu dacă sensitive_col are 3+ grupuri)
#* @serializer json
function(file_id, sensitive_col, target_col, group1 = NULL, group2 = NULL, res) {
  r <- get_numeric_groups(file_id, sensitive_col, target_col)
  if (!is.null(r$error)) { res$status <- 400; return(list(error = r$error)) }
  
  f <- filter_two_groups(r$groups, group1, group2, sensitive_col)
  if (!is.null(f$error)) { res$status <- 400; return(list(error = f$error)) }
  
  tt <- t.test(f$groups[[f$g1]], f$groups[[f$g2]], var.equal = FALSE)
  
  list(
    file_id             = file_id,
    sensitive_col       = sensitive_col,
    target_col          = target_col,
    group1              = f$g1,
    group2              = f$g2,
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

#* ANOVA Welch — F-statistic și p-value pentru orice număr de grupuri
#* @tag Metrici Numerice
#* @post /metrics/anova
#* @param file_id ID-ul fișierului
#* @param sensitive_col Coloana atribut sensibil (orice număr de grupuri)
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

# ============================================================
# GRUP 4 — METRICI PENTRU TARGET BINAR
# ============================================================

#* Statistical Parity Difference — diferența proporțiilor între două grupuri
#* @tag Metrici Binare
#* @post /metrics/spd
#* @param file_id ID-ul fișierului
#* @param sensitive_col Coloana atribut sensibil
#* @param target_col Coloana binară analizată
#* @param positive_value Valoarea considerată succes (opțional, detectată automat)
#* @param group1 Primul grup de comparat (obligatoriu dacă sensitive_col are 3+ grupuri)
#* @param group2 Al doilea grup de comparat (obligatoriu dacă sensitive_col are 3+ grupuri)
#* @serializer json
function(file_id, sensitive_col, target_col,
         positive_value = NULL, group1 = NULL, group2 = NULL, res) {
  r <- get_binary_groups(file_id, sensitive_col, target_col,
                         positive_value, group1, group2)
  if (!is.null(r$error)) { res$status <- 400; return(list(error = r$error)) }
  
  spd <- r$privileged$proportion - r$protected$proportion
  
  list(
    file_id          = file_id,
    sensitive_col    = sensitive_col,
    target_col       = target_col,
    positive_value   = r$positive_value,
    group_privileged = list(
      name       = r$privileged$group,
      count      = r$privileged$count,
      n_positive = r$privileged$n_positive,
      proportion = round(r$privileged$proportion, 4)
    ),
    group_protected  = list(
      name       = r$protected$group,
      count      = r$protected$count,
      n_positive = r$protected$n_positive,
      proportion = round(r$protected$proportion, 4)
    ),
    spd            = round(spd, 4),
    spd_pct        = round(spd * 100, 2),
    equitable      = abs(spd) < 0.1,
    interpretation = if (abs(spd) < 0.1) "echitabil"
    else if (spd > 0) "grupul protejat are rata de succes mai mică"
    else "grupul protejat are rata de succes mai mare"
  )
}

#* Disparate Impact — raportul proporțiilor între două grupuri (regula 80%)
#* @tag Metrici Binare
#* @post /metrics/disparate-impact
#* @param file_id ID-ul fișierului
#* @param sensitive_col Coloana atribut sensibil
#* @param target_col Coloana binară analizată
#* @param positive_value Valoarea considerată succes (opțional, detectată automat)
#* @param group1 Primul grup de comparat (obligatoriu dacă sensitive_col are 3+ grupuri)
#* @param group2 Al doilea grup de comparat (obligatoriu dacă sensitive_col are 3+ grupuri)
#* @serializer json
function(file_id, sensitive_col, target_col,
         positive_value = NULL, group1 = NULL, group2 = NULL, res) {
  r <- get_binary_groups(file_id, sensitive_col, target_col,
                         positive_value, group1, group2)
  if (!is.null(r$error)) { res$status <- 400; return(list(error = r$error)) }
  
  p_priv <- r$privileged$proportion
  p_prot <- r$protected$proportion
  
  if (p_priv == 0) {
    res$status <- 422
    return(list(error = "Grupul privilegiat are proporție 0 — Disparate Impact nu poate fi calculat."))
  }
  
  di <- p_prot / p_priv
  
  list(
    file_id          = file_id,
    sensitive_col    = sensitive_col,
    target_col       = target_col,
    positive_value   = r$positive_value,
    group_privileged = list(
      name       = r$privileged$group,
      count      = r$privileged$count,
      proportion = round(p_priv, 4)
    ),
    group_protected  = list(
      name       = r$protected$group,
      count      = r$protected$count,
      proportion = round(p_prot, 4)
    ),
    disparate_impact = round(di, 4),
    risk_ratio       = round(di, 4),
    interpretation   = if (di >= 0.8 && di <= 1.25) "echitabil"
    else if (di < 0.8) "risc de discriminare"
    else "favorizare inversă",
    equitable        = di >= 0.8 && di <= 1.25,
    rule_80_pct      = list(lower = 0.8, upper = 1.25)
  )
}

# ============================================================
# GRUP 5 — ALERTE DE CALITATE DATE
# ============================================================

#* Analiză distribuție target: skewness și outlieri IQR
#* @tag Alerte Calitate
#* @post /alerts/distribution
#* @param file_id ID-ul fișierului
#* @param target_col Coloana numerică analizată
#* @serializer json
function(file_id, target_col, res) {
  entry <- file_store[[file_id]]
  if (is.null(entry)) {
    res$status <- 404
    return(list(error = sprintf("file_id '%s' nu există sau sesiunea a expirat.", file_id)))
  }
  
  df <- entry$df
  
  if (!target_col %in% colnames(df)) {
    res$status <- 400
    return(list(error = sprintf("Coloana '%s' nu există în fișier.", target_col)))
  }
  if (!is.numeric(df[[target_col]])) {
    res$status <- 400
    return(list(error = sprintf("Coloana '%s' nu este numerică.", target_col)))
  }
  
  x <- df[[target_col]][!is.na(df[[target_col]])]
  
  if (length(x) < 4) {
    res$status <- 422
    return(list(error = "Date insuficiente pentru analiză distribuție (minimum 4 valori non-NA)."))
  }
  
  skew       <- compute_skewness(x)
  skew_level <- if (abs(skew) <= 0.5) "simetrica"
  else if (abs(skew) <= 1) "asimetrie_moderata"
  else "asimetrie_puternica"
  
  q1            <- unname(quantile(x, 0.25))
  q3            <- unname(quantile(x, 0.75))
  iqr           <- q3 - q1
  lower_fence   <- q1 - 1.5 * iqr
  upper_fence   <- q3 + 1.5 * iqr
  outlier_count <- sum(x < lower_fence | x > upper_fence)
  outlier_pct   <- outlier_count / length(x) * 100
  
  list(
    file_id          = file_id,
    target_col       = target_col,
    n_values         = length(x),
    skewness         = round(skew, 4),
    skew_level       = skew_level,
    q1               = round(q1, 4),
    q3               = round(q3, 4),
    iqr              = round(iqr, 4),
    lower_fence      = round(lower_fence, 4),
    upper_fence      = round(upper_fence, 4),
    outlier_count    = outlier_count,
    outlier_pct      = round(outlier_pct, 2),
    outlier_severity = if (outlier_pct > 10) "critica"
    else if (outlier_pct > 5) "atentie"
    else "ok",
    thresholds = list(skew_moderat = 0.5, skew_puternic = 1.0,
                      outlier_atentie = 5.0, outlier_critica = 10.0)
  )
}

#* Dezechilibru distribuțional — detectează grupuri sub-reprezentate
#* @tag Alerte Calitate
#* @post /alerts/imbalance
#* @param file_id ID-ul fișierului
#* @param sensitive_col Coloana atribut sensibil (orice număr de grupuri)
#* @serializer json
function(file_id, sensitive_col, res) {
  entry <- file_store[[file_id]]
  if (is.null(entry)) {
    res$status <- 404
    return(list(error = sprintf("file_id '%s' nu există sau sesiunea a expirat.", file_id)))
  }
  
  df <- entry$df
  
  if (!sensitive_col %in% colnames(df)) {
    res$status <- 400
    return(list(error = sprintf("Coloana '%s' nu există în fișier.", sensitive_col)))
  }
  
  vals    <- df[[sensitive_col]][!is.na(df[[sensitive_col]])]
  n_total <- length(vals)
  
  if (n_total < 4) {
    res$status <- 422
    return(list(error = "Date insuficiente pentru analiză dezechilibru (minimum 4 valori non-NA)."))
  }
  
  counts  <- table(vals)
  props   <- counts / n_total
  min_idx <- which.min(props)
  min_prop <- unname(props[min_idx])
  
  groups_info <- lapply(names(counts), function(g) {
    list(group      = g,
         count      = unname(counts[g]),
         proportion = round(unname(props[g]), 4),
         alert      = unname(props[g]) < 0.20)
  })
  
  list(
    file_id        = file_id,
    sensitive_col  = sensitive_col,
    n_total        = n_total,
    n_groups       = length(counts),
    groups         = groups_info,
    min_group      = names(props)[min_idx],
    min_proportion = round(min_prop, 4),
    imbalanced     = min_prop < 0.20,
    severity       = if (min_prop < 0.20) "critica" else "ok",
    threshold      = 0.20
  )
}

#* Raport valori lipsă per coloană
#* @tag Alerte Calitate
#* @get /alerts/missing/<file_id>
#* @serializer json
function(file_id, res) {
  entry <- file_store[[file_id]]
  if (is.null(entry)) {
    res$status <- 404
    return(list(error = sprintf("file_id '%s' nu există sau sesiunea a expirat.", file_id)))
  }
  
  df     <- entry$df
  n_rows <- nrow(df)
  
  columns <- lapply(colnames(df), function(col_name) {
    n_missing <- sum(is.na(df[[col_name]]))
    pct       <- n_missing / n_rows
    list(column        = col_name,
         missing_count = n_missing,
         missing_pct   = round(pct * 100, 2),
         severity      = if (pct > 0.20) "critica"
         else if (pct > 0.05) "atentie"
         else "ok")
  })
  
  list(
    file_id     = file_id,
    filename    = entry$filename,
    n_rows      = n_rows,
    n_cols      = ncol(df),
    has_missing = any(sapply(columns, function(c) c$missing_count > 0)),
    columns     = columns,
    thresholds  = list(atentie = 5.0, critica = 20.0)
  )
}

# ============================================================
# GRUP 6 — BIAS SCORE
# ============================================================

#* Scor compus de bias: 0.7 × effect_norm + 0.3 × imbalance_penalty
#* Pentru 2 grupuri: effect_norm = Cohen's d (numeric) sau SPD (binar)
#* Pentru 3+ grupuri: effect_norm = eta-squared din ANOVA (numeric)
#*                    sau SPD între group1/group2 specificați (binar)
#* @tag Bias Score
#* @post /bias-score
#* @param file_id ID-ul fișierului
#* @param sensitive_col Coloana atribut sensibil
#* @param target_col Coloana target (numeric sau binary)
#* @param positive_value Valoarea pozitivă pentru target binar (opțional)
#* @param group1 Grup de referință — doar pentru target binar cu 3+ grupuri
#* @param group2 Grup protejat — doar pentru target binar cu 3+ grupuri
#* @serializer json
function(file_id, sensitive_col, target_col,
         positive_value = NULL, group1 = NULL, group2 = NULL, res) {
  entry <- file_store[[file_id]]
  if (is.null(entry)) {
    res$status <- 404
    return(list(error = sprintf("file_id '%s' nu există sau sesiunea a expirat.", file_id)))
  }
  
  df <- entry$df
  
  if (!sensitive_col %in% colnames(df)) {
    res$status <- 400
    return(list(error = sprintf("Coloana '%s' nu există în fișier.", sensitive_col)))
  }
  if (!target_col %in% colnames(df)) {
    res$status <- 400
    return(list(error = sprintf("Coloana '%s' nu există în fișier.", target_col)))
  }
  
  target_type <- detect_col_type(df[[target_col]])
  
  # ── Componenta 1: effect_norm ─────────────────────────────
  if (target_type == "numeric") {
    r <- get_numeric_groups(file_id, sensitive_col, target_col)
    if (!is.null(r$error)) { res$status <- 400; return(list(error = r$error)) }
    
    use_pairwise <- (!is.null(group1) && !is.null(group2) &&
                       trimws(group1) != "" && trimws(group2) != "") ||
      r$n_groups == 2
    
    if (use_pairwise) {
      f <- filter_two_groups(r$groups, group1, group2, sensitive_col)
      if (!is.null(f$error)) { res$status <- 400; return(list(error = f$error)) }
      x1 <- f$groups[[f$g1]]; x2 <- f$groups[[f$g2]]
      n1 <- length(x1);       n2 <- length(x2)
      sd_pooled    <- sqrt(((n1-1)*var(x1) + (n2-1)*var(x2)) / (n1+n2-2))
      cohens_d     <- if (sd_pooled == 0) 0 else (mean(x1) - mean(x2)) / sd_pooled
      effect_norm  <- min(abs(cohens_d), 1.0)
      effect_label <- "Cohen's d"
      effect_value <- round(cohens_d, 4)
    } else {
      eta2         <- compute_eta_squared(r$groups)
      effect_norm  <- eta2
      effect_label <- "eta-squared (η²) — toate grupurile"
      effect_value <- round(eta2, 4)
    }
    
  } else if (target_type == "binary") {
    r <- get_binary_groups(file_id, sensitive_col, target_col,
                           positive_value, group1, group2)
    if (!is.null(r$error)) { res$status <- 400; return(list(error = r$error)) }
    
    spd          <- r$privileged$proportion - r$protected$proportion
    effect_norm  <- min(abs(spd), 1.0)
    effect_label <- "SPD"
    effect_value <- round(spd, 4)
    
  } else {
    res$status <- 400
    return(list(error = sprintf(
      "Coloana '%s' este de tip '%s'. Target-ul trebuie să fie numeric sau binary.",
      target_col, target_type
    )))
  }
  
  # ── Componenta 2: imbalance_penalty ──────────────────────
  vals              <- df[[sensitive_col]][!is.na(df[[sensitive_col]])]
  props             <- table(vals) / length(vals)
  min_prop          <- unname(min(props))
  imbalance_penalty <- if (min_prop >= 0.20) 0
  else (0.20 - min_prop) / 0.20
  
  # ── Scor final ───────────────────────────────────────────
  bias_score <- min(0.7 * effect_norm + 0.3 * imbalance_penalty, 1.0)
  severity   <- if (bias_score < 0.20) "neglijabil"
  else if (bias_score < 0.50) "moderat"
  else "ridicat"
  
  list(
    file_id           = file_id,
    sensitive_col     = sensitive_col,
    target_col        = target_col,
    target_type       = target_type,
    effect_metric     = effect_label,
    effect_value      = effect_value,
    effect_norm       = round(effect_norm, 4),
    min_group_prop    = round(min_prop, 4),
    imbalance_penalty = round(imbalance_penalty, 4),
    bias_score        = round(bias_score, 4),
    severity          = severity,
    formula           = "min(0.7 * effect_norm + 0.3 * imbalance_penalty, 1.0)",
    thresholds        = list(neglijabil = 0.20, moderat = 0.50)
  )
}
# ============================================================
# GRUP 7 — SOCIO-DEMOGRAFIC
# ============================================================

# ── Date de referință statice Eurostat (2022-2023) ───────────
EUROSTAT_REF <- list(
  pay_gap_pct = list(
    EU = 12.7, RO = 3.6, DE = 17.6, FR = 15.8, HU = 17.7, BG = 12.3,
    note = "Gender pay gap neajustat (% diferență câștiguri orare brute, sursa Eurostat 2022)"
  ),
  employment_rate_pct = list(
    EU = 70.4, RO = 62.8, DE = 76.7, FR = 68.1, HU = 74.4, BG = 70.3,
    note = "Rata de ocupare 15-64 ani, ambele sexe (%, sursa Eurostat 2022)"
  ),
  tertiary_education_pct = list(
    EU = 33.5, RO = 19.6, DE = 33.5, FR = 38.5, HU = 26.3, BG = 30.1,
    note = "Populație 25-64 ani cu educație terțiară (%, sursa Eurostat 2022)"
  )
)

# ── Distribuție regională populație România (INS 2022, %) ─────
RO_REGION_POP_PCT <- list(
  "Nord-Est"          = 16.5,
  "Sud-Est"           = 13.5,
  "Sud - Muntenia"    = 16.0,
  "Sud-Vest Oltenia"  = 10.5,
  "Vest"              =  9.5,
  "Nord-Vest"         = 13.0,
  "Centru"            = 12.0,
  "Bucuresti-Ilfov"   =  9.0
)

#* Date de referință Eurostat: pay gap, rata ocupare, educație terțiară
#* @tag Socio-Demografic
#* @get /socio/reference
#* @serializer json
function() {
  list(
    source      = "Eurostat / INS România",
    reference_year = 2022,
    indicators  = list(
      pay_gap = list(
        label = "Gender pay gap neajustat (%)",
        note  = EUROSTAT_REF$pay_gap_pct$note,
        values = list(
          EU = EUROSTAT_REF$pay_gap_pct$EU,
          RO = EUROSTAT_REF$pay_gap_pct$RO,
          DE = EUROSTAT_REF$pay_gap_pct$DE,
          FR = EUROSTAT_REF$pay_gap_pct$FR,
          HU = EUROSTAT_REF$pay_gap_pct$HU,
          BG = EUROSTAT_REF$pay_gap_pct$BG
        )
      ),
      employment_rate = list(
        label = "Rata de ocupare 15-64 ani (%)",
        note  = EUROSTAT_REF$employment_rate_pct$note,
        values = list(
          EU = EUROSTAT_REF$employment_rate_pct$EU,
          RO = EUROSTAT_REF$employment_rate_pct$RO,
          DE = EUROSTAT_REF$employment_rate_pct$DE,
          FR = EUROSTAT_REF$employment_rate_pct$FR,
          HU = EUROSTAT_REF$employment_rate_pct$HU,
          BG = EUROSTAT_REF$employment_rate_pct$BG
        )
      ),
      tertiary_education = list(
        label = "Educație terțiară 25-64 ani (%)",
        note  = EUROSTAT_REF$tertiary_education_pct$note,
        values = list(
          EU = EUROSTAT_REF$tertiary_education_pct$EU,
          RO = EUROSTAT_REF$tertiary_education_pct$RO,
          DE = EUROSTAT_REF$tertiary_education_pct$DE,
          FR = EUROSTAT_REF$tertiary_education_pct$FR,
          HU = EUROSTAT_REF$tertiary_education_pct$HU,
          BG = EUROSTAT_REF$tertiary_education_pct$BG
        )
      )
    ),
    ro_region_population_pct = RO_REGION_POP_PCT
  )
}

#* Distribuție pe grupe de vârstă + comparație cu referință EU
#* @tag Socio-Demografic
#* @post /socio/age
#* @param file_id ID-ul fișierului
#* @param age_col Coloana cu vârsta (numerică)
#* @serializer json
function(file_id, age_col, res) {
  entry <- file_store[[file_id]]
  if (is.null(entry)) {
    res$status <- 404
    return(list(error = sprintf("file_id '%s' nu există sau sesiunea a expirat.", file_id)))
  }
  
  df <- entry$df
  
  if (!age_col %in% colnames(df)) {
    res$status <- 400
    return(list(error = sprintf("Coloana '%s' nu există în fișier.", age_col)))
  }
  if (!is.numeric(df[[age_col]])) {
    res$status <- 400
    return(list(error = sprintf("Coloana '%s' nu este numerică.", age_col)))
  }
  
  ages <- df[[age_col]][!is.na(df[[age_col]])]
  if (length(ages) < 4) {
    res$status <- 422
    return(list(error = "Date insuficiente pentru analiza vârstei (minimum 4 valori)."))
  }
  
  breaks <- c(0, 25, 35, 50, 65, Inf)
  labels <- c("<=25", "26-35", "36-50", "51-65", "65+")
  grupe  <- cut(ages, breaks = breaks, labels = labels, right = TRUE)
  counts <- table(grupe)
  props  <- counts / length(ages) * 100
  
  age_groups <- lapply(labels, function(lb) {
    list(
      group      = lb,
      count      = unname(counts[lb]),
      percent    = round(unname(props[lb]), 2)
    )
  })
  
  list(
    file_id      = file_id,
    age_col      = age_col,
    n_valid      = length(ages),
    mean_age     = round(mean(ages), 2),
    median_age   = round(median(ages), 2),
    min_age      = min(ages),
    max_age      = max(ages),
    std_age      = round(sd(ages), 2),
    age_groups   = age_groups,
    reference    = list(
      note       = "Vârsta medie forță de muncă RO (INS 2022): ~42 ani",
      ro_mean_workforce_age = 42
    )
  )
}

#* Distribuție nivel educație + comparație cu referință Eurostat
#* @tag Socio-Demografic
#* @post /socio/education
#* @param file_id ID-ul fișierului
#* @param education_col Coloana cu nivelul de educație
#* @serializer json
function(file_id, education_col, res) {
  entry <- file_store[[file_id]]
  if (is.null(entry)) {
    res$status <- 404
    return(list(error = sprintf("file_id '%s' nu există sau sesiunea a expirat.", file_id)))
  }
  
  df <- entry$df
  
  if (!education_col %in% colnames(df)) {
    res$status <- 400
    return(list(error = sprintf("Coloana '%s' nu există în fișier.", education_col)))
  }
  
  vals    <- df[[education_col]][!is.na(df[[education_col]])]
  n_total <- length(vals)
  
  if (n_total < 4) {
    res$status <- 422
    return(list(error = "Date insuficiente pentru analiza educației (minimum 4 valori)."))
  }
  
  counts <- table(as.character(vals))
  props  <- counts / n_total * 100
  
  levels_info <- lapply(names(counts), function(lv) {
    list(
      level   = lv,
      count   = unname(counts[lv]),
      percent = round(unname(props[lv]), 2)
    )
  })
  
  # Detectează automat proporția cu educație terțiară (Facultate/Masterat/Doctorat)
  tertiary_keywords <- c("facultate", "masterat", "doctorat", "universitar",
                         "bachelor", "master", "phd", "licenta", "licență")
  tertiary_vals <- vals[tolower(as.character(vals)) %in% tertiary_keywords |
                          grepl(paste(tertiary_keywords, collapse="|"),
                                tolower(as.character(vals)))]
  pct_tertiary <- round(length(tertiary_vals) / n_total * 100, 2)
  
  list(
    file_id          = file_id,
    education_col    = education_col,
    n_valid          = n_total,
    n_levels         = length(counts),
    levels           = levels_info,
    pct_tertiary_est = pct_tertiary,
    reference        = list(
      note               = "Educație terțiară 25-64 ani (Eurostat 2022)",
      ro_pct             = EUROSTAT_REF$tertiary_education_pct$RO,
      eu_pct             = EUROSTAT_REF$tertiary_education_pct$EU,
      above_ro_reference = pct_tertiary > EUROSTAT_REF$tertiary_education_pct$RO
    )
  )
}

#* Distribuție regională + comparație cu structura populației RO
#* @tag Socio-Demografic
#* @post /socio/region
#* @param file_id ID-ul fișierului
#* @param region_col Coloana cu regiunea
#* @serializer json
function(file_id, region_col, res) {
  entry <- file_store[[file_id]]
  if (is.null(entry)) {
    res$status <- 404
    return(list(error = sprintf("file_id '%s' nu există sau sesiunea a expirat.", file_id)))
  }
  
  df <- entry$df
  
  if (!region_col %in% colnames(df)) {
    res$status <- 400
    return(list(error = sprintf("Coloana '%s' nu există în fișier.", region_col)))
  }
  
  vals    <- df[[region_col]][!is.na(df[[region_col]])]
  n_total <- length(vals)
  
  if (n_total < 4) {
    res$status <- 422
    return(list(error = "Date insuficiente pentru analiza regională (minimum 4 valori)."))
  }
  
  counts <- table(as.character(vals))
  props  <- counts / n_total * 100
  
  regions_info <- lapply(names(counts), function(rg) {
    ref_pct    <- RO_REGION_POP_PCT[[rg]]
    sample_pct <- round(unname(props[rg]), 2)
    list(
      region         = rg,
      count          = unname(counts[rg]),
      sample_pct     = sample_pct,
      reference_pct  = ref_pct,
      deviation_pp   = if (!is.null(ref_pct)) round(sample_pct - ref_pct, 2) else NULL,
      undersampled   = if (!is.null(ref_pct)) sample_pct < ref_pct else NULL
    )
  })
  
  matched <- names(counts)[names(counts) %in% names(RO_REGION_POP_PCT)]
  
  list(
    file_id          = file_id,
    region_col       = region_col,
    n_valid          = n_total,
    n_regions        = length(counts),
    regions          = regions_info,
    reference_source = "INS România 2022 — distribuție populație pe regiuni NUTS2",
    matched_regions  = as.list(matched),
    note             = if (length(matched) < length(counts))
      "Unele regiuni din fișier nu au corespondent în datele de referință INS."
    else
      "Toate regiunile au corespondent în datele de referință INS."
  )
}