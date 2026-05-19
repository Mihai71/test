# R/standards.R - Constante standardizate și date de referință (FR-03)
library(dplyr)

# ---------------------------------------------------------------------------
# FR-03: Grupare vârstă standardizată
# ---------------------------------------------------------------------------
age_bins   <- c(0, 18, 25, 45, 60, 150)
age_labels <- c("<18", "18-25", "26-45", "46-59", "60+")

# ---------------------------------------------------------------------------
# FR-03: Clasificare educație
# ---------------------------------------------------------------------------
edu_levels <- c("Primară", "Secundară", "Terțiară")

edu_map <- list(
  "Primară"   = c("primar", "fara scoala", "gimnaziu", "fără studii", "primar incomplet",
                  "primary", "no education", "isced 0", "isced 1"),
  "Secundară" = c("liceu", "profesional", "postliceal", "ttp", "secondary",
                  "isced 2", "isced 3", "isced 4", "vocational"),
  "Terțiară"  = c("facultate", "universitar", "masterat", "doctorat", "superior",
                  "tertiary", "university", "isced 5", "isced 6", "isced 7", "isced 8")
)

classify_education <- function(x) {
  x_lower <- tolower(trimws(as.character(x)))
  result <- rep(NA_character_, length(x_lower))
  for (level in names(edu_map)) {
    keywords <- edu_map[[level]]
    matches  <- sapply(x_lower, function(val) any(sapply(keywords, function(kw) grepl(kw, val, fixed = TRUE))))
    result[matches] <- level
  }
  result
}

# ---------------------------------------------------------------------------
# FR-03: Date de referință România și UE (fallback static, actualizate 2024)
# Sursa: Eurostat (earn_ses_annual), INS România
# Unitate: RON/lună  (conversie din EUR la curs 1 EUR = 5.2 RON, mai 2024)
# NOTĂ METODOLOGICĂ:
#   - RO = salariu mediu NET (sursa INS Romania 2023)
#   - EU/DE/FR/HU/BG = salariu mediu BRUT (sursa Eurostat earn_ses_annual 2023)
#   Comparația net vs. brut este orientativă; salariul net RO e aprox. 70-75% din brut.
# ---------------------------------------------------------------------------
RON_PER_EUR <- 5.2   # curs de schimb aproximativ utilizat pentru conversie

reference_data <- list(
  salary = list(
    RO  = 6250,   # 1202 EUR net  × 5.2 — Salariu mediu net România 2023 (INS)
    EU  = 15532,  # 2987 EUR brut × 5.2 — Salariu mediu brut UE-27 2023 (Eurostat)
    DE  = 21346,  # 4105 EUR brut × 5.2 — Germania
    FR  = 17742,  # 3412 EUR brut × 5.2 — Franța
    HU  = 7571,   # 1456 EUR brut × 5.2 — Ungaria
    BG  = 5320    # 1023 EUR brut × 5.2 — Bulgaria
  ),
  pension = list(
    RO  = 3931,   # 756  EUR      × 5.2 — Pensie medie România 2023 (CNPP)
    EU  = 7540    # 1450 EUR      × 5.2 — Pensie medie UE
  )
)

# Funcție pentru a obține date Eurostat live (cu fallback la statice)
get_eurostat_reference <- function(indicator = "salary", country = "RO") {
  tryCatch({
    if (!requireNamespace("eurostat", quietly = TRUE)) stop("eurostat not installed")
    dat <- eurostat::get_eurostat("earn_ses_annual", time_format = "num",
                                  filters = list(geo = country, sex = "T",
                                                 nace_r2 = "B-S", worktime = "TOTAL",
                                                 earnings = "MEAN", unit = "EUR"))
    if (!is.null(dat) && nrow(dat) > 0) {
      val <- dat %>% filter(time == max(time)) %>% pull(values)
      # Eurostat returnează EUR — convertim în RON
      if (length(val) > 0 && !is.na(val[1])) return(as.numeric(val[1]) * RON_PER_EUR)
    }
    stop("no data")
  }, error = function(e) {
    ref <- reference_data[[indicator]]
    if (!is.null(ref[[country]])) return(ref[[country]])
    return(NA_real_)
  })
}

# ---------------------------------------------------------------------------
# NUTS România – regiunile de dezvoltare (NUTS 2) și județe (NUTS 3)
# Sursa: Eurostat LAU-NUTS 2024
# ---------------------------------------------------------------------------
nuts2_ro <- data.frame(
  NUTS_ID   = c("RO11", "RO12", "RO21", "RO22", "RO31", "RO32", "RO41", "RO42"),
  NAME_RO   = c("Nord-Vest", "Centru", "Nord-Est", "Sud-Est",
                "Sud - Muntenia", "București-Ilfov", "Sud-Vest Oltenia", "Vest"),
  stringsAsFactors = FALSE
)

nuts3_ro <- data.frame(
  NUTS_ID   = c("RO111","RO112","RO113","RO114","RO115","RO116",
                "RO121","RO122","RO123","RO124","RO125","RO126",
                "RO211","RO212","RO213","RO214","RO215","RO216","RO217",
                "RO221","RO222","RO223","RO224","RO225","RO226",
                "RO311","RO312","RO313","RO314","RO315","RO316","RO317",
                "RO321","RO322",
                "RO411","RO412","RO413","RO414","RO415",
                "RO421","RO422","RO423","RO424","RO425","RO426"),
  NAME_RO   = c("Bihor","Bistrița-Năsăud","Cluj","Maramureș","Satu Mare","Sălaj",
                "Alba","Brașov","Covasna","Harghita","Mureș","Sibiu",
                "Bacău","Botoșani","Iași","Neamț","Suceava","Vaslui","Iași",
                "Brăila","Buzău","Constanța","Galați","Tulcea","Vrancea",
                "Argeș","Călărași","Dâmbovița","Giurgiu","Ialomița","Prahova","Teleorman",
                "București","Ilfov",
                "Dolj","Gorj","Mehedinți","Olt","Vâlcea",
                "Arad","Caraș-Severin","Hunedoara","Timiș","Timiș","Timiș"),
  NUTS2_ID  = c(rep("RO11",6), rep("RO12",6), rep("RO21",7), rep("RO22",6),
                rep("RO31",7), rep("RO32",2), rep("RO41",5), rep("RO42",6)),
  stringsAsFactors = FALSE
) %>% distinct(NUTS_ID, .keep_all = TRUE)

judet_to_nuts3 <- setNames(nuts3_ro$NUTS_ID, toupper(nuts3_ro$NAME_RO))

nuts_shapes <- tryCatch({
  if (requireNamespace("giscoR", quietly = TRUE) && requireNamespace("sf", quietly = TRUE)) {
    list(
      nuts2 = giscoR::gisco_get_nuts(country = "Romania", nuts_level = 2, resolution = "20"),
      nuts3 = giscoR::gisco_get_nuts(country = "Romania", nuts_level = 3, resolution = "20")
    )
  } else NULL
}, error = function(e) NULL)

