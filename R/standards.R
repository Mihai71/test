library(dplyr)

age_bins   <- c(0, 18, 25, 45, 60, 150)
age_labels <- c("<18", "18-25", "26-45", "46-59", "60+")

edu_levels <- c("Primara", "Secundara", "Tertiara")

edu_map <- list(
  "Primara"   = c("primar", "fara scoala", "gimnaziu", "fara studii",
                  "primary", "no education", "isced 0", "isced 1"),
  "Secundara" = c("liceu", "profesional", "postliceal", "secondary",
                  "isced 2", "isced 3", "isced 4", "vocational"),
  "Tertiara"  = c("facultate", "universitar", "masterat", "doctorat", "superior",
                  "tertiary", "university", "isced 5", "isced 6", "isced 7", "isced 8")
)

classify_education <- function(x) {
  x_lower <- tolower(trimws(as.character(x)))
  result  <- rep(NA_character_, length(x_lower))
  for (level in names(edu_map)) {
    kws     <- edu_map[[level]]
    matches <- sapply(x_lower, function(v) any(sapply(kws, function(k) grepl(k, v, fixed = TRUE))))
    result[matches] <- level
  }
  result
}

# Date de referinta statice (Eurostat / INS 2023-2024), EUR/luna
reference_data <- list(
  salary  = list(RO = 1202, EU = 2987, DE = 4105, FR = 3412, HU = 1456, BG = 1023),
  pension = list(RO = 756,  EU = 1450)
)

get_eurostat_reference <- function(indicator = "salary", country = "RO") {
  tryCatch({
    if (!requireNamespace("eurostat", quietly = TRUE)) stop("not installed")
    dat <- eurostat::get_eurostat("earn_ses_annual", time_format = "num",
                                  filters = list(geo = country, sex = "T",
                                                 nace_r2 = "B-S", worktime = "TOTAL",
                                                 earnings = "MEAN", unit = "EUR"))
    if (!is.null(dat) && nrow(dat) > 0) {
      val <- dat %>% filter(time == max(time)) %>% pull(values)
      if (length(val) > 0 && !is.na(val[1])) return(as.numeric(val[1]))
    }
    stop("no data")
  }, error = function(e) {
    ref <- reference_data[[indicator]]
    if (!is.null(ref[[country]])) return(ref[[country]])
    NA_real_
  })
}

nuts2_ro <- data.frame(
  NUTS_ID = c("RO11","RO12","RO21","RO22","RO31","RO32","RO41","RO42"),
  NAME_RO = c("Nord-Vest","Centru","Nord-Est","Sud-Est",
              "Sud - Muntenia","Bucuresti-Ilfov","Sud-Vest Oltenia","Vest"),
  stringsAsFactors = FALSE
)

nuts3_ro <- data.frame(
  NUTS_ID  = c("RO111","RO112","RO113","RO114","RO115","RO116",
               "RO121","RO122","RO123","RO124","RO125","RO126",
               "RO211","RO212","RO213","RO214","RO215","RO216",
               "RO221","RO222","RO223","RO224","RO225","RO226",
               "RO311","RO312","RO313","RO314","RO315","RO316","RO317",
               "RO321","RO322",
               "RO411","RO412","RO413","RO414","RO415",
               "RO421","RO422","RO423","RO424","RO425"),
  NAME_RO  = c("Bihor","Bistrita-Nasaud","Cluj","Maramures","Satu Mare","Salaj",
               "Alba","Brasov","Covasna","Harghita","Mures","Sibiu",
               "Bacau","Botosani","Iasi","Neamt","Suceava","Vaslui",
               "Braila","Buzau","Constanta","Galati","Tulcea","Vrancea",
               "Arges","Calarasi","Dambovita","Giurgiu","Ialomita","Prahova","Teleorman",
               "Bucuresti","Ilfov",
               "Dolj","Gorj","Mehedinti","Olt","Valcea",
               "Arad","Caras-Severin","Hunedoara","Timis","Timis"),
  NUTS2_ID = c(rep("RO11",6), rep("RO12",6), rep("RO21",6), rep("RO22",6),
               rep("RO31",7), rep("RO32",2), rep("RO41",5), rep("RO42",5)),
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