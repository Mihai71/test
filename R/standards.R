# R/standards.R
library(giscoR)
library(dplyr)

#(FR-03)
age_bins <- c(0, 18, 25, 45, 60, 150)
age_labels <- c("<18", "18-25", "26-45", "46-59", "60+")
edu_levels <- c("Primar", "Secundar", "Tertiar")

# NUTS 3 România (Județe) 
nuts3_ro <- gisco_get_nuts(country = "Romania", nuts_level = 3) %>%
  as.data.frame() %>%
  select(NUTS_ID, NAME_LATN)