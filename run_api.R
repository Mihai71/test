# run_api.R — Rulează din RStudio Console cu: source("run_api.R")
library(plumber)

pr <- plumber::plumb("api.R")

pr$run(
  host = "127.0.0.1",
  port = 8000,
  docs = TRUE    # Swagger UI disponibil la http://127.0.0.1:8000/__docs__/
)


