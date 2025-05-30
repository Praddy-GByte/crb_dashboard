#!/usr/bin/env Rscript

# Create user library if it doesn't exist
user_lib <- Sys.getenv("R_LIBS_USER")
if (!dir.exists(user_lib)) dir.create(user_lib, recursive = TRUE)

# List of required packages
packages <- c(
  "shiny",
  "shinydashboard",
  "ggplot2",
  "plotly",
  "raster",
  "sf",
  "dplyr",
  "tidyr",
  "lubridate",
  "DT",
  "ncdf4",
  "leaflet",
  "markdown"
)

# Function to install missing packages
install_if_missing <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, repos = "https://cran.rstudio.com/", 
                    dependencies = TRUE,
                    lib = Sys.getenv("R_LIBS_USER"))
  }
}

# Install packages
sapply(packages, install_if_missing)

# Print session info
sessionInfo() 