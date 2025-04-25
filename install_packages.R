required_packages <- c(
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

# Check which packages are not installed
missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]

# Install missing packages
if (length(missing_packages) > 0) {
  install.packages(missing_packages, dependencies = TRUE)
  cat("Installed missing packages:", paste(missing_packages, collapse = ", "), "\n")
} else {
  cat("All required packages are already installed.\n")
} 