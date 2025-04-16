# Enable detailed error reporting
options(error = function() {
  traceback(3)
  if(!interactive()) quit("no", status = 1, runLast = FALSE)
})

# Print R version and session info
print(R.version.string)
print(sessionInfo())

# Install required packages if not already installed
required_packages <- c(
  "shiny", "shinydashboard", "ggplot2", "plotly", "raster", "sf", "dplyr",
  "tidyr", "lubridate", "DT", "ncdf4", "leaflet", "viridis", "htmltools",
  "htmlwidgets", "data.table", "shinycssloaders", "shinyjs", "httr",
  "jsonlite", "units"
)

# Install missing packages
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) {
  print("Installing missing packages:")
  print(new_packages)
  install.packages(new_packages, repos = "https://cloud.r-project.org")
}

# Load required packages
print("Loading packages...")
for(pkg in required_packages) {
  print(paste("Loading", pkg))
  if(!require(pkg, character.only = TRUE)) {
    stop(paste("Failed to load package:", pkg))
  }
}

# Set memory limit
options(shiny.maxRequestSize = 1000*1024^2)  # Increase to 1GB

# Check if app.R exists
if(!file.exists("app.R")) {
  stop("app.R file not found in current directory")
}

# Check if help documentation exists
if(!dir.exists("help")) {
  stop("help directory not found")
}

if(!file.exists("help/documentation.md")) {
  stop("help/documentation.md not found")
}

if(!file.exists("help/data_sources.md")) {
  stop("help/data_sources.md not found")
}

# Print current working directory
print(paste("Current working directory:", getwd()))

# Try to run the application
print("Starting Shiny application...")
tryCatch({
  # Use a fixed port and handle any errors
  port <- 3838
  print(paste("Attempting to run on port:", port))
  
  # Run the application
  shiny::runApp('app.R', 
                port = port, 
                launch.browser = TRUE,
                host = "127.0.0.1")
}, error = function(e) {
  print("Error starting application:")
  print(e$message)
  print("\nTroubleshooting steps:")
  print("1. Check if port 3838 is already in use:")
  print("   - On macOS/Linux: lsof -i :3838")
  print("   - On Windows: netstat -ano | findstr :3838")
  print("2. Verify all required data files are present")
  print("3. Check file permissions")
  print("4. Try running with a different port")
}) 