# Load required packages
library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(raster)
library(sf)
library(dplyr)
library(tidyr)
library(lubridate)
library(DT)
library(ncdf4)
library(leaflet)
library(markdown)

# Add resource path for images
addResourcePath("images", "/Users/praddy5/Desktop/Dashboard/images")

# Function to check for processed data
check_processed_data <- function(data_type, year = NULL) {
  processed_path <- file.path("data", paste0(data_type, "_processed"))
  if (!dir.exists(processed_path)) {
    return(FALSE)
  }
  
  if (!is.null(year)) {
    file_pattern <- paste0(".*", year, ".*\\.rds$")
    files <- list.files(processed_path, pattern = file_pattern, full.names = TRUE)
    return(length(files) > 0)
  }
  
  return(TRUE)
}

# Function to load processed data
load_processed_data <- function(data_type, year = NULL) {
  base_path <- "data/processed"
  if (data_type == "vic") {
    if (!is.null(year)) {
      file_path <- file.path(base_path, paste0("vic_", year, ".rds"))
    } else {
      file_path <- file.path(base_path, "vic_all.rds")
    }
  } else if (data_type == "smap") {
    file_path <- file.path(base_path, "smap.rds")
  } else if (data_type == "grace") {
    file_path <- file.path(base_path, "grace.rds")
  } else if (data_type == "snotel") {
    file_path <- file.path(base_path, "snotel.rds")
  }
  
  if (file.exists(file_path)) {
    return(readRDS(file_path))
  } else {
    return(NULL)
  }
}

# Function to save processed data
save_processed_data <- function(data, data_type, year = NULL) {
  processed_path <- file.path("data", paste0(data_type, "_processed"))
  if (!dir.exists(processed_path)) {
    dir.create(processed_path, recursive = TRUE)
  }
  
  if (!is.null(year)) {
    file_name <- paste0(data_type, "_", year, ".rds")
  } else {
    file_name <- paste0(data_type, ".rds")
  }
  
  saveRDS(data, file.path(processed_path, file_name))
}

# Define server logic
server <- function(input, output, session) {
  # Server-side code will be added here
  # This will include all the reactive expressions, observers, and output renderings
  # that were previously in app.R
  
  # Render main plot
  output$main_plot <- renderPlotly({
    # Example plot
    plot_ly(data = data.frame(x = 1:10, y = 1:10), x = ~x, y = ~y, type = "scatter", mode = "lines+markers")
  })
  
  # Render data table
  output$data_table <- renderDT({
    # Example data table
    datatable(data.frame(
      Variable = c("Temperature", "Precipitation", "Soil Moisture"),
      Value = c(25, 100, 0.3),
      Unit = c("°C", "mm", "m³/m³")
    ))
  })
}
