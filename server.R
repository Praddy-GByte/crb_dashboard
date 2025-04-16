# Load required libraries
library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(tidyr)
library(lubridate)
library(ncdf4)
library(raster)
library(viridis)
library(sf)
library(leaflet)
library(DT)
library(markdown)
library(shinycssloaders)
library(shinythemes)
library(shinyjs)
library(shinyWidgets)
library(zoo)

# Source server components
source("server_components.R")

# Define server
server <- function(input, output, session) {
  # Call server components
  source("server_components.R", local = TRUE)
  
  # Initialize reactive values
  values <- reactiveValues(
    vic_data = NULL,
    snotel_data = NULL,
    analysis_results = NULL
  )
  
  # Call individual server components
  prism_server(input, output, session)
  vic_server(input, output, session, values)
  smap_server(input, output, session)
  grace_server(input, output, session)
  analysis_server(input, output, session, values)
  analysis_output_server(input, output, session)
  
  # Handle analysis tab
  observeEvent(input$run_analysis, {
    showModal(modalDialog("Running analysis...", footer = NULL))
    tryCatch({
      # Get the selected analysis type
      analysis_type <- input$analysis_type
      
      # Create analysis plot based on selected type
      analysis_plot <- switch(analysis_type,
                            "Trend Analysis" = create_trend_plot(values$vic_data, values$snotel_data),
                            "Correlation Analysis" = create_correlation_plot(values$vic_data, values$snotel_data),
                            "Anomaly Analysis" = create_anomaly_plot(values$vic_data, values$snotel_data))
      
      # Store the results
      values$analysis_results <- analysis_plot
      
      # Render the analysis plot
      output$analysis_plot <- renderPlotly({ analysis_plot })
      
      removeModal()
    }, error = function(e) {
      removeModal()
      showNotification(paste("Error running analysis:", e$message), type = "error")
    })
  })
} 