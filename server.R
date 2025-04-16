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

# Source helper functions
source("server_components.R")
source("helpers.R")

# Add resource path for images
addResourcePath("images", "/Users/praddy5/Desktop/Dashboard/images")

# Main server function
server <- function(input, output, session) {
  # ... existing code ...
  
  # VIC Model visualizations
  output$vic_time_series <- renderImage({
    list(src = "vic_analysis/time_series.png",
         contentType = "image/png",
         width = "100%",
         height = "400px")
  }, deleteFile = FALSE)
  
  output$vic_monthly_stats <- renderImage({
    list(src = "vic_analysis/monthly_stats.png",
         contentType = "image/png",
         width = "100%",
         height = "400px")
  }, deleteFile = FALSE)
  
  output$precipitation_map <- renderImage({
    list(src = "vic_analysis/precipitation_map.png",
         contentType = "image/png",
         width = "100%",
         height = "400px")
  }, deleteFile = FALSE)
  
  output$snow_water_map <- renderImage({
    list(src = "vic_analysis/snow_water_equivalent_map.png",
         contentType = "image/png",
         width = "100%",
         height = "400px")
  }, deleteFile = FALSE)
  
  output$runoff_map <- renderImage({
    list(src = "vic_analysis/runoff_map.png",
         contentType = "image/png",
         width = "100%",
         height = "400px")
  }, deleteFile = FALSE)
  
  output$baseflow_map <- renderImage({
    list(src = "vic_analysis/baseflow_map.png",
         contentType = "image/png",
         width = "100%",
         height = "400px")
  }, deleteFile = FALSE)
  
  output$evapotranspiration_map <- renderImage({
    list(src = "vic_analysis/evapotranspiration_map.png",
         contentType = "image/png",
         width = "100%",
         height = "400px")
  }, deleteFile = FALSE)
  
  output$soil_moisture_1_map <- renderImage({
    list(src = "vic_analysis/soil_moisture_layer_1_map.png",
         contentType = "image/png",
         width = "100%",
         height = "400px")
  }, deleteFile = FALSE)
  
  output$soil_moisture_2_map <- renderImage({
    list(src = "vic_analysis/soil_moisture_layer_2_map.png",
         contentType = "image/png",
         width = "100%",
         height = "400px")
  }, deleteFile = FALSE)
  
  output$soil_moisture_3_map <- renderImage({
    list(src = "vic_analysis/soil_moisture_layer_3_map.png",
         contentType = "image/png",
         width = "100%",
         height = "400px")
  }, deleteFile = FALSE)
} 