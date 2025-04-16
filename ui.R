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

# Add resource path for images
addResourcePath("images", "images")

# Source UI components
source("ui_components.R")
source("sidebar_menu.R")

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Colorado River Basin Dashboard"),
  dashboardSidebar(sidebar_menu),
  dashboardBody(
    # Include custom CSS
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),
    
    tabItems(
      spatial_map_tab_ui(),
      vic_tab_ui(),
      smap_tab_ui(),
      grace_tab_ui(),
      precipitation_tab_ui(),
      swe_tab_ui(),
      soil_moisture_tab_ui(),
      swe_anomalies_tab_ui(),
      help_tab_ui()
    )
  )
) 