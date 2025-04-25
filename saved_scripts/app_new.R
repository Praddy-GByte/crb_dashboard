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

# Create Shiny app
ui <- dashboardPage(
  dashboardHeader(title = "Hydrological Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("VIC Data", tabName = "vic", icon = icon("tint")),
      menuItem("GRACE Data", tabName = "grace", icon = icon("globe")),
      menuItem("PRISM Data", tabName = "prism", icon = icon("cloud"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "home", 
              h2("Welcome to the Hydrological Dashboard"),
              p("This dashboard provides analysis and visualization of various hydrological datasets.")
      ),
      tabItem(tabName = "vic", 
              h2("VIC Data Analysis"),
              fluidRow(
                box(
                  title = "Controls",
                  selectInput("vic_year", "Select Year:", choices = c("All Years", 2000:2020)),
                  selectInput("vic_variable", "Select Variable:",
                             choices = c("Precipitation", "Evapotranspiration", "Soil Moisture", 
                                       "Snow Water Equivalent", "Runoff"))
                )
              ),
              fluidRow(
                box(plotlyOutput("vic_spatial_map")),
                box(plotlyOutput("vic_time_series"))
              )
      ),
      tabItem(tabName = "grace", 
              h2("GRACE Data Analysis - Colorado River Basin"),
              fluidRow(
                box(
                  title = "Controls",
                  selectInput("grace_year", "Select Year:", choices = 2002:2024),
                  selectInput("grace_variable", "Select Variable:", 
                             choices = c("Total Water Storage", "Uncertainty")),
                  selectInput("grace_aggregation", "Time Aggregation:",
                             choices = c("Monthly", "Seasonal", "Annual"))
                )
              ),
              fluidRow(
                box(plotlyOutput("grace_spatial_map")),
                box(plotlyOutput("grace_timeseries"))
              ),
              fluidRow(
                box(plotlyOutput("grace_seasonal")),
                box(plotlyOutput("grace_trend"))
              )
      ),
      tabItem(tabName = "prism", 
              h2("PRISM Data Analysis - Colorado River Basin"),
              fluidRow(
                box(
                  title = "Controls",
                  selectInput("prism_year", "Select Year:", choices = 1981:2024),
                  selectInput("prism_variable", "Select Variable:", 
                             choices = c("Precipitation", "Temperature")),
                  selectInput("prism_aggregation", "Time Aggregation:",
                             choices = c("Monthly", "Seasonal", "Annual"))
                )
              ),
              fluidRow(
                box(plotlyOutput("prism_spatial_map")),
                box(plotlyOutput("prism_timeseries"))
              ),
              fluidRow(
                box(plotlyOutput("prism_seasonal")),
                box(plotlyOutput("prism_trend"))
              )
      )
    )
  )
)

server <- function(input, output) {
  # Server logic
  source("server_functions.R")  # Load server functions
}

# Run the application
shinyApp(ui = ui, server = server) 