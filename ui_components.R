# UI Components

# Helper function to create image display UI
create_image_display_ui <- function(prefix) {
  fluidRow(
    box(title = "Spatial Distribution", width = 12,
        plotlyOutput(paste0(prefix, "_spatial"), height = "400px")),
    box(title = "Time Series", width = 12,
        plotlyOutput(paste0(prefix, "_timeseries"), height = "400px")),
    box(title = "Monthly Statistics", width = 12,
        plotlyOutput(paste0(prefix, "_monthly"), height = "400px"))
  )
}

# Overview tab UI
overview_tab_ui <- function() {
  tabItem(tabName = "overview",
          fluidRow(
            box(title = "Welcome", width = 12,
                includeMarkdown("docs/overview.md"))
          )
  )
}

# VIC Data tab UI
vic_tab_ui <- function() {
  tabItem(
    tabName = "vic",
    fluidRow(
      box(
        title = "VIC Model Overview",
        width = 12,
        p("The Variable Infiltration Capacity (VIC) model is a macroscale hydrologic model that represents the land surface as a grid of cells. It simulates water and energy fluxes at the land surface and subsurface.")
      )
    ),
    fluidRow(
      box(
        title = "Time Series Analysis",
        width = 12,
        imageOutput("vic_time_series", height = "400px")
      )
    ),
    fluidRow(
      box(
        title = "Monthly Statistics",
        width = 12,
        imageOutput("vic_monthly_stats", height = "400px")
      )
    ),
    fluidRow(
      box(
        title = "Spatial Distribution Maps",
        width = 12,
        tabsetPanel(
          tabPanel("Precipitation", imageOutput("precipitation_map", height = "400px")),
          tabPanel("Snow Water Equivalent", imageOutput("snow_water_map", height = "400px")),
          tabPanel("Runoff", imageOutput("runoff_map", height = "400px")),
          tabPanel("Baseflow", imageOutput("baseflow_map", height = "400px")),
          tabPanel("Evapotranspiration", imageOutput("evapotranspiration_map", height = "400px")),
          tabPanel("Soil Moisture Layer 1", imageOutput("soil_moisture_1_map", height = "400px")),
          tabPanel("Soil Moisture Layer 2", imageOutput("soil_moisture_2_map", height = "400px")),
          tabPanel("Soil Moisture Layer 3", imageOutput("soil_moisture_3_map", height = "400px"))
        )
      )
    )
  )
}

# SMAP Data tab UI
smap_tab_ui <- function() {
  tabItem(tabName = "smap",
          fluidRow(
            box(title = "SMAP Data Selection", width = 12,
                actionButton("load_smap", "Load SMAP Data"))
          ),
          fluidRow(
            box(title = "SMAP Data Visualization", width = 12,
                plotlyOutput("smap_plot", height = get_config("plot_height")))
          ),
          create_image_display_ui("smap")
  )
}

# GRACE Data tab UI
grace_tab_ui <- function() {
  tabItem(tabName = "grace",
          fluidRow(
            box(title = "GRACE Data Selection", width = 12,
                actionButton("load_grace", "Load GRACE Data"))
          ),
          fluidRow(
            box(title = "GRACE Data Visualization", width = 12,
                plotlyOutput("grace_plot", height = get_config("plot_height")))
          ),
          create_image_display_ui("grace")
  )
}

# Analysis tab UI
analysis_tab_ui <- function() {
  tabItem(tabName = "analysis",
          fluidRow(
            box(title = "Analysis Selection", width = 12,
                selectInput("analysis_type", "Select Analysis Type",
                          choices = c("Trend Analysis", "Correlation Analysis", "Anomaly Analysis"),
                          selected = "Trend Analysis"),
                actionButton("run_analysis", "Run Analysis"))
          ),
          fluidRow(
            box(title = "Analysis Results", width = 12,
                plotlyOutput("analysis_plot", height = "400px"))
          )
  )
}

# Main dashboard UI
dashboard_ui <- function() {
  dashboardPage(
    dashboardHeader(title = "Colorado River Basin Dashboard"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
        menuItem("VIC Data", tabName = "vic", icon = icon("water")),
        menuItem("SMAP Data", tabName = "smap", icon = icon("satellite")),
        menuItem("GRACE Data", tabName = "grace", icon = icon("globe")),
        menuItem("Analysis", tabName = "analysis", icon = icon("chart-line"))
      )
    ),
    dashboardBody(
      # Include custom CSS
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
      ),
      
      tabItems(
        overview_tab_ui(),
        vic_tab_ui(),
        smap_tab_ui(),
        grace_tab_ui(),
        analysis_tab_ui()
      )
    )
  )
} 