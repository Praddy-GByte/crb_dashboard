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
        title = "Spatial Distribution Maps",
        width = 12,
        tabsetPanel(
          tabPanel("Precipitation", 
                   fluidRow(
                     box(title = "Spatial Distribution", width = 6,
                         imageOutput("precipitation_spatial", height = "400px")),
                     box(title = "Time Series", width = 6,
                         imageOutput("precipitation_timeseries", height = "400px")),
                     box(title = "Monthly Statistics", width = 12,
                         imageOutput("precipitation_monthly", height = "400px"))
                   )),
          tabPanel("Snow Water Equivalent", 
                   fluidRow(
                     box(title = "Spatial Distribution", width = 6,
                         imageOutput("swe_spatial", height = "400px")),
                     box(title = "Time Series", width = 6,
                         imageOutput("swe_timeseries", height = "400px")),
                     box(title = "Monthly Statistics", width = 12,
                         imageOutput("swe_monthly", height = "400px"))
                   )),
          tabPanel("Runoff", 
                   fluidRow(
                     box(title = "Spatial Distribution", width = 6,
                         imageOutput("runoff_spatial", height = "400px")),
                     box(title = "Time Series", width = 6,
                         imageOutput("runoff_timeseries", height = "400px")),
                     box(title = "Monthly Statistics", width = 12,
                         imageOutput("runoff_monthly", height = "400px"))
                   )),
          tabPanel("Baseflow", 
                   fluidRow(
                     box(title = "Spatial Distribution", width = 6,
                         imageOutput("baseflow_spatial", height = "400px")),
                     box(title = "Time Series", width = 6,
                         imageOutput("baseflow_timeseries", height = "400px")),
                     box(title = "Monthly Statistics", width = 12,
                         imageOutput("baseflow_monthly", height = "400px"))
                   )),
          tabPanel("Evapotranspiration", 
                   fluidRow(
                     box(title = "Spatial Distribution", width = 6,
                         imageOutput("evap_spatial", height = "400px")),
                     box(title = "Time Series", width = 6,
                         imageOutput("evap_timeseries", height = "400px")),
                     box(title = "Monthly Statistics", width = 12,
                         imageOutput("evap_monthly", height = "400px"))
                   )),
          tabPanel("Soil Moisture", 
                   fluidRow(
                     box(title = "Spatial Distribution", width = 6,
                         imageOutput("soil_moisture_spatial", height = "400px")),
                     box(title = "Time Series", width = 6,
                         imageOutput("soil_moisture_timeseries", height = "400px")),
                     box(title = "Monthly Statistics", width = 12,
                         imageOutput("soil_moisture_monthly", height = "400px"))
                   )),
          tabPanel("Temperature", 
                   fluidRow(
                     box(title = "Spatial Distribution", width = 6,
                         imageOutput("temp_spatial", height = "400px")),
                     box(title = "Time Series", width = 6,
                         imageOutput("temp_timeseries", height = "400px")),
                     box(title = "Monthly Statistics", width = 12,
                         imageOutput("temp_monthly", height = "400px"))
                   )),
          tabPanel("Canopy Evaporation", 
                   fluidRow(
                     box(title = "Spatial Distribution", width = 6,
                         imageOutput("evap_canop_spatial", height = "400px")),
                     box(title = "Time Series", width = 6,
                         imageOutput("evap_canop_timeseries", height = "400px")),
                     box(title = "Monthly Statistics", width = 12,
                         imageOutput("evap_canop_monthly", height = "400px"))
                   )),
          tabPanel("Bare Soil Evaporation", 
                   fluidRow(
                     box(title = "Spatial Distribution", width = 6,
                         imageOutput("evap_bare_spatial", height = "400px")),
                     box(title = "Time Series", width = 6,
                         imageOutput("evap_bare_timeseries", height = "400px")),
                     box(title = "Monthly Statistics", width = 12,
                         imageOutput("evap_bare_monthly", height = "400px"))
                   )),
          tabPanel("Vegetation Transpiration", 
                   fluidRow(
                     box(title = "Spatial Distribution", width = 6,
                         imageOutput("transp_veg_spatial", height = "400px")),
                     box(title = "Time Series", width = 6,
                         imageOutput("transp_veg_timeseries", height = "400px")),
                     box(title = "Monthly Statistics", width = 12,
                         imageOutput("transp_veg_monthly", height = "400px"))
                   )),
          tabPanel("Surface Storage", 
                   fluidRow(
                     box(title = "Spatial Distribution", width = 6,
                         imageOutput("surfstor_spatial", height = "400px")),
                     box(title = "Time Series", width = 6,
                         imageOutput("surfstor_timeseries", height = "400px")),
                     box(title = "Monthly Statistics", width = 12,
                         imageOutput("surfstor_monthly", height = "400px"))
                   )),
          tabPanel("Surface Temperature", 
                   fluidRow(
                     box(title = "Spatial Distribution", width = 6,
                         imageOutput("surf_temp_spatial", height = "400px")),
                     box(title = "Time Series", width = 6,
                         imageOutput("surf_temp_timeseries", height = "400px")),
                     box(title = "Monthly Statistics", width = 12,
                         imageOutput("surf_temp_monthly", height = "400px"))
                   )),
          tabPanel("Snow Pack Temperature", 
                   fluidRow(
                     box(title = "Spatial Distribution", width = 6,
                         imageOutput("snow_pack_temp_spatial", height = "400px")),
                     box(title = "Time Series", width = 6,
                         imageOutput("snow_pack_temp_timeseries", height = "400px")),
                     box(title = "Monthly Statistics", width = 12,
                         imageOutput("snow_pack_temp_monthly", height = "400px"))
                   )),
          tabPanel("Soil Temperature", 
                   fluidRow(
                     box(title = "Spatial Distribution", width = 6,
                         imageOutput("soil_temp_spatial", height = "400px")),
                     box(title = "Time Series", width = 6,
                         imageOutput("soil_temp_timeseries", height = "400px")),
                     box(title = "Monthly Statistics", width = 12,
                         imageOutput("soil_temp_monthly", height = "400px"))
                   )),
          tabPanel("Air Temperature", 
                   fluidRow(
                     box(title = "Spatial Distribution", width = 6,
                         imageOutput("air_temp_spatial", height = "400px")),
                     box(title = "Time Series", width = 6,
                         imageOutput("air_temp_timeseries", height = "400px")),
                     box(title = "Monthly Statistics", width = 12,
                         imageOutput("air_temp_monthly", height = "400px"))
                   )),
          tabPanel("Snowfall", 
                   fluidRow(
                     box(title = "Spatial Distribution", width = 6,
                         imageOutput("snowf_spatial", height = "400px")),
                     box(title = "Time Series", width = 6,
                         imageOutput("snowf_timeseries", height = "400px")),
                     box(title = "Monthly Statistics", width = 12,
                         imageOutput("snowf_monthly", height = "400px"))
                   )),
          tabPanel("Rainfall", 
                   fluidRow(
                     box(title = "Spatial Distribution", width = 6,
                         imageOutput("rainf_spatial", height = "400px")),
                     box(title = "Time Series", width = 6,
                         imageOutput("rainf_timeseries", height = "400px")),
                     box(title = "Monthly Statistics", width = 12,
                         imageOutput("rainf_monthly", height = "400px"))
                   ))
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
  fluidRow(
    box(
      title = "GRACE Terrestrial Water Storage Analysis",
      width = 12,
      solidHeader = TRUE,
      status = "primary",
      p("Analysis of terrestrial water storage using GRACE satellite data (spatial resolution: 300 km, temporal resolution: monthly).")
    ),
    box(
      title = "Spatial Distribution",
      width = 6,
      solidHeader = TRUE,
      status = "info",
      imageOutput("grace_spatial", height = "400px")
    ),
    box(
      title = "Time Series",
      width = 6,
      solidHeader = TRUE,
      status = "info",
      imageOutput("grace_timeseries", height = "400px")
    ),
    box(
      title = "Monthly Statistics",
      width = 12,
      solidHeader = TRUE,
      status = "info",
      imageOutput("grace_monthly", height = "400px")
    )
  )
}

# Analysis tab UI
analysis_tab_ui <- function() {
  tabItem(
    tabName = "analysis",
    fluidRow(
      box(
        title = "Analysis Configuration",
        width = 12,
        solidHeader = TRUE,
        status = "primary",
        p("Select the type of analysis you want to perform and click 'Run Analysis' to generate results.")
      )
    ),
    fluidRow(
      box(
        title = "Analysis Selection",
        width = 12,
        solidHeader = TRUE,
        status = "info",
        selectInput("analysis_type", "Select Analysis Type",
                  choices = c("Trend Analysis", "Correlation Analysis", "Anomaly Analysis"),
                  selected = "Trend Analysis"),
        actionButton("run_analysis", "Run Analysis", 
                    icon = icon("play"),
                    class = "btn-primary")
      )
    ),
    fluidRow(
      box(
        title = "Analysis Results",
        width = 12,
        solidHeader = TRUE,
        status = "success",
        withSpinner(plotlyOutput("analysis_plot", height = "600px"))
      )
    )
  )
}

# Soil Moisture Tab UI
soil_moisture_tab_ui <- function() {
  fluidRow(
    box(
      title = "Soil Moisture Analysis",
      width = 12,
      solidHeader = TRUE,
      status = "primary",
      p("Analysis of soil moisture patterns and trends in the Colorado River Basin.")
    ),
    box(
      title = "Spatial Distribution",
      width = 6,
      solidHeader = TRUE,
      status = "info",
      imageOutput("soil_moisture_spatial", height = "400px")
    ),
    box(
      title = "Time Series",
      width = 6,
      solidHeader = TRUE,
      status = "info",
      imageOutput("soil_moisture_timeseries", height = "400px")
    ),
    box(
      title = "Monthly Statistics",
      width = 12,
      solidHeader = TRUE,
      status = "info",
      imageOutput("soil_moisture_monthly", height = "400px")
    )
  )
}

# Snow Water Equivalent Tab UI
swe_tab_ui <- function() {
  fluidRow(
    box(
      title = "Snow Water Equivalent Analysis",
      width = 12,
      solidHeader = TRUE,
      status = "primary",
      p("Analysis of snow water equivalent patterns and trends in the Colorado River Basin.")
    ),
    box(
      title = "Spatial Distribution",
      width = 6,
      solidHeader = TRUE,
      status = "info",
      imageOutput("swe_spatial", height = "400px")
    ),
    box(
      title = "Time Series",
      width = 6,
      solidHeader = TRUE,
      status = "info",
      imageOutput("swe_timeseries", height = "400px")
    ),
    box(
      title = "Monthly Statistics",
      width = 12,
      solidHeader = TRUE,
      status = "info",
      imageOutput("swe_monthly", height = "400px")
    )
  )
}

# PRISM Data tab UI
prism_tab_ui <- function() {
  tabItem(tabName = "prism",
          fluidRow(
            div(class = "variable-info",
                h4("PRISM Precipitation Analysis"),
                p("Analysis of PRISM calibrated precipitation data for the Colorado River Basin."),
                p("Spatial resolution: 4km, Temporal resolution: Daily")
            )
          ),
          fluidRow(
            box(width = 12, title = "Precipitation Statistics",
                fluidRow(
                  valueBoxOutput("prism_mean", width = 3),
                  valueBoxOutput("prism_max", width = 3),
                  valueBoxOutput("prism_min", width = 3),
                  valueBoxOutput("prism_std", width = 3)
                )
            )
          ),
          fluidRow(
            box(title = "Spatial Distribution", width = 6,
                plotlyOutput("prism_spatial", height = "400px")),
            box(title = "Time Series", width = 6,
                plotlyOutput("prism_timeseries", height = "400px"))
          ),
          fluidRow(
            box(title = "Monthly Statistics", width = 12,
                plotlyOutput("prism_monthly", height = "400px"))
          )
  )
}

# Analysis Output tab UI
analysis_output_tab_ui <- function() {
  tabItem(
    tabName = "analysis_output",
    fluidRow(
      box(
        title = "Comprehensive Analysis Results",
        width = 12,
        solidHeader = TRUE,
        status = "primary",
        p("This section displays comprehensive analysis results from various data sources and methodologies.")
      )
    ),
    fluidRow(
      box(
        title = "Combined Analysis",
        width = 12,
        tabsetPanel(
          tabPanel("Trend Analysis",
                   fluidRow(
                     box(title = "Combined Trends", width = 6,
                         imageOutput("combined_trends", height = "400px")),
                         box(title = "Normalized Trends", width = 6,
                             imageOutput("combined_trends_normalized", height = "400px"))
                   )),
          tabPanel("Time Series Analysis",
                   fluidRow(
                     box(title = "Combined Time Series", width = 6,
                         imageOutput("vic_combined_timeseries", height = "400px")),
                         box(title = "Normalized Time Series", width = 6,
                             imageOutput("vic_combined_timeseries_normalized", height = "400px"))
                   )),
          tabPanel("Monthly Statistics",
                   fluidRow(
                     box(title = "Monthly Statistics", width = 12,
                         imageOutput("monthly_statistics", height = "400px"))
                   ))
        )
      )
    ),
    fluidRow(
      box(
        title = "Variable-Specific Analysis",
        width = 12,
        tabsetPanel(
          tabPanel("Precipitation",
                   fluidRow(
                     box(title = "Spatial Distribution", width = 6,
                         imageOutput("precipitation_spatial", height = "400px")),
                         box(title = "Time Series", width = 6,
                             imageOutput("precipitation_timeseries", height = "400px")),
                             box(title = "Rain vs Snow", width = 6,
                                 imageOutput("precipitation_rain_snow", height = "400px")),
                                 box(title = "Monthly Analysis", width = 6,
                                     imageOutput("precipitation_monthly", height = "400px"))
                   )),
          tabPanel("Snow Water Equivalent",
                   fluidRow(
                     box(title = "April 1 SWE Trends", width = 6,
                         imageOutput("april1_swe_trends", height = "400px")),
                         box(title = "April 1 SWE Anomalies", width = 6,
                             imageOutput("april1_swe_anomalies", height = "400px")),
                             box(title = "SWE Analysis", width = 12,
                                 imageOutput("swe_analysis", height = "400px"))
                   )),
          tabPanel("Soil Moisture",
                   fluidRow(
                     box(title = "Soil Moisture Analysis", width = 12,
                         imageOutput("soil_moisture", height = "400px")),
                         box(title = "SMAP Surface Trend", width = 6,
                             imageOutput("smap_surface_trend", height = "400px")),
                             box(title = "SMAP Rootzone Trend", width = 6,
                                 imageOutput("smap_rootzone_trend", height = "400px"))
                   )),
          tabPanel("Water Storage",
                   fluidRow(
                     box(title = "Water Storage Analysis", width = 12,
                         imageOutput("water_storage", height = "400px")),
                         box(title = "GRACE Trend", width = 12,
                             imageOutput("grace_trend", height = "400px"))
                   ))
        )
      )
    )
  )
}

# Spatial Map tab UI
spatial_map_tab_ui <- function() {
  tabItem(
    tabName = "spatial_map",
    fluidRow(
      box(
        title = "Colorado River Basin Spatial Map",
        width = 12,
        solidHeader = TRUE,
        status = "primary",
        leafletOutput("basin_map", height = "600px")
      )
    )
  )
}

# Precipitation Analysis tab UI
precipitation_tab_ui <- function() {
  tabItem(
    tabName = "precipitation",
    fluidRow(
      box(
        title = "Precipitation Analysis",
        width = 12,
        solidHeader = TRUE,
        status = "primary",
        p("Analysis of precipitation patterns and trends in the Colorado River Basin.")
      )
    ),
    fluidRow(
      box(
        title = "Spatial Distribution",
        width = 6,
        solidHeader = TRUE,
        status = "info",
        imageOutput("precipitation_spatial", height = "400px")
      ),
      box(
        title = "Time Series",
        width = 6,
        solidHeader = TRUE,
        status = "info",
        imageOutput("precipitation_timeseries", height = "400px")
      )
    ),
    fluidRow(
      box(
        title = "Monthly Statistics",
        width = 12,
        solidHeader = TRUE,
        status = "info",
        imageOutput("precipitation_monthly", height = "400px")
      )
    )
  )
}

# SWE Anomalies tab UI
swe_anomalies_tab_ui <- function() {
  tabItem(
    tabName = "swe_anomalies",
    fluidRow(
      box(
        title = "Snow Water Equivalent Anomalies",
        width = 12,
        solidHeader = TRUE,
        status = "primary",
        p("Analysis of snow water equivalent anomalies in the Colorado River Basin.")
      )
    ),
    fluidRow(
      box(
        title = "April 1 SWE Anomalies",
        width = 12,
        solidHeader = TRUE,
        status = "info",
        imageOutput("april1_swe_anomalies", height = "400px")
      )
    ),
    fluidRow(
      box(
        title = "SWE Trend Analysis",
        width = 12,
        solidHeader = TRUE,
        status = "info",
        imageOutput("swe_trend", height = "400px")
      )
    )
  )
}

# Help tab UI
help_tab_ui <- function() {
  tabItem(
    tabName = "help",
    fluidRow(
      box(
        title = "Help Documentation",
        width = 12,
        solidHeader = TRUE,
        status = "primary",
        includeMarkdown("help/help.md")
      )
    ),
    fluidRow(
      box(
        title = "Data Sources",
        width = 12,
        solidHeader = TRUE,
        status = "info",
        includeMarkdown("help/data_sources.md")
      )
    ),
    fluidRow(
      box(
        title = "Troubleshooting",
        width = 12,
        solidHeader = TRUE,
        status = "warning",
        includeMarkdown("help/troubleshooting.md")
      )
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
        menuItem("PRISM Data", tabName = "prism", icon = icon("cloud-rain")),
        menuItem("VIC Data", tabName = "vic", icon = icon("water")),
        menuItem("SMAP Data", tabName = "smap", icon = icon("satellite")),
        menuItem("GRACE Data", tabName = "grace", icon = icon("globe")),
        menuItem("Analysis", tabName = "analysis", icon = icon("chart-line")),
        menuItem("Spatial Map", tabName = "spatial_map", icon = icon("map")),
        menuItem("Precipitation Analysis", tabName = "precipitation", icon = icon("rain")),
        menuItem("SWE Anomalies", tabName = "swe_anomalies", icon = icon("snowflake")),
        menuItem("Help", tabName = "help", icon = icon("question"))
      )
    ),
    dashboardBody(
      # Include custom CSS
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
      ),
      
      tabItems(
        overview_tab_ui(),
        prism_tab_ui(),
        vic_tab_ui(),
        smap_tab_ui(),
        grace_tab_ui(),
        analysis_tab_ui(),
        analysis_output_tab_ui(),
        spatial_map_tab_ui(),
        precipitation_tab_ui(),
        swe_anomalies_tab_ui(),
        help_tab_ui()
      )
    )
  )
} 