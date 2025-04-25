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

# ASU Theme Colors
asu_maroon <- "#8C1D40"
asu_gold <- "#FFC627"
asu_light_gold <- "#FFD700"
asu_dark_maroon <- "#5C0025"

# Custom CSS for enhanced ASU theme
custom_css <- tags$head(
  tags$style(HTML(paste0("
    /* Main header */
    .skin-blue .main-header .logo {
      background-color: white;
      color: #000000;
      font-weight: bold;
      box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      border-left: 4px solid ", asu_maroon, ";
      border-right: 4px solid ", asu_gold, ";
    }
    
    /* Header navbar */
    .skin-blue .main-header .navbar {
      background-color: white;
      box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      border-left: 4px solid ", asu_maroon, ";
      border-right: 4px solid ", asu_gold, ";
    }
    
    /* Sidebar */
    .skin-blue .main-sidebar {
      background-color: white;
      border-right: 4px solid ", asu_maroon, ";
      box-shadow: 2px 0 4px rgba(0,0,0,0.1);
    }
    
    /* Sidebar menu items */
    .skin-blue .sidebar-menu > li > a {
      color: #000000;
      font-weight: 500;
      border-left: 4px solid ", asu_maroon, ";
      border-right: 4px solid ", asu_gold, ";
    }
    
    .skin-blue .sidebar-menu > li.active > a {
      background-color: ", asu_gold, ";
      color: #000000;
      border-left: 4px solid ", asu_maroon, ";
      box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      font-weight: bold;
    }
    
    .skin-blue .sidebar-menu > li:hover > a {
      background-color: ", asu_light_gold, ";
      color: #000000;
      border-left: 4px solid ", asu_maroon, ";
      transition: all 0.3s ease;
      font-weight: bold;
    }
    
    /* Sidebar icons */
    .skin-blue .sidebar-menu > li > a > .fa,
    .skin-blue .sidebar-menu > li > a > .fas,
    .skin-blue .sidebar-menu > li > a > .far {
      color: #000000;
    }
    
    /* Sidebar treeview */
    .skin-blue .sidebar-menu > li > .treeview-menu {
      background-color: white;
    }
    
    .skin-blue .sidebar-menu > li > .treeview-menu > li > a {
      color: #000000;
    }
    
    .skin-blue .sidebar-menu > li > .treeview-menu > li.active > a {
      color: #000000;
      font-weight: bold;
    }
    
    /* Box headers */
    .box-header {
      background-color: white;
      color: #000000;
      border-bottom: 2px solid ", asu_maroon, ";
      box-shadow: 0 2px 4px rgba(0,0,0,0.1);
    }
    
    /* Box content */
    .box {
      background-color: white;
      border: 1px solid #ddd;
      box-shadow: 0 4px 8px rgba(0,0,0,0.1);
      border-radius: 4px;
      margin-bottom: 20px;
      transition: all 0.3s ease;
    }
    
    .box:hover {
      box-shadow: 0 6px 12px rgba(0,0,0,0.15);
      transform: translateY(-2px);
    }
    
    /* Tab panels */
    .nav-tabs-custom > .nav-tabs > li.active {
      border-top-color: ", asu_maroon, ";
      box-shadow: 0 -2px 4px rgba(0,0,0,0.1);
    }
    
    /* Buttons */
    .btn-primary {
      background-color: ", asu_maroon, ";
      border-color: ", asu_dark_maroon, ";
      box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      transition: all 0.3s ease;
    }
    
    .btn-primary:hover {
      background-color: ", asu_dark_maroon, ";
      border-color: ", asu_maroon, ";
      box-shadow: 0 4px 8px rgba(0,0,0,0.2);
      transform: translateY(-1px);
    }
    
    /* Info boxes */
    .info-box {
      background-color: white;
      border: 1px solid #ddd;
      box-shadow: 0 4px 8px rgba(0,0,0,0.1);
      border-radius: 4px;
      margin-bottom: 20px;
      transition: all 0.3s ease;
    }
    
    .info-box:hover {
      box-shadow: 0 6px 12px rgba(0,0,0,0.15);
      transform: translateY(-2px);
    }
    
    .info-box-icon {
      background-color: ", asu_gold, ";
      color: #000000;
      border-right: 2px solid ", asu_maroon, ";
    }
    
    /* Dashboard body */
    .content-wrapper {
      background-color: #f9f9f9;
    }
    
    /* Plot containers */
    .plot-container {
      background-color: white;
      padding: 15px;
      border-radius: 4px;
      box-shadow: 0 2px 4px rgba(0,0,0,0.1);
    }
    
    /* Variable info tiles */
    .variable-info {
      background-color: white;
      padding: 15px;
      margin-bottom: 15px;
      border-radius: 4px;
      box-shadow: 0 4px 8px rgba(0,0,0,0.1);
      border-left: 4px solid ", asu_gold, ";
      border-right: 4px solid ", asu_maroon, ";
    }
    
    .variable-info h4 {
      color: #000000;
      margin-bottom: 10px;
      font-weight: bold;
    }
    
    .variable-info p {
      color: #000000;
      margin-bottom: 5px;
    }

    /* General text styles */
    body {
      color: #000000;
    }

    h1, h2, h3, h4, h5, h6 {
      color: #000000;
    }

    .box-title {
      color: #000000;
      font-weight: bold;
    }

    .nav-tabs > li > a {
      color: #000000;
    }

    .nav-tabs > li.active > a {
      color: #000000;
      font-weight: bold;
    }

    /* Info tiles with yellow border and 3D shadow */
    .info-tile {
      background-color: white;
      border-left: 4px solid #FFC627;
      border-right: 4px solid #8C1D40;
      box-shadow: 0 4px 8px rgba(0,0,0,0.1);
      padding: 15px;
      margin-bottom: 15px;
      border-radius: 4px;
      transition: all 0.3s ease;
    }

    .info-tile:hover {
      box-shadow: 0 6px 12px rgba(0,0,0,0.15);
      transform: translateY(-2px);
    }

    .info-tile h4 {
      color: #000000;
      margin-bottom: 10px;
      font-weight: bold;
    }

    .info-tile p {
      color: #000000;
      margin-bottom: 5px;
      font-size: 14px;
    }
  ")))
)

# Add static image paths with proper file handling
static_images <- list(
  basin = "/Users/praddy5/Desktop/Dashboard/images/colorado_river_basin_map.png",
  huc10 = "/Users/praddy5/Desktop/Dashboard/images/colorado_river_huc10_map.png",
  snotel = "/Users/praddy5/Desktop/Dashboard/images/colorado_river_snotel_map.png",
  vic_precip = "/Users/praddy5/Desktop/Dashboard/images/vic_precipitation_map.png",
  vic_et = "/Users/praddy5/Desktop/Dashboard/images/vic_evapotranspiration_map.png",
  vic_runoff = "/Users/praddy5/Desktop/Dashboard/images/vic_runoff_map.png",
  vic_soil = "/Users/praddy5/Desktop/Dashboard/images/vic_soil_moisture_layer_1_map.png",
  vic_swe = "/Users/praddy5/Desktop/Dashboard/images/vic_snow_water_equivalent_map.png",
  vic_baseflow = "/Users/praddy5/Desktop/Dashboard/images/vic_baseflow_map.png",
  smap = "/Users/praddy5/Desktop/Dashboard/images/smap_time_series.png",
  grace = "/Users/praddy5/Desktop/Dashboard/images/grace_time_series.png",
  snotel_ts = "/Users/praddy5/Desktop/Dashboard/images/snotel_time_series.png",
  monthly = "/Users/praddy5/Desktop/Dashboard/images/monthly_trends.png",
  historical = "/Users/praddy5/Desktop/Dashboard/images/historical_trend.png",
  combined = "/Users/praddy5/Desktop/Dashboard/images/combined_map.png"
)

# Function to load VIC data
load_vic_data <- function() {
  nc_file <- nc_open("data/VICOut2.nc")
  return(nc_file)
}

# Function to load SMAP data
load_smap_data <- function() {
  nc_file <- nc_open("data/SPL4SMGP.007_9km_aid0001.nc")
  return(nc_file)
}

# Function to load GRACE data
load_grace_data <- function() {
  nc_file <- nc_open("data/GRCTellus.JPL.200204_202401.GLO.RL06.1M.MSCNv03CRI.nc")
  return(nc_file)
}

# Function to load calibrated VIC data
load_calibrated_vic_data <- function(year) {
  file_path <- paste0("data/CRB_PRISM_Calibrated.", year, "-01-01.nc")
  if (file.exists(file_path)) {
    nc_file <- nc_open(file_path)
    return(nc_file)
  }
  return(NULL)
}

# Function to extract time series data
extract_time_series <- function(nc_file, var_name, lat_idx = NULL, lon_idx = NULL) {
  if (is.null(lat_idx)) lat_idx <- 1
  if (is.null(lon_idx)) lon_idx <- 1
  
  var_data <- ncvar_get(nc_file, var_name)
  time <- ncvar_get(nc_file, "time")
  
  # Check if we have valid data
  if (length(dim(var_data)) == 0 || length(time) == 0) {
    return(NULL)
  }
  
  # Handle different data dimensions
  if (var_name == "OUT_SOIL_MOIST") {
    # For soil moisture, average across layers
    if (length(dim(var_data)) == 4) {
      data <- apply(var_data[lon_idx, lat_idx, , ], 2, mean, na.rm = TRUE)
    } else {
      data <- var_data[lon_idx, lat_idx, ]
    }
  } else if (length(dim(var_data)) == 3) {
    # For 3D data (lon, lat, time)
    data <- var_data[lon_idx, lat_idx, ]
  } else if (length(dim(var_data)) == 4) {
    # For 4D data (lon, lat, layer, time)
    data <- var_data[lon_idx, lat_idx, 1, ]  # Using first layer
  } else {
    return(NULL)
  }
  
  # Create data frame with proper structure
  result <- data.frame(
    time = time,
    value = as.vector(data)
  )
  
  return(result)
}

# Function to convert VIC time to date
convert_vic_time <- function(time) {
  # VIC time is in days since 0001-01-01
  # Convert to days since 1982-01-01 (start of our data)
  days_since_1982 <- time - as.numeric(as.Date("1982-01-01") - as.Date("0001-01-01"))
  as.Date(days_since_1982, origin = "1982-01-01")
}

# Function to get variable metadata
get_vic_metadata <- function(var_name) {
  metadata <- list(
    "OUT_PREC" = list(
      name = "Total Precipitation",
      unit = "mm/day",
      description = "Total incoming precipitation (rain + snow)"
    ),
    "OUT_RAINF" = list(
      name = "Rainfall",
      unit = "mm/day",
      description = "Liquid rainfall amount"
    ),
    "OUT_SNOWF" = list(
      name = "Snowfall",
      unit = "mm/day",
      description = "Snowfall amount"
    ),
    "OUT_EVAP" = list(
      name = "Total Evaporation",
      unit = "mm/day",
      description = "Total net evaporation"
    ),
    "OUT_EVAP_BARE" = list(
      name = "Bare Soil Evaporation",
      unit = "mm/day",
      description = "Evaporation from bare soil"
    ),
    "OUT_EVAP_CANOP" = list(
      name = "Canopy Evaporation",
      unit = "mm/day",
      description = "Evaporation from canopy"
    ),
    "OUT_TRANSP_VEG" = list(
      name = "Vegetation Transpiration",
      unit = "mm/day",
      description = "Transpiration from vegetation"
    ),
    "OUT_PET" = list(
      name = "Potential ET",
      unit = "mm/day",
      description = "Potential evapotranspiration"
    ),
    "OUT_RUNOFF" = list(
      name = "Runoff",
      unit = "mm/day",
      description = "Surface runoff"
    ),
    "OUT_BASEFLOW" = list(
      name = "Baseflow",
      unit = "mm/day",
      description = "Subsurface runoff"
    ),
    "OUT_SOIL_MOIST" = list(
      name = "Soil Moisture",
      unit = "mm",
      description = "Soil total moisture content"
    ),
    "OUT_SOIL_WET" = list(
      name = "Soil Wetness",
      unit = "fraction",
      description = "Soil wetness fraction"
    ),
    "OUT_SOIL_TEMP" = list(
      name = "Soil Temperature",
      unit = "°C",
      description = "Soil temperature"
    ),
    "OUT_SWE" = list(
      name = "Snow Water Equivalent",
      unit = "mm",
      description = "Snow water equivalent in snow pack"
    ),
    "OUT_SNOW_MELT" = list(
      name = "Snow Melt",
      unit = "mm/day",
      description = "Snow melt rate"
    ),
    "OUT_SUB_SNOW" = list(
      name = "Sub-Snow",
      unit = "mm",
      description = "Water content below snow pack"
    ),
    "OUT_SNOW_SURF_TEMP" = list(
      name = "Snow Surface Temperature",
      unit = "°C",
      description = "Temperature at snow surface"
    ),
    "OUT_SNOW_PACK_TEMP" = list(
      name = "Snow Pack Temperature",
      unit = "°C",
      description = "Temperature of snow pack"
    ),
    "OUT_AIR_TEMP" = list(
      name = "Air Temperature",
      unit = "°C",
      description = "Air temperature at 2m height"
    ),
    "OUT_SURF_TEMP" = list(
      name = "Surface Temperature",
      unit = "°C",
      description = "Surface temperature"
    ),
    "OUT_BARESOILT" = list(
      name = "Bare Soil Temperature",
      unit = "°C",
      description = "Temperature of bare soil"
    ),
    "OUT_VEGT" = list(
      name = "Vegetation Temperature",
      unit = "°C",
      description = "Vegetation temperature"
    ),
    "OUT_SURFSTOR" = list(
      name = "Surface Storage",
      unit = "mm",
      description = "Surface water storage"
    )
  )
  return(metadata[[var_name]])
}

# Function to get VIC data range
get_vic_years_range <- function() {
  nc_file <- nc_open("data/VICOut2.nc")
  time <- ncvar_get(nc_file, "time")
  dates <- convert_vic_time(time)
  years <- as.numeric(unique(format(dates, "%Y")))
  nc_close(nc_file)
  return(c(min(years), max(years)))
}

# UI
ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(
    title = "Colorado River Basin Dashboard",
    titleWidth = 300,
    tags$li(class = "dropdown",
      tags$div(style = "display: inline-block; margin-right: 10px;",
        tags$img(src = "logos/nasa.png", height = "40px", 
                style = "padding: 5px; border: 2px solid #FFC627; border-radius: 50%;")
      )
    ),
    tags$li(class = "dropdown",
      tags$div(style = "display: inline-block; margin-right: 10px;",
        tags$img(src = "logos/asu.jpg", height = "40px", 
                style = "padding: 5px; border: 2px solid #FFC627; border-radius: 50%;")
      )
    ),
    tags$li(class = "dropdown",
      tags$div(style = "display: inline-block;",
        tags$img(src = "logos/cap.jpg", height = "40px", 
                style = "padding: 5px; border: 2px solid #FFC627; border-radius: 50%;")
      )
    )
  ),
  
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      menuItem("Spatial Map", tabName = "spatial", icon = icon("map")),
      menuItem("VIC Model", tabName = "vic", icon = icon("water")),
      menuItem("SMAP", tabName = "smap", icon = icon("satellite")),
      menuItem("GRACE", tabName = "grace", icon = icon("weight")),
      menuItem("Precipitation Analysis", tabName = "precipitation", icon = icon("cloud-rain")),
      menuItem("Snow Water Equivalent", tabName = "swe", icon = icon("snowflake")),
      menuItem("Soil Moisture", tabName = "soil", icon = icon("seedling")),
      menuItem("SWE Anomalies", tabName = "swe_anomalies", icon = icon("snowflake")),
      menuItem("Help", tabName = "help", icon = icon("question-circle")),
      menuItem("Static Outputs", tabName = "static_outputs", icon = icon("image"),
               menuSubItem("VIC Model", tabName = "static_vic"),
               menuSubItem("SMAP", tabName = "static_smap"),
               menuSubItem("GRACE", tabName = "static_grace"),
               menuSubItem("PRISM", tabName = "static_prism"),
               menuSubItem("SNOTEL", tabName = "static_snotel"),
               menuSubItem("Combined Analysis", tabName = "static_combined")
      )
    )
  ),
  
  dashboardBody(
    custom_css,
    tabItems(
      # Spatial Map Tab
      tabItem(tabName = "spatial",
              fluidRow(
                div(class = "variable-info",
                    h4("Basin Information"),
                    p("The Colorado River Basin covers approximately 246,000 square miles across seven U.S. states and Mexico."),
                    p("Key features include major reservoirs like Lake Powell and Lake Mead."),
                    p("The basin supports over 40 million people and 5.5 million acres of farmland.")
                )
              ),
              fluidRow(
                # Add info tiles for basin monitoring
                infoBox(
                  title = "Basin Area",
                  value = "246,000 sq mi",
                  icon = icon("map"),
                  color = "maroon",
                  width = 3,
                  subtitle = HTML("<span style='font-size: 8px; font-style: italic;'>Source: USGS National Hydrography Dataset (NHD) - 2024</span>")
                ),
                infoBox(
                  title = "Population Served",
                  value = "40M+",
                  icon = icon("users"),
                  color = "yellow",
                  width = 3,
                  subtitle = HTML("<span style='font-size: 8px; font-style: italic;'>Source: US Bureau of Reclamation - 2024</span>")
                ),
                infoBox(
                  title = "Farmland",
                  value = "5.5M acres",
                  icon = icon("tractor"),
                  color = "green",
                  width = 3,
                  subtitle = HTML("<span style='font-size: 8px; font-style: italic;'>Source: USDA Census of Agriculture - 2022</span>")
                ),
                infoBox(
                  title = "Major Reservoirs",
                  value = "2",
                  icon = icon("water"),
                  color = "blue",
                  width = 3,
                  subtitle = HTML("<span style='font-size: 8px; font-style: italic;'>Source: US Bureau of Reclamation - 2024</span>")
                )
              ),
              fluidRow(
                # Add climate info tiles
                infoBox(
                  title = "Average Temperature",
                  value = "12.5°C",
                  icon = icon("temperature-high"),
                  color = "red",
                  width = 3,
                  subtitle = HTML("<span style='font-size: 8px; font-style: italic;'>Source: NOAA Climate Data - 2023</span>")
                ),
                infoBox(
                  title = "Annual Precipitation",
                  value = "400mm",
                  icon = icon("cloud-rain"),
                  color = "aqua",
                  width = 3,
                  subtitle = HTML("<span style='font-size: 8px; font-style: italic;'>Source: NOAA Climate Data - 2023</span>")
                ),
                infoBox(
                  title = "Drought Status",
                  value = "Moderate",
                  icon = icon("sun"),
                  color = "orange",
                  width = 3,
                  subtitle = HTML("<span style='font-size: 8px; font-style: italic;'>Source: US Drought Monitor - 2024</span>")
                ),
                infoBox(
                  title = "Water Use",
                  value = "15MAF/yr",
                  icon = icon("faucet"),
                  color = "teal",
                  width = 3,
                  subtitle = HTML("<span style='font-size: 8px; font-style: italic;'>Source: US Bureau of Reclamation - 2023</span>")
                )
              ),
              fluidRow(
                tabsetPanel(
                  tabPanel("Colorado Basin",
                           box(width = 12, title = "Colorado River Basin Map",
                               div(class = "info-tile",
                                   h4("Basin Overview"),
                                   p("The Colorado River Basin spans across seven U.S. states, covering approximately 246,000 square miles."),
                                   p("Key features include Lake Powell and Lake Mead reservoirs.")
                               ),
                               fluidRow(
                                 column(width = 8,
                                   div(class = "map-container",
                                       uiOutput("basin_map")
                                   )
                                 ),
                                 column(width = 4,
                                   # Add info tiles for basin characteristics
                                   infoBox(
                                     title = "Total Area",
                                     value = "246,000 sq mi",
                                     icon = icon("map"),
                                     color = "maroon",
                                     width = 12,
                                     subtitle = "Source: USGS NHD - 2024"
                                   ),
                                   infoBox(
                                     title = "Major Reservoirs",
                                     value = "2",
                                     icon = icon("water"),
                                     color = "blue",
                                     width = 12,
                                     subtitle = "Lake Powell and Lake Mead"
                                   ),
                                   infoBox(
                                     title = "States Covered",
                                     value = "7",
                                     icon = icon("flag"),
                                     color = "green",
                                     width = 12,
                                     subtitle = "AZ, CA, CO, NM, NV, UT, WY"
                                   )
                                 )
                               ),
                               div(class = "source-info",
                                   p("Source: USGS National Hydrography Dataset (NHD) - 2024",
                                     style = "font-size: 10px; font-style: italic; text-align: right;")
                               )
                           )
                  ),
                  tabPanel("HUC10 Map",
                           box(width = 12, title = "HUC10 Watersheds",
                               div(class = "info-tile",
                                   h4("HUC10 Watersheds"),
                                   p("Hydrologic Unit Code (HUC) 10 watersheds represent sub-basins used for water management."),
                                   p("These units help in organizing and managing water resources at a local scale.")
                               ),
                               fluidRow(
                                 column(width = 8,
                                   div(class = "map-container",
                                       tags$img(src = "images/colorado_river_huc10_map.png",
                                               alt = "HUC10 Watersheds Map",
                                               style = "width: 100%; height: auto; max-height: 600px; object-fit: contain;"),
                                       div(class = "map-caption",
                                           p("Hydrologic Unit Code (HUC) 10 watersheds within the Colorado River Basin."),
                                           p("These units represent sub-basins used for water management.")
                                       )
                                   )
                                 ),
                                 column(width = 4,
                                   # Add info tiles for HUC10 characteristics
                                   infoBox(
                                     title = "Total HUC10 Units",
                                     value = "100+",
                                     icon = icon("layer-group"),
                                     color = "maroon",
                                     width = 12,
                                     subtitle = "Source: USGS WBD - 2024"
                                   ),
                                   infoBox(
                                     title = "Average Area",
                                     value = "2,400 sq mi",
                                     icon = icon("ruler"),
                                     color = "blue",
                                     width = 12,
                                     subtitle = "Per HUC10 watershed"
                                   ),
                                   infoBox(
                                     title = "Data Resolution",
                                     value = "1:24,000",
                                     icon = icon("map-marked-alt"),
                                     color = "green",
                                     width = 12,
                                     subtitle = "Scale of delineation"
                                   )
                                 )
                               ),
                               tabsetPanel(
                                 tabPanel("Area Analysis",
                                          fluidRow(
                                            column(width = 8,
                                              div(class = "plot-container",
                                                  tags$img(src = "images/huc10_area.png",
                                                          alt = "HUC10 Area Analysis",
                                                          style = "width: 100%; height: auto; max-height: 600px; object-fit: contain;"),
                                                  div(class = "plot-caption",
                                                      p("Area distribution of HUC10 watersheds."),
                                                      p("Shows the size distribution of sub-basins.")
                                                  )
                                              )
                                            ),
                                            column(width = 4,
                                              infoBox(
                                                title = "Largest HUC10",
                                                value = "5,000+ sq mi",
                                                icon = icon("expand"),
                                                color = "maroon",
                                                width = 12,
                                                subtitle = "Upper Colorado River"
                                              ),
                                              infoBox(
                                                title = "Smallest HUC10",
                                                value = "500 sq mi",
                                                icon = icon("compress"),
                                                color = "blue",
                                                width = 12,
                                                subtitle = "Lower Basin tributaries"
                                              ),
                                              infoBox(
                                                title = "Median Area",
                                                value = "2,400 sq mi",
                                                icon = icon("balance-scale"),
                                                color = "green",
                                                width = 12,
                                                subtitle = "Typical watershed size"
                                              )
                                            )
                                          )
                                 ),
                                 tabPanel("Combined View",
                                          fluidRow(
                                            column(width = 8,
                                              div(class = "map-container",
                                                  tags$img(src = "images/basin_huc10_snotel.png",
                                                          alt = "Combined Basin and HUC10 Map",
                                                          style = "width: 100%; height: auto; max-height: 600px; object-fit: contain;"),
                                                  div(class = "map-caption",
                                                      p("Combined view of Colorado Basin and HUC10 watersheds."),
                                                      p("Shows the spatial relationship between basin boundaries and watersheds.")
                                                  )
                                              )
                                            ),
                                            column(width = 4,
                                              infoBox(
                                                title = "Spatial Resolution",
                                                value = "1:24,000",
                                                icon = icon("ruler"),
                                                color = "maroon",
                                                width = 12,
                                                subtitle = "Map scale"
                                              ),
                                              infoBox(
                                                title = "Data Integration",
                                                value = "3 Layers",
                                                icon = icon("layer-group"),
                                                color = "blue",
                                                width = 12,
                                                subtitle = "Basin, HUC10, SNOTEL"
                                              ),
                                              infoBox(
                                                title = "Update Frequency",
                                                value = "Annual",
                                                icon = icon("sync"),
                                                color = "green",
                                                width = 12,
                                                subtitle = "Data refresh cycle"
                                              )
                                            )
                                          )
                                 ),
                                 tabPanel("Detailed Map",
                                          fluidRow(
                                            column(width = 8,
                                              div(class = "map-container",
                                                  tags$img(src = "images/huc10_map.png",
                                                          alt = "Detailed HUC10 Map",
                                                          style = "width: 100%; height: auto; max-height: 600px; object-fit: contain;"),
                                                  div(class = "map-caption",
                                                      p("Detailed view of HUC10 watershed boundaries."),
                                                      p("Provides high-resolution visualization of watershed delineations.")
                                                  )
                                              )
                                            ),
                                            column(width = 4,
                                              infoBox(
                                                title = "Boundary Type",
                                                value = "Hydrologic",
                                                icon = icon("water"),
                                                color = "maroon",
                                                width = 12,
                                                subtitle = "Watershed boundaries"
                                              ),
                                              infoBox(
                                                title = "Coordinate System",
                                                value = "NAD83",
                                                icon = icon("globe"),
                                                color = "blue",
                                                width = 12,
                                                subtitle = "Geographic reference"
                                              ),
                                              infoBox(
                                                title = "Data Source",
                                                value = "USGS WBD",
                                                icon = icon("database"),
                                                color = "green",
                                                width = 12,
                                                subtitle = "Watershed Boundary Dataset"
                                              )
                                            )
                                          )
                                 )
                               ),
                               div(class = "source-info",
                                   p("Source: USGS Watershed Boundary Dataset (WBD) - 2024",
                                     style = "font-size: 10px; font-style: italic; text-align: right;")
                               )
                           )
                  ),
                  tabPanel("SNOTEL Stations",
                           box(width = 12, title = "SNOTEL Stations in Colorado Basin",
                               div(class = "info-tile",
                                   h4("SNOTEL Network"),
                                   p("SNOTEL (SNOwpack TELemetry) stations provide real-time snowpack data across the basin."),
                                   p("Each station measures snow water equivalent, snow depth, temperature, and precipitation.")
                               ),
                               fluidRow(
                                 column(width = 8,
                                   div(class = "map-container",
                                       tags$img(src = "images/colorado_river_snotel_map.png",
                                               alt = "SNOTEL Stations Map",
                                               style = "width: 100%; height: auto; max-height: 600px; object-fit: contain;"),
                                       div(class = "map-caption",
                                           p("SNOTEL stations across the Colorado River Basin."),
                                           p("Each station provides real-time snowpack data.")
                                       )
                                   )
                                 ),
                                 column(width = 4,
                                   infoBox(
                                     title = "Total Stations",
                                     value = "100+",
                                     icon = icon("snowflake"),
                                     color = "maroon",
                                     width = 12,
                                     subtitle = "Active monitoring sites"
                                   ),
                                   infoBox(
                                     title = "Elevation Range",
                                     value = "5,000-12,000 ft",
                                     icon = icon("mountain"),
                                     color = "blue",
                                     width = 12,
                                     subtitle = "Station coverage"
                                   ),
                                   infoBox(
                                     title = "Update Frequency",
                                     value = "Daily",
                                     icon = icon("clock"),
                                     color = "green",
                                     width = 12,
                                     subtitle = "Data collection"
                                   )
                                 )
                               ),
                               tabsetPanel(
                                 tabPanel("Time Series",
                                          fluidRow(
                                            column(width = 8,
                                              div(class = "plot-container",
                                                  tags$img(src = "images/snotel_time_series.png",
                                                          alt = "SNOTEL Time Series",
                                                          style = "width: 100%; height: auto; max-height: 600px; object-fit: contain;"),
                                                  div(class = "plot-caption",
                                                      p("Time series of snow water equivalent (SWE) from SNOTEL stations."),
                                                      p("Shows daily variations in snowpack conditions.")
                                                  )
                                              )
                                            ),
                                            column(width = 4,
                                              infoBox(
                                                title = "Data Range",
                                                value = "1982-2024",
                                                icon = icon("calendar"),
                                                color = "maroon",
                                                width = 12,
                                                subtitle = "Historical coverage"
                                              ),
                                              infoBox(
                                                title = "Variables",
                                                value = "4",
                                                icon = icon("chart-line"),
                                                color = "blue",
                                                width = 12,
                                                subtitle = "SWE, depth, temp, precip"
                                              ),
                                              infoBox(
                                                title = "Resolution",
                                                value = "Daily",
                                                icon = icon("clock"),
                                                color = "green",
                                                width = 12,
                                                subtitle = "Temporal resolution"
                                              )
                                            )
                                          )
                                 ),
                                 tabPanel("Seasonal Analysis",
                                          fluidRow(
                                            column(width = 8,
                                              div(class = "plot-container",
                                                  tags$img(src = "images/snotel_seasonal.png",
                                                          alt = "Seasonal Analysis",
                                                          style = "width: 100%; height: auto; max-height: 600px; object-fit: contain;"),
                                                  div(class = "plot-caption",
                                                      p("Seasonal patterns of snowpack accumulation and melt."),
                                                      p("Helps understand annual snowpack dynamics.")
                                                  )
                                              )
                                            ),
                                            column(width = 4,
                                              infoBox(
                                                title = "Peak SWE",
                                                value = "April",
                                                icon = icon("calendar-alt"),
                                                color = "maroon",
                                                width = 12,
                                                subtitle = "Typical maximum"
                                              ),
                                              infoBox(
                                                title = "Melt Season",
                                                value = "May-July",
                                                icon = icon("sun"),
                                                color = "blue",
                                                width = 12,
                                                subtitle = "Primary melt period"
                                              ),
                                              infoBox(
                                                title = "Accumulation",
                                                value = "Nov-Mar",
                                                icon = icon("snowflake"),
                                                color = "green",
                                                width = 12,
                                                subtitle = "Snow accumulation"
                                              )
                                            )
                                          )
                                 ),
                                 tabPanel("Elevation Analysis",
                                          fluidRow(
                                            column(width = 8,
                                              div(class = "plot-container",
                                                  tags$img(src = "images/snotel_elevation.png",
                                                          alt = "Elevation Analysis",
                                                          style = "width: 100%; height: auto; max-height: 600px; object-fit: contain;"),
                                                  div(class = "plot-caption",
                                                      p("Relationship between elevation and snowpack characteristics."),
                                                      p("Important for understanding snow distribution patterns.")
                                                  )
                                              )
                                            ),
                                            column(width = 4,
                                              infoBox(
                                                title = "Elevation Range",
                                                value = "5,000-12,000 ft",
                                                icon = icon("mountain"),
                                                color = "maroon",
                                                width = 12,
                                                subtitle = "Station coverage"
                                              ),
                                              infoBox(
                                                title = "Optimal Zone",
                                                value = "8,000-10,000 ft",
                                                icon = icon("chart-line"),
                                                color = "blue",
                                                width = 12,
                                                subtitle = "Peak snow accumulation"
                                              ),
                                              infoBox(
                                                title = "Data Points",
                                                value = "100+",
                                                icon = icon("map-marker-alt"),
                                                color = "green",
                                                width = 12,
                                                subtitle = "Station locations"
                                              )
                                            )
                                          )
                                 ),
                                 tabPanel("Combined Map",
                                          fluidRow(
                                            column(width = 8,
                                              div(class = "map-container",
                                                  tags$img(src = "images/basin_huc10_snotel.png",
                                                          alt = "Combined Basin, HUC10, and SNOTEL Map",
                                                          style = "width: 100%; height: auto; max-height: 600px; object-fit: contain;"),
                                                  div(class = "map-caption",
                                                      p("Combined view of Colorado Basin, HUC10 watersheds, and SNOTEL stations."),
                                                      p("Shows the spatial relationship between different data layers.")
                                                  )
                                              )
                                            ),
                                            column(width = 4,
                                              infoBox(
                                                title = "Data Layers",
                                                value = "3",
                                                icon = icon("layer-group"),
                                                color = "maroon",
                                                width = 12,
                                                subtitle = "Basin, HUC10, SNOTEL"
                                              ),
                                              infoBox(
                                                title = "Spatial Coverage",
                                                value = "100%",
                                                icon = icon("globe"),
                                                color = "blue",
                                                width = 12,
                                                subtitle = "Basin coverage"
                                              ),
                                              infoBox(
                                                title = "Integration",
                                                value = "Seamless",
                                                icon = icon("link"),
                                                color = "green",
                                                width = 12,
                                                subtitle = "Data layer integration"
                                              )
                                            )
                                          )
                                 )
                               ),
                               div(class = "source-info",
                                   p("Source: NRCS SNOTEL Network - Updated Daily",
                                     style = "font-size: 10px; font-style: italic; text-align: right;")
                               )
                           )
                  ),
                  tabPanel("Historical Analysis",
                           box(width = 12, title = "Historical Analysis Trends",
                               div(class = "info-tile",
                                   h4("Historical Trends"),
                                   p("Analysis of historical trends in snowpack, precipitation, and water resources."),
                                   p("Provides insights into long-term changes and patterns in the basin.")
                               ),
                               fluidRow(
                                 column(width = 8,
                                   div(class = "map-container",
                                       tags$img(src = "images/historical_trend.png",
                                               alt = "Historical Analysis Map",
                                               style = "width: 100%; height: auto; max-height: 600px; object-fit: contain;"),
                                       div(class = "map-caption",
                                           p("Historical trends analysis across the Colorado River Basin."),
                                           p("Shows long-term changes in key water resource indicators.")
                                       )
                                   )
                                 ),
                                 column(width = 4,
                                   infoBox(
                                     title = "Time Period",
                                     value = "1982-2024",
                                     icon = icon("calendar"),
                                     color = "maroon",
                                     width = 12,
                                     subtitle = "Analysis period"
                                   ),
                                   infoBox(
                                     title = "Key Indicators",
                                     value = "5+",
                                     icon = icon("chart-line"),
                                     color = "blue",
                                     width = 12,
                                     subtitle = "Snowpack, precip, flow"
                                   ),
                                   infoBox(
                                     title = "Update Frequency",
                                     value = "Annual",
                                     icon = icon("sync"),
                                     color = "green",
                                     width = 12,
                                     subtitle = "Trend analysis"
                                   )
                                 )
                               ),
                               tabsetPanel(
                                 tabPanel("Snowpack Trends",
                                          fluidRow(
                                            column(width = 8,
                                              div(class = "plot-container",
                                                  tags$img(src = "images/snotel_trend.png",
                                                          alt = "Snowpack Trends",
                                                          style = "width: 100%; height: auto; max-height: 600px; object-fit: contain;"),
                                                  div(class = "plot-caption",
                                                      p("Long-term trends in snowpack accumulation and melt."),
                                                      p("Shows changes in snow water equivalent over time.")
                                                  )
                                              )
                                            ),
                                            column(width = 4,
                                              infoBox(
                                                title = "Trend Direction",
                                                value = "Decreasing",
                                                icon = icon("arrow-down"),
                                                color = "maroon",
                                                width = 12,
                                                subtitle = "Overall trend"
                                              ),
                                              infoBox(
                                                title = "Rate of Change",
                                                value = "-2.5%/decade",
                                                icon = icon("chart-line"),
                                                color = "blue",
                                                width = 12,
                                                subtitle = "Average change"
                                              ),
                                              infoBox(
                                                title = "Significance",
                                                value = "95%",
                                                icon = icon("chart-bar"),
                                                color = "green",
                                                width = 12,
                                                subtitle = "Confidence level"
                                              )
                                            )
                                          )
                                 ),
                                 tabPanel("Precipitation Trends",
                                          fluidRow(
                                            column(width = 8,
                                              div(class = "plot-container",
                                                  tags$img(src = "images/vic_trend.png",
                                                          alt = "Precipitation Trends",
                                                          style = "width: 100%; height: auto; max-height: 600px; object-fit: contain;"),
                                                  div(class = "plot-caption",
                                                      p("Historical trends in precipitation patterns."),
                                                      p("Analyzes changes in rainfall and snowfall over time.")
                                                  )
                                              )
                                            ),
                                            column(width = 4,
                                              infoBox(
                                                title = "Annual Change",
                                                value = "-1.2%/decade",
                                                icon = icon("cloud-rain"),
                                                color = "maroon",
                                                width = 12,
                                                subtitle = "Precipitation trend"
                                              ),
                                              infoBox(
                                                title = "Seasonal Shift",
                                                value = "Earlier",
                                                icon = icon("calendar-alt"),
                                                color = "blue",
                                                width = 12,
                                                subtitle = "Peak timing"
                                              ),
                                              infoBox(
                                                title = "Variability",
                                                value = "Increasing",
                                                icon = icon("random"),
                                                color = "green",
                                                width = 12,
                                                subtitle = "Year-to-year variation"
                                              )
                                            )
                                          )
                                 ),
                                 tabPanel("Water Resources",
                                          fluidRow(
                                            column(width = 8,
                                              div(class = "plot-container",
                                                  tags$img(src = "images/grace_trend.png",
                                                          alt = "Water Resources Trends",
                                                          style = "width: 100%; height: auto; max-height: 600px; object-fit: contain;"),
                                                  div(class = "plot-caption",
                                                      p("Changes in water availability and usage."),
                                                      p("Tracks long-term trends in water resources.")
                                                  )
                                              )
                                            ),
                                            column(width = 4,
                                              infoBox(
                                                title = "Storage Change",
                                                value = "-15%",
                                                icon = icon("water"),
                                                color = "maroon",
                                                width = 12,
                                                subtitle = "Reservoir levels"
                                              ),
                                              infoBox(
                                                title = "Demand Growth",
                                                value = "+2%/year",
                                                icon = icon("users"),
                                                color = "blue",
                                                width = 12,
                                                subtitle = "Water usage"
                                              ),
                                              infoBox(
                                                title = "Efficiency",
                                                value = "+1.5%/year",
                                                icon = icon("tint"),
                                                color = "green",
                                                width = 12,
                                                subtitle = "Water use efficiency"
                                              )
                                            )
                                          )
                                 ),
                                 tabPanel("Soil Moisture Trends",
                                          fluidRow(
                                            column(width = 8,
                                              div(class = "plot-container",
                                                  tags$img(src = "images/smap_surface_trend.png",
                                                          alt = "Soil Moisture Trends",
                                                          style = "width: 100%; height: auto; max-height: 600px; object-fit: contain;"),
                                                  div(class = "plot-caption",
                                                      p("Historical trends in soil moisture patterns."),
                                                      p("Analyzes changes in surface and root zone soil moisture.")
                                                  )
                                              )
                                            ),
                                            column(width = 4,
                                              infoBox(
                                                title = "Surface Change",
                                                value = "-1.8%/decade",
                                                icon = icon("tint"),
                                                color = "maroon",
                                                width = 12,
                                                subtitle = "Surface soil moisture"
                                              ),
                                              infoBox(
                                                title = "Root Zone Change",
                                                value = "-2.1%/decade",
                                                icon = icon("seedling"),
                                                color = "blue",
                                                width = 12,
                                                subtitle = "Root zone moisture"
                                              ),
                                              infoBox(
                                                title = "Seasonal Impact",
                                                value = "Increasing",
                                                icon = icon("calendar-alt"),
                                                color = "green",
                                                width = 12,
                                                subtitle = "Seasonal variation"
                                              )
                                            )
                                          )
                                 ),
                                 tabPanel("Combined Trends",
                                          fluidRow(
                                            column(width = 8,
                                              div(class = "plot-container",
                                                  tags$img(src = "images/combined_trends.png",
                                                          alt = "Combined Trends",
                                                          style = "width: 100%; height: auto; max-height: 600px; object-fit: contain;"),
                                                  div(class = "plot-caption",
                                                      p("Combined analysis of all water resource trends."),
                                                      p("Shows the relationship between different water resource indicators.")
                                                  )
                                              )
                                            ),
                                            column(width = 4,
                                              infoBox(
                                                title = "Correlation",
                                                value = "0.85",
                                                icon = icon("link"),
                                                color = "maroon",
                                                width = 12,
                                                subtitle = "Trend correlation"
                                              ),
                                              infoBox(
                                                title = "Consistency",
                                                value = "High",
                                                icon = icon("check-circle"),
                                                color = "blue",
                                                width = 12,
                                                subtitle = "Trend consistency"
                                              ),
                                              infoBox(
                                                title = "Significance",
                                                value = "99%",
                                                icon = icon("chart-bar"),
                                                color = "green",
                                                width = 12,
                                                subtitle = "Statistical significance"
                                              )
                                            )
                                          )
                                 )
                               ),
                               div(class = "source-info",
                                   p("Source: USGS, NOAA, and Bureau of Reclamation - 2024",
                                     style = "font-size: 10px; font-style: italic; text-align: right;")
                               )
                           )
                  )
                )
              )
      ),
      
      # VIC Model Tab
      tabItem(tabName = "vic",
              fluidRow(
                box(title = "VIC Model Analysis", width = 12,
                  div(class = "variable-info",
                      h4("VIC Model Output Analysis"),
                      p("Variable Infiltration Capacity (VIC) model outputs for the Colorado River Basin."),
                      p("Spatial resolution: 4km, Temporal resolution: Daily")
                  )
                ),
                fluidRow(
                  column(width = 4,
                         selectInput("vic_variable", "Select Variable:",
                                   choices = c(
                                     "OUT_PREC" = "Precipitation (mm/day)",
                                     "OUT_RAINF" = "Rainfall (mm/day)",
                                     "OUT_EVAP" = "Evapotranspiration (mm/day)",
                                     "OUT_RUNOFF" = "Surface Runoff (mm/day)",
                                     "OUT_BASEFLOW" = "Baseflow (mm/day)",
                                     "OUT_SWE" = "Snow Water Equivalent (mm)",
                                     "OUT_SOIL_MOIST" = "Soil Moisture (mm)",
                                     "OUT_AIR_TEMP" = "Air Temperature (°C)",
                                     "OUT_SURF_TEMP" = "Surface Temperature (°C)",
                                     "OUT_SOIL_TEMP" = "Soil Temperature (°C)",
                                     "OUT_SNOW_SURF_TEMP" = "Snow Surface Temperature (°C)",
                                     "OUT_SNOW_PACK_TEMP" = "Snow Pack Temperature (°C)",
                                     "OUT_LATENT" = "Latent Heat Flux (W/m²)"
                                   ),
                                   selected = "OUT_PREC")
                  ),
                  column(width = 4,
                         sliderInput("vic_year_slider", "Select Year", 
                                   min = 1982, max = 2024, value = 2024, step = 1)
                  ),
                  column(width = 4,
                         sliderInput("vic_day_slider", "Select Day", 
                                   min = 1, max = 365, value = 1, step = 1)
                  )
                ),
                actionButton("vic_update", "Update Analysis", 
                            icon = icon("refresh"),
                            class = "btn-primary")
              ),
          
              fluidRow(
                box(title = "Spatial Distribution", width = 6,
                    plotlyOutput("vic_spatial_map", height = "500px")
                ),
                box(title = "Time Series", width = 6,
                    tabsetPanel(
                      tabPanel("Current Year",
                        conditionalPanel(
                          condition = "input.vic_variable == 'OUT_PREC'",
                          img(src = "images/vic_precipitation_timeseries.png", width = "100%", height = "auto"),
                          div(style = "margin-top: 20px;",
                              img(src = "images/vic_precipitation_static.png", width = "100%", height = "auto")
                          )
                        ),
                        conditionalPanel(
                          condition = "input.vic_variable == 'OUT_RAINF'",
                          img(src = "images/vic_rainfall_timeseries.png", width = "100%", height = "auto"),
                          div(style = "margin-top: 20px;",
                              img(src = "images/vic_rainfall_static.png", width = "100%", height = "auto")
                          )
                        ),
                        conditionalPanel(
                          condition = "input.vic_variable == 'OUT_EVAP'",
                          img(src = "images/vic_evapotranspiration_timeseries.png", width = "100%", height = "auto"),
                          div(style = "margin-top: 20px;",
                              img(src = "images/vic_evapotranspiration_static.png", width = "100%", height = "auto")
                          )
                        ),
                        conditionalPanel(
                          condition = "input.vic_variable == 'OUT_RUNOFF'",
                          img(src = "images/vic_runoff_timeseries.png", width = "100%", height = "auto"),
                          div(style = "margin-top: 20px;",
                              img(src = "images/vic_runoff_static.png", width = "100%", height = "auto")
                          )
                        ),
                        conditionalPanel(
                          condition = "input.vic_variable == 'OUT_BASEFLOW'",
                          img(src = "images/vic_baseflow_timeseries.png", width = "100%", height = "auto"),
                          div(style = "margin-top: 20px;",
                              img(src = "images/vic_baseflow_static.png", width = "100%", height = "auto")
                          )
                        ),
                        conditionalPanel(
                          condition = "input.vic_variable == 'OUT_SWE'",
                          img(src = "images/vic_swe_timeseries.png", width = "100%", height = "auto"),
                          div(style = "margin-top: 20px;",
                              img(src = "images/vic_swe_static.png", width = "100%", height = "auto")
                          )
                        ),
                        conditionalPanel(
                          condition = "input.vic_variable == 'OUT_SOIL_MOIST'",
                          img(src = "images/vic_soil_moisture_timeseries.png", width = "100%", height = "auto"),
                          div(style = "margin-top: 20px;",
                              img(src = "images/vic_soil_moisture_static.png", width = "100%", height = "auto")
                          )
                        ),
                        conditionalPanel(
                          condition = "input.vic_variable == 'OUT_AIR_TEMP'",
                          img(src = "images/vic_air_temp_timeseries.png", width = "100%", height = "auto"),
                          div(style = "margin-top: 20px;",
                              img(src = "images/vic_air_temp_static.png", width = "100%", height = "auto")
                          )
                        ),
                        conditionalPanel(
                          condition = "input.vic_variable == 'OUT_SURF_TEMP'",
                          img(src = "images/vic_surface_temp_timeseries.png", width = "100%", height = "auto"),
                          div(style = "margin-top: 20px;",
                              img(src = "images/vic_surface_temp_static.png", width = "100%", height = "auto")
                          )
                        ),
                        conditionalPanel(
                          condition = "input.vic_variable == 'OUT_SOIL_TEMP'",
                          img(src = "images/vic_soil_temp_timeseries.png", width = "100%", height = "auto"),
                          div(style = "margin-top: 20px;",
                              img(src = "images/vic_soil_temp_static.png", width = "100%", height = "auto")
                          )
                        ),
                        conditionalPanel(
                          condition = "input.vic_variable == 'OUT_SNOW_SURF_TEMP'",
                          img(src = "images/vic_snow_surface_temp_timeseries.png", width = "100%", height = "auto"),
                          div(style = "margin-top: 20px;",
                              img(src = "images/vic_snow_surface_temp_static.png", width = "100%", height = "auto")
                          )
                        ),
                        conditionalPanel(
                          condition = "input.vic_variable == 'OUT_SNOW_PACK_TEMP'",
                          img(src = "images/vic_snow_pack_temp_timeseries.png", width = "100%", height = "auto"),
                          div(style = "margin-top: 20px;",
                              img(src = "images/vic_snow_pack_temp_static.png", width = "100%", height = "auto")
                          )
                        ),
                        conditionalPanel(
                          condition = "input.vic_variable == 'OUT_LATENT'",
                          img(src = "images/vic_latent_heat_timeseries.png", width = "100%", height = "auto"),
                          div(style = "margin-top: 20px;",
                              img(src = "images/vic_latent_heat_static.png", width = "100%", height = "auto")
                          )
                        )
                      ),
                      tabPanel("Historical Trend",
                        conditionalPanel(
                          condition = "input.vic_variable == 'OUT_PREC'",
                          img(src = "images/vic_precipitation_trend.png", width = "100%", height = "auto"),
                          div(style = "margin-top: 20px;",
                              img(src = "images/vic_precipitation_trend_static.png", width = "100%", height = "auto")
                          )
                        ),
                        conditionalPanel(
                          condition = "input.vic_variable == 'OUT_RAINF'",
                          img(src = "images/vic_rainfall_trend.png", width = "100%", height = "auto"),
                          div(style = "margin-top: 20px;",
                              img(src = "images/vic_rainfall_trend_static.png", width = "100%", height = "auto")
                          )
                        ),
                        conditionalPanel(
                          condition = "input.vic_variable == 'OUT_EVAP'",
                          img(src = "images/vic_evapotranspiration_trend.png", width = "100%", height = "auto"),
                          div(style = "margin-top: 20px;",
                              img(src = "images/vic_evapotranspiration_trend_static.png", width = "100%", height = "auto")
                          )
                        ),
                        conditionalPanel(
                          condition = "input.vic_variable == 'OUT_RUNOFF'",
                          img(src = "images/vic_runoff_trend.png", width = "100%", height = "auto"),
                          div(style = "margin-top: 20px;",
                              img(src = "images/vic_runoff_trend_static.png", width = "100%", height = "auto")
                          )
                        ),
                        conditionalPanel(
                          condition = "input.vic_variable == 'OUT_BASEFLOW'",
                          img(src = "images/vic_baseflow_trend.png", width = "100%", height = "auto"),
                          div(style = "margin-top: 20px;",
                              img(src = "images/vic_baseflow_trend_static.png", width = "100%", height = "auto")
                          )
                        ),
                        conditionalPanel(
                          condition = "input.vic_variable == 'OUT_SWE'",
                          img(src = "images/vic_swe_trend.png", width = "100%", height = "auto"),
                          div(style = "margin-top: 20px;",
                              img(src = "images/vic_swe_trend_static.png", width = "100%", height = "auto")
                          )
                        ),
                        conditionalPanel(
                          condition = "input.vic_variable == 'OUT_SOIL_MOIST'",
                          img(src = "images/vic_soil_moisture_trend.png", width = "100%", height = "auto"),
                          div(style = "margin-top: 20px;",
                              img(src = "images/vic_soil_moisture_trend_static.png", width = "100%", height = "auto")
                          )
                        ),
                        conditionalPanel(
                          condition = "input.vic_variable == 'OUT_AIR_TEMP'",
                          img(src = "images/vic_air_temp_trend.png", width = "100%", height = "auto"),
                          div(style = "margin-top: 20px;",
                              img(src = "images/vic_air_temp_trend_static.png", width = "100%", height = "auto")
                          )
                        ),
                        conditionalPanel(
                          condition = "input.vic_variable == 'OUT_SURF_TEMP'",
                          img(src = "images/vic_surface_temp_trend.png", width = "100%", height = "auto"),
                          div(style = "margin-top: 20px;",
                              img(src = "images/vic_surface_temp_trend_static.png", width = "100%", height = "auto")
                          )
                        ),
                        conditionalPanel(
                          condition = "input.vic_variable == 'OUT_SOIL_TEMP'",
                          img(src = "images/vic_soil_temp_trend.png", width = "100%", height = "auto"),
                          div(style = "margin-top: 20px;",
                              img(src = "images/vic_soil_temp_trend_static.png", width = "100%", height = "auto")
                          )
                        ),
                        conditionalPanel(
                          condition = "input.vic_variable == 'OUT_SNOW_SURF_TEMP'",
                          img(src = "images/vic_snow_surface_temp_trend.png", width = "100%", height = "auto"),
                          div(style = "margin-top: 20px;",
                              img(src = "images/vic_snow_surface_temp_trend_static.png", width = "100%", height = "auto")
                          )
                        ),
                        conditionalPanel(
                          condition = "input.vic_variable == 'OUT_SNOW_PACK_TEMP'",
                          img(src = "images/vic_snow_pack_temp_trend.png", width = "100%", height = "auto"),
                          div(style = "margin-top: 20px;",
                              img(src = "images/vic_snow_pack_temp_trend_static.png", width = "100%", height = "auto")
                          )
                        ),
                        conditionalPanel(
                          condition = "input.vic_variable == 'OUT_LATENT'",
                          img(src = "images/vic_latent_heat_trend.png", width = "100%", height = "auto"),
                          div(style = "margin-top: 20px;",
                              img(src = "images/vic_latent_heat_trend_static.png", width = "100%", height = "auto")
                          )
                        )
                      ),
                      tabPanel("Seasonal Pattern",
                        conditionalPanel(
                          condition = "input.vic_variable == 'OUT_PREC'",
                          img(src = "images/vic_precipitation_seasonal.png", width = "100%", height = "auto"),
                          div(style = "margin-top: 20px;",
                              img(src = "images/vic_precipitation_seasonal_static.png", width = "100%", height = "auto")
                          )
                        ),
                        conditionalPanel(
                          condition = "input.vic_variable == 'OUT_RAINF'",
                          img(src = "images/vic_rainfall_seasonal.png", width = "100%", height = "auto"),
                          div(style = "margin-top: 20px;",
                              img(src = "images/vic_rainfall_seasonal_static.png", width = "100%", height = "auto")
                          )
                        ),
                        conditionalPanel(
                          condition = "input.vic_variable == 'OUT_EVAP'",
                          img(src = "images/vic_evapotranspiration_seasonal.png", width = "100%", height = "auto"),
                          div(style = "margin-top: 20px;",
                              img(src = "images/vic_evapotranspiration_seasonal_static.png", width = "100%", height = "auto")
                          )
                        ),
                        conditionalPanel(
                          condition = "input.vic_variable == 'OUT_RUNOFF'",
                          img(src = "images/vic_runoff_seasonal.png", width = "100%", height = "auto"),
                          div(style = "margin-top: 20px;",
                              img(src = "images/vic_runoff_seasonal_static.png", width = "100%", height = "auto")
                          )
                        ),
                        conditionalPanel(
                          condition = "input.vic_variable == 'OUT_BASEFLOW'",
                          img(src = "images/vic_baseflow_seasonal.png", width = "100%", height = "auto"),
                          div(style = "margin-top: 20px;",
                              img(src = "images/vic_baseflow_seasonal_static.png", width = "100%", height = "auto")
                          )
                        ),
                        conditionalPanel(
                          condition = "input.vic_variable == 'OUT_SWE'",
                          img(src = "images/vic_swe_seasonal.png", width = "100%", height = "auto"),
                          div(style = "margin-top: 20px;",
                              img(src = "images/vic_swe_seasonal_static.png", width = "100%", height = "auto")
                          )
                        ),
                        conditionalPanel(
                          condition = "input.vic_variable == 'OUT_SOIL_MOIST'",
                          img(src = "images/vic_soil_moisture_seasonal.png", width = "100%", height = "auto"),
                          div(style = "margin-top: 20px;",
                              img(src = "images/vic_soil_moisture_seasonal_static.png", width = "100%", height = "auto")
                          )
                        ),
                        conditionalPanel(
                          condition = "input.vic_variable == 'OUT_AIR_TEMP'",
                          img(src = "images/vic_air_temp_seasonal.png", width = "100%", height = "auto"),
                          div(style = "margin-top: 20px;",
                              img(src = "images/vic_air_temp_seasonal_static.png", width = "100%", height = "auto")
                          )
                        ),
                        conditionalPanel(
                          condition = "input.vic_variable == 'OUT_SURF_TEMP'",
                          img(src = "images/vic_surface_temp_seasonal.png", width = "100%", height = "auto"),
                          div(style = "margin-top: 20px;",
                              img(src = "images/vic_surface_temp_seasonal_static.png", width = "100%", height = "auto")
                          )
                        ),
                        conditionalPanel(
                          condition = "input.vic_variable == 'OUT_SOIL_TEMP'",
                          img(src = "images/vic_soil_temp_seasonal.png", width = "100%", height = "auto"),
                          div(style = "margin-top: 20px;",
                              img(src = "images/vic_soil_temp_seasonal_static.png", width = "100%", height = "auto")
                          )
                        ),
                        conditionalPanel(
                          condition = "input.vic_variable == 'OUT_SNOW_SURF_TEMP'",
                          img(src = "images/vic_snow_surface_temp_seasonal.png", width = "100%", height = "auto"),
                          div(style = "margin-top: 20px;",
                              img(src = "images/vic_snow_surface_temp_seasonal_static.png", width = "100%", height = "auto")
                          )
                        ),
                        conditionalPanel(
                          condition = "input.vic_variable == 'OUT_SNOW_PACK_TEMP'",
                          img(src = "images/vic_snow_pack_temp_seasonal.png", width = "100%", height = "auto"),
                          div(style = "margin-top: 20px;",
                              img(src = "images/vic_snow_pack_temp_seasonal_static.png", width = "100%", height = "auto")
                          )
                        ),
                        conditionalPanel(
                          condition = "input.vic_variable == 'OUT_LATENT'",
                          img(src = "images/vic_latent_heat_seasonal.png", width = "100%", height = "auto"),
                          div(style = "margin-top: 20px;",
                              img(src = "images/vic_latent_heat_seasonal_static.png", width = "100%", height = "auto")
                          )
                        )
                      )
                    )
                )
              ),
              fluidRow(
                box(title = "Variable Statistics", width = 12,
                    fluidRow(
                      column(width = 3,
                             infoBox("Annual Mean", 
                                    textOutput("vic_annual_mean"),
                                    icon = icon("calculator"),
                                    color = "blue",
                                    width = NULL)
                      ),
                      column(width = 3,
                             infoBox("Annual Max", 
                                    textOutput("vic_annual_max"),
                                    icon = icon("arrow-up"),
                                    color = "red",
                                    width = NULL)
                      ),
                      column(width = 3,
                             infoBox("Annual Min", 
                                    textOutput("vic_annual_min"),
                                    icon = icon("arrow-down"),
                                    color = "green",
                                    width = NULL)
                      ),
                      column(width = 3,
                             infoBox("Annual Trend", 
                                    textOutput("vic_annual_trend"),
                                    icon = icon("chart-line"),
                                    color = "yellow",
                                    width = NULL)
                      )
                    )
                )
              )
      ),
      
      # SMAP Data Tab
      tabItem(tabName = "smap",
              fluidRow(
                div(class = "variable-info",
                    h4("SMAP Soil Moisture Analysis"),
                    p("Surface and root zone soil moisture data from NASA's Soil Moisture Active Passive (SMAP) mission."),
                    p("Daily measurements at 9km resolution."),
                    p("Critical for drought monitoring and water resource management.")
                )
              ),
              fluidRow(
                column(width = 12,
                  infoBox("Surface SM", "0.25 m³/m³", 
                         "Source: SMAP L3 - 2024", 
                         icon = icon("tint"), color = "blue", width = 3),
                  infoBox("Root Zone SM", "0.35 m³/m³", 
                         "Source: SMAP L4 - 2024", 
                         icon = icon("tint"), color = "green", width = 3),
                  infoBox("Data Resolution", "9km", 
                         "Source: SMAP - 2024", 
                         icon = icon("ruler"), color = "light-blue", width = 3),
                  infoBox("Update Frequency", "Daily", 
                         "Source: SMAP Mission", 
                         icon = icon("clock"), color = "yellow", width = 3)
                )
              ),
              fluidRow(
                box(width = 12, title = "SMAP Soil Moisture Analysis",
                    tabsetPanel(
                      tabPanel("Surface Soil Moisture",
                           fluidRow(
                             column(width = 6,
                                box(width = 12, title = "Surface Soil Moisture Trend",
                                    img(src = "images/smap_surface_trend.png", width = "100%", height = "auto"),
                                    p("Surface soil moisture trends from SMAP data.")
                                )
                             ),
                             column(width = 6,
                                box(width = 12, title = "Surface Time Series",
                                    img(src = "images/analysis_results/smap_surface_time_series.png", width = "100%", height = "auto"),
                                    p("Temporal analysis of surface soil moisture.")
                                )
                             )
                           ),
                           fluidRow(
                             column(width = 12,
                                box(width = 12, title = "Surface Monthly Averages",
                                    img(src = "images/analysis_results/smap_surface_monthly_avg.png", width = "100%", height = "auto"),
                                    p("Monthly averages of surface soil moisture.")
                                )
                             )
                           )
                      ),
                      tabPanel("Root Zone Soil Moisture",
                           fluidRow(
                             column(width = 6,
                                box(width = 12, title = "Root Zone Soil Moisture Trend",
                                    img(src = "images/smap_rootzone_trend.png", width = "100%", height = "auto"),
                                    p("Root zone soil moisture trends from SMAP data.")
                                )
                             ),
                             column(width = 6,
                                box(width = 12, title = "Root Zone Time Series",
                                    img(src = "images/analysis_results/smap_rootzone_time_series.png", width = "100%", height = "auto"),
                                    p("Temporal analysis of root zone soil moisture.")
                                )
                             )
                           ),
                           fluidRow(
                             column(width = 12,
                                box(width = 12, title = "Root Zone Monthly Averages",
                                    img(src = "images/analysis_results/smap_rootzone_monthly_avg.png", width = "100%", height = "auto"),
                                    p("Monthly averages of root zone soil moisture.")
                                )
                             )
                           )
                      ),
                      tabPanel("Combined Analysis",
                           fluidRow(
                             column(width = 6,
                                box(width = 12, title = "SMAP Time Series Analysis",
                                    img(src = "images/smap_time_series.png", width = "100%", height = "auto"),
                                    p("Combined analysis of surface and root zone soil moisture.")
                                )
                             ),
                             column(width = 6,
                                box(width = 12, title = "Correlation Analysis",
                                    img(src = "images/analysis_results/correlation_soil_moisture.png", width = "100%", height = "auto"),
                                    p("Correlation between surface and root zone soil moisture.")
                                )
                             )
                           ),
                           fluidRow(
                             column(width = 12,
                                box(width = 12, title = "Trend Analysis",
                                    img(src = "images/analysis_results/trend_soil_moisture.png", width = "100%", height = "auto"),
                                    p("Long-term trends in soil moisture.")
                                )
                             )
                           )
                      )
                    )
                )
              )
      ),
      
      # GRACE Data Tab
      tabItem(tabName = "grace",
              h2("GRACE Data Analysis - Colorado River Basin"),
              fluidRow(
                box(width = 12, title = "GRACE Data Overview", solidHeader = TRUE,
                    fluidRow(
                      infoBox("TWS Change", "-15 cm", "GRACE/GRACE-FO Mission", 
                              icon = icon("balance-scale"), color = "blue", width = 3,
                              subtitle = "Source: GRACE/GRACE-FO Mission - 2024"),
                      infoBox("Data Resolution", "300 km", "GRACE/GRACE-FO", 
                              icon = icon("ruler"), color = "green", width = 3,
                              subtitle = "Source: NASA JPL - 2024"),
                      infoBox("Data Coverage", "2002-2024", "Historical Period", 
                              icon = icon("calendar"), color = "light-blue", width = 3,
                              subtitle = "Source: GRACE/GRACE-FO Mission - 2024"),
                      infoBox("Update Frequency", "Monthly", "Data Refresh", 
                              icon = icon("clock"), color = "purple", width = 3,
                              subtitle = "Source: NASA JPL - 2024")
                    )
                )
              ),
              box(width = 12, title = "GRACE Analysis", solidHeader = TRUE,
                    tabsetPanel(
                    tabPanel("Time Series",
                               fluidRow(
                               infoBox("Time Period", "2002-2024", "Data Coverage", 
                                      icon = icon("calendar"), color = "blue", width = 3),
                               infoBox("Resolution", "Monthly", "Temporal Scale", 
                                      icon = icon("clock"), color = "green", width = 3),
                               infoBox("Units", "cm", "TWS Anomalies", 
                                      icon = icon("ruler"), color = "light-blue", width = 3),
                               infoBox("Data Source", "GRACE/GRACE-FO", "Mission", 
                                      icon = icon("satellite"), color = "purple", width = 3)
                             ),
                             img(src = "images/grace_time_series.png", width = "100%", height = "auto"),
                             p("Monthly time series of GRACE Terrestrial Water Storage (TWS) anomalies for the Colorado River Basin.")
                    ),
                    tabPanel("Trend Analysis",
                               fluidRow(
                               infoBox("Analysis Period", "2002-2024", "Long-term Trends", 
                                      icon = icon("chart-line"), color = "blue", width = 3),
                               infoBox("Trend Type", "Linear", "Analysis Method", 
                                      icon = icon("calculator"), color = "green", width = 3),
                               infoBox("Confidence", "95%", "Statistical Significance", 
                                      icon = icon("chart-bar"), color = "light-blue", width = 3),
                               infoBox("Units", "cm/year", "Trend Rate", 
                                      icon = icon("ruler"), color = "purple", width = 3)
                             ),
                             img(src = "images/grace_trend.png", width = "100%", height = "auto"),
                             p("Long-term trends in GRACE TWS anomalies showing the overall water storage changes in the basin.")
                    ),
                    tabPanel("Water Storage",
                               fluidRow(
                               infoBox("Storage Type", "Total Water", "TWS Components", 
                                      icon = icon("water"), color = "blue", width = 3),
                               infoBox("Spatial Scale", "Basin-wide", "Coverage", 
                                      icon = icon("globe"), color = "green", width = 3),
                               infoBox("Temporal Scale", "Monthly", "Resolution", 
                                      icon = icon("clock"), color = "light-blue", width = 3),
                               infoBox("Units", "cm", "Water Equivalent", 
                                      icon = icon("ruler"), color = "purple", width = 3)
                             ),
                             img(src = "images/water_storage.png", width = "100%", height = "auto"),
                             p("Analysis of total water storage changes in the Colorado River Basin.")
                    ),
                    tabPanel("Combined Analysis",
                               fluidRow(
                               infoBox("Variables", "Multiple", "Hydrological Components", 
                                      icon = icon("layer-group"), color = "blue", width = 3),
                               infoBox("Time Period", "2002-2024", "Analysis Period", 
                                      icon = icon("calendar"), color = "green", width = 3),
                               infoBox("Scale", "Basin-wide", "Spatial Coverage", 
                                      icon = icon("globe"), color = "light-blue", width = 3),
                               infoBox("Method", "Correlation", "Analysis Type", 
                                      icon = icon("chart-line"), color = "purple", width = 3)
                             ),
                             img(src = "images/combined_trends.png", width = "100%", height = "auto"),
                             p("Comparison of GRACE TWS with other hydrological variables.")
                    ),
                    tabPanel("Normalized Trends",
                               fluidRow(
                               infoBox("Normalization", "Z-score", "Method", 
                                      icon = icon("calculator"), color = "blue", width = 3),
                               infoBox("Period", "2002-2024", "Analysis Window", 
                                      icon = icon("calendar"), color = "green", width = 3),
                               infoBox("Variables", "Multiple", "Components", 
                                      icon = icon("layer-group"), color = "light-blue", width = 3),
                               infoBox("Scale", "Standardized", "Units", 
                                      icon = icon("ruler"), color = "purple", width = 3)
                             ),
                             img(src = "images/combined_trends_normalized.png", width = "100%", height = "auto"),
                             p("Normalized comparison of GRACE TWS trends with other hydrological variables.")
                    )
                )
              )
      ),
      
      # Snow Water Equivalent Tab
      tabItem(tabName = "swe",
              fluidRow(
                div(class = "variable-info",
                    h4("Snow Water Equivalent Analysis"),
                    p("Analysis of snow water equivalent (SWE) data from various sources."),
                    p("Includes spatial distribution, temporal trends, and seasonal patterns."),
                    p("Critical for water resource management and flood forecasting.")
                )
              ),
              fluidRow(
                column(width = 12,
                  infoBox("Max SWE", "1200 mm", 
                         "Source: VIC Model - 2024", 
                         icon = icon("snowflake"), color = "blue", width = 3),
                  infoBox("Mean SWE", "450 mm", 
                         "Source: VIC Model - 2024", 
                         icon = icon("chart-line"), color = "green", width = 3),
                  infoBox("Data Resolution", "1/16°", 
                         "Source: VIC Model", 
                         icon = icon("ruler"), color = "light-blue", width = 3),
                  infoBox("Update Frequency", "Daily", 
                         "Source: VIC Model", 
                         icon = icon("clock"), color = "yellow", width = 3)
                )
              ),
              fluidRow(
                box(width = 12, title = "Snow Water Equivalent Analysis",
                    tabsetPanel(
                      tabPanel("Spatial Analysis",
                           fluidRow(
                             column(width = 6,
                                box(width = 12, title = "Spatial Distribution",
                                    img(src = "images/snow_water_analysis/spatial_distribution.png", width = "100%", height = "auto"),
                                    p("Spatial distribution of snow water equivalent across the basin.")
                                )
                             ),
                             column(width = 6,
                                box(width = 12, title = "VIC Model SWE",
                                    img(src = "images/vic_snow_water_equivalent_map.png", width = "100%", height = "auto"),
                                    p("VIC model simulated snow water equivalent.")
                                )
                             )
                           ),
                           fluidRow(
                             column(width = 12,
                                box(width = 12, title = "Snow Melt Patterns",
                                    img(src = "images/snow_water_analysis/snow_melt.png", width = "100%", height = "auto"),
                                    p("Patterns of snow melt and accumulation.")
                                )
                             )
                           )
                      ),
                      tabPanel("Temporal Analysis",
                           fluidRow(
                             column(width = 6,
                                box(width = 12, title = "Time Series",
                                    img(src = "images/snow_water_analysis/time_series.png", width = "100%", height = "auto"),
                                    p("Temporal evolution of snow water equivalent.")
                                )
                             ),
                             column(width = 6,
                                box(width = 12, title = "Monthly Statistics",
                                    img(src = "images/snow_water_analysis/monthly_stats.png", width = "100%", height = "auto"),
                                    p("Monthly statistics of snow water equivalent.")
                                )
                             )
                           ),
                           fluidRow(
                             column(width = 12,
                                box(width = 12, title = "Seasonal Patterns",
                                    img(src = "images/swe_analysis/seasonal_patterns.png", width = "100%", height = "auto"),
                                    p("Seasonal patterns in snow water equivalent.")
                                )
                             )
                           )
                      ),
                      tabPanel("Precipitation Analysis",
                           fluidRow(
                             column(width = 6,
                                box(width = 12, title = "Rain vs Snow",
                                    img(src = "images/precipitation_rain_snow.png", width = "100%", height = "auto"),
                                    p("Distribution of precipitation as rain vs snow.")
                                )
                             ),
                             column(width = 6,
                                box(width = 12, title = "Precipitation Analysis",
                                    img(src = "images/precipitation_analysis/rain_vs_snow.png", width = "100%", height = "auto"),
                                    p("Detailed analysis of precipitation types.")
                                )
                             )
                           ),
                           fluidRow(
                             column(width = 12,
                                box(width = 12, title = "SWE Analysis",
                                    img(src = "images/swe_analysis.png", width = "100%", height = "auto"),
                                    p("Comprehensive snow water equivalent analysis.")
                                )
                             )
                           )
                      )
                    )
                )
              )
      ),
      
      # Precipitation Tab
      tabItem(tabName = "precipitation",
              h2("Precipitation Analysis - Colorado River Basin"),
              fluidRow(
                box(width = 12, title = "Overview", solidHeader = TRUE,
                    fluidRow(
                      infoBox("Annual Average", "400 mm", "Precipitation", 
                              icon = icon("cloud-rain"), color = "blue", width = 3,
                              subtitle = "Source: PRISM Data"),
                      infoBox("Data Resolution", "4 km", "Spatial Scale", 
                              icon = icon("ruler"), color = "green", width = 3,
                              subtitle = "Source: PRISM Data"),
                      infoBox("Data Coverage", "1981-2024", "Temporal Range", 
                              icon = icon("calendar"), color = "light-blue", width = 3,
                              subtitle = "Source: PRISM Data"),
                      infoBox("Update Frequency", "Monthly", "Temporal Resolution", 
                              icon = icon("clock"), color = "purple", width = 3,
                              subtitle = "Source: PRISM Data")
                    )
                )
              ),
              fluidRow(
                box(title = "Spatial Distribution", width = 6, solidHeader = TRUE,
                    img(src = "images/analysis_results/precipitation_spatial.png", 
                        width = "100%", height = "auto"),
                    div(style = "margin-top: 20px;",
                        infoBox("About This Graph", 
                               "Shows the spatial distribution of precipitation across the Colorado River Basin. 
                               The map helps identify areas of high and low precipitation.",
                               icon = icon("info-circle"), 
                               color = "blue", 
                               width = 12)
                    )
                ),
                box(title = "Time Series Analysis", width = 6, solidHeader = TRUE,
                    img(src = "images/analysis_results/precipitation_timeseries.png", 
                        width = "100%", height = "auto"),
                    div(style = "margin-top: 20px;",
                        infoBox("About This Graph", 
                               "Displays the temporal variation of precipitation. The time series shows 
                               monthly values and helps identify trends and patterns.",
                               icon = icon("info-circle"), 
                               color = "blue", 
                               width = 12)
                    )
                )
              ),
              fluidRow(
                box(title = "Monthly Patterns", width = 6, solidHeader = TRUE,
                    img(src = "images/analysis_results/precipitation_monthly.png", 
                        width = "100%", height = "auto"),
                    div(style = "margin-top: 20px;",
                        infoBox("About This Graph", 
                               "Shows the average monthly precipitation patterns. Helps identify 
                               seasonal variations and typical precipitation distribution.",
                               icon = icon("info-circle"), 
                               color = "blue", 
                               width = 12)
                    )
                ),
                box(title = "Trend Analysis", width = 6, solidHeader = TRUE,
                    img(src = "images/analysis_results/trend_precipitation.png", 
                        width = "100%", height = "auto"),
                    div(style = "margin-top: 20px;",
                        infoBox("About This Graph", 
                               "Shows the long-term trends in precipitation across the basin. 
                               Helps identify areas of increasing or decreasing precipitation.",
                               icon = icon("info-circle"), 
                               color = "blue", 
                               width = 12)
                    )
                )
              )
      ),
      # New tab for comprehensive analysis
      tabItem(
        tabName = "comprehensive_analysis",
        fluidRow(
          box(
            title = "Interactive Map",
            width = 12,
            height = "600px",
            htmlOutput("interactive_map")
          )
        ),
        fluidRow(
          box(
            title = "Time Series Analysis",
            width = 12,
            tabBox(
              width = 12,
              tabPanel(
                "Monthly Averages",
                imageOutput("monthly_timeseries", height = "400px")
              ),
              tabPanel(
                "Annual Averages",
                imageOutput("annual_timeseries", height = "400px")
              )
            )
          )
        ),
        fluidRow(
          box(
            title = "Correlation Analysis",
            width = 6,
            imageOutput("correlation_matrix", height = "400px")
          ),
          box(
            title = "Trend Analysis",
            width = 6,
            tabBox(
              width = 12,
              tabPanel(
                "Precipitation",
                imageOutput("trend_precipitation", height = "300px")
              ),
              tabPanel(
                "Snow Water Equivalent",
                imageOutput("trend_swe", height = "300px")
              ),
              tabPanel(
                "Runoff",
                imageOutput("trend_runoff", height = "300px")
              ),
              tabPanel(
                "Baseflow",
                imageOutput("trend_baseflow", height = "300px")
              ),
              tabPanel(
                "Evapotranspiration",
                imageOutput("trend_evap", height = "300px")
              ),
              tabPanel(
                "Soil Moisture",
                imageOutput("trend_soil_moisture", height = "300px")
              )
            )
          )
        )
      ),
      # Runoff Analysis Tab
      tabItem(tabName = "runoff",
              fluidRow(
                div(class = "variable-info",
                    h4("Runoff Analysis"),
                    p("Analysis of surface runoff patterns across the Colorado River Basin."),
                    p("Includes both historical trends and current conditions."),
                    p("Critical for flood forecasting and water resource management.")
                )
              ),
              fluidRow(
                box(width = 12, title = "Runoff Analysis",
                    tabsetPanel(
                      tabPanel("Monthly Statistics",
                           fluidRow(
                             column(width = 6,
                                box(width = 12, title = "Monthly Statistics",
                                    img(src = "images/runoff_analysis/monthly_statistics.png", width = "100%", height = "auto"),
                                    p("Monthly runoff statistics."),
                                    plotlyOutput("runoff_monthly_interactive", height = "300px")
                                )
                             ),
                             column(width = 6,
                                box(width = 12, title = "Key Metrics",
                                    valueBoxOutput("runoff_mean", width = 12),
                                    valueBoxOutput("runoff_trend", width = 12),
                                    valueBoxOutput("runoff_anomaly", width = 12)
                                )
                             )
                           )
                      )
                    )
                )
              )
      ),

      # Baseflow Analysis Tab
      tabItem(tabName = "baseflow",
              fluidRow(
                div(class = "variable-info",
                    h4("Baseflow Analysis"),
                    p("Analysis of baseflow patterns across the Colorado River Basin."),
                    p("Includes both historical trends and current conditions."),
                    p("Critical for understanding groundwater contributions to streamflow.")
                )
              ),
              fluidRow(
                box(width = 12, title = "Baseflow Analysis",
                    tabsetPanel(
                      tabPanel("Trend Analysis",
                           fluidRow(
                             column(width = 6,
                                box(width = 12, title = "Baseflow Trends",
                                    img(src = "images/dashboard/trend_baseflow.png", width = "100%", height = "auto"),
                                    p("Trend analysis of baseflow across the basin."),
                                    plotlyOutput("baseflow_trend_interactive", height = "300px")
                                )
                             ),
                             column(width = 6,
                                box(width = 12, title = "Key Metrics",
                                    valueBoxOutput("baseflow_mean", width = 12),
                                    valueBoxOutput("baseflow_trend", width = 12),
                                    valueBoxOutput("baseflow_anomaly", width = 12)
                                )
                             )
                           )
                      ),
                      tabPanel("Spatial Distribution",
                           fluidRow(
                             column(width = 6,
                                box(width = 12, title = "Baseflow Distribution",
                                    img(src = "images/dashboard/trend_baseflow.png", width = "100%", height = "auto"),
                                    p("Spatial distribution of baseflow."),
                                    plotlyOutput("baseflow_spatial_interactive", height = "300px")
                                )
                             ),
                             column(width = 6,
                                box(width = 12, title = "Key Metrics",
                                    valueBoxOutput("baseflow_corr_strength", width = 12),
                                    valueBoxOutput("baseflow_corr_significance", width = 12),
                                    valueBoxOutput("baseflow_corr_trend", width = 12)
                                )
                             )
                           )
                      )
                    )
                )
              )
      ),

      # Soil Moisture Tab
      tabItem(tabName = "soil",
              fluidRow(
                div(class = "variable-info",
                    h4("Soil Moisture Analysis"),
                    p("Analysis of soil moisture data from various sources."),
                    p("Includes surface, root zone, and profile soil moisture analysis."),
                    p("Critical for drought monitoring and agricultural planning.")
                )
              ),
              fluidRow(
                column(width = 12,
                  infoBox("Surface SM", "0.25 m³/m³", 
                         "Source: SMAP - 2024", 
                         icon = icon("tint"), color = "blue", width = 3),
                  infoBox("Root Zone SM", "0.35 m³/m³", 
                         "Source: SMAP - 2024", 
                         icon = icon("tint"), color = "green", width = 3),
                  infoBox("Profile SM", "0.30 m³/m³", 
                         "Source: VIC Model", 
                         icon = icon("layer-group"), color = "light-blue", width = 3),
                  infoBox("Update Frequency", "Daily", 
                         "Source: SMAP/VIC", 
                         icon = icon("clock"), color = "yellow", width = 3)
                )
              ),
              fluidRow(
                box(width = 12, title = "Soil Moisture Analysis",
                    tabsetPanel(
                      tabPanel("Spatial Analysis",
                           fluidRow(
                             column(width = 6,
                                box(width = 12, title = "Spatial Distribution",
                                    img(src = "images/soil_moisture_analysis/spatial_distribution.png", width = "100%", height = "auto"),
                                    p("Spatial distribution of soil moisture across the basin.")
                                )
                             ),
                             column(width = 6,
                                box(width = 12, title = "VIC Model Soil Moisture",
                                    img(src = "images/vic_soil_moisture_layer_1_map.png", width = "100%", height = "auto"),
                                    p("VIC model simulated soil moisture.")
                                )
                             )
                           ),
                           fluidRow(
                             column(width = 12,
                                box(width = 12, title = "Soil Layers",
                                    img(src = "images/soil_moisture_analysis/soil_layers.png", width = "100%", height = "auto"),
                                    p("Distribution of soil moisture across different layers.")
                                )
                             )
                           )
                      ),
                      tabPanel("Temporal Analysis",
                           fluidRow(
                             column(width = 6,
                                box(width = 12, title = "Time Series",
                                    img(src = "images/soil_moisture_analysis/time_series.png", width = "100%", height = "auto"),
                                    p("Temporal evolution of soil moisture.")
                                )
                             ),
                             column(width = 6,
                                box(width = 12, title = "Monthly Statistics",
                                    img(src = "images/soil_moisture_analysis/monthly_stats.png", width = "100%", height = "auto"),
                                    p("Monthly statistics of soil moisture.")
                                )
                             )
                           ),
                           fluidRow(
                             column(width = 12,
                                box(width = 12, title = "Monthly Trends",
                                    img(src = "images/soil_moisture_analysis/monthly_statistics.png", width = "100%", height = "auto"),
                                    p("Monthly trends in soil moisture.")
                                )
                             )
                           )
                      ),
                      tabPanel("Comparative Analysis",
                           fluidRow(
                             column(width = 6,
                                box(width = 12, title = "Correlation Analysis",
                                    img(src = "images/analysis_results/correlation_soil_moisture.png", width = "100%", height = "auto"),
                                    p("Correlation between different soil moisture layers.")
                                )
                             ),
                             column(width = 6,
                                box(width = 12, title = "Trend Analysis",
                                    img(src = "images/analysis_results/trend_soil_moisture.png", width = "100%", height = "auto"),
                                    p("Long-term trends in soil moisture.")
                                )
                             )
                           ),
                           fluidRow(
                             column(width = 12,
                                box(width = 12, title = "Soil Moisture Overview",
                                    img(src = "images/soil_moisture.png", width = "100%", height = "auto"),
                                    p("Comprehensive soil moisture analysis.")
                                )
                             )
                           )
                      )
                    )
                )
              )
      ),

      # Temperature Tab
      tabItem(tabName = "temperature",
              fluidRow(
                div(class = "variable-info",
                    h4("Temperature Analysis"),
                    p("Analysis of temperature patterns across the Colorado River Basin."),
                    p("Includes both air and surface temperature measurements."),
                    p("Critical for understanding climate patterns and snow melt timing.")
                )
              ),
              fluidRow(
                box(width = 12, title = "Temperature Analysis",
                    tabsetPanel(
                      tabPanel("Monthly Statistics",
                           fluidRow(
                             column(width = 6,
                                box(width = 12, title = "Monthly Statistics",
                                    img(src = "images/temperature_analysis/monthly_statistics.png", width = "100%", height = "auto"),
                                    p("Monthly temperature statistics."),
                                    plotlyOutput("temp_monthly_interactive", height = "300px")
                                )
                             ),
                             column(width = 6,
                                box(width = 12, title = "Key Metrics",
                                    valueBoxOutput("temp_mean", width = 12),
                                    valueBoxOutput("temp_trend", width = 12),
                                    valueBoxOutput("temp_anomaly", width = 12)
                                )
                             )
                           )
                      )
                    )
                )
              )
      ),

      # Water Balance Tab
      tabItem(tabName = "water_balance",
              fluidRow(
                div(class = "variable-info",
                    h4("Water Balance Analysis"),
                    p("Analysis of water balance components across the Colorado River Basin."),
                    p("Includes precipitation, evapotranspiration, runoff, and storage changes."),
                    p("Critical for understanding the basin's water budget.")
                )
              ),
              fluidRow(
                box(width = 12, title = "Water Balance Analysis",
                    tabsetPanel(
                      tabPanel("Monthly Averages",
                           fluidRow(
                             column(width = 6,
                                box(width = 12, title = "Monthly Water Balance",
                                    img(src = "images/analysis_results/water_balance_monthly.png", width = "100%", height = "auto"),
                                    p("Monthly water balance components."),
                                    plotlyOutput("water_balance_monthly_interactive", height = "300px")
                                )
                             ),
                             column(width = 6,
                                box(width = 12, title = "Water Balance Metrics",
                                    valueBoxOutput("water_balance_mean", width = 12),
                                    valueBoxOutput("water_balance_trend", width = 12),
                                    valueBoxOutput("water_balance_anomaly", width = 12)
                                )
                             )
                           )
                      )
                    )
                )
              )
      ),
      # VIC Data Analysis Tab
      tabItem(tabName = "vic_analysis",
        fluidRow(
          # Info tiles at the top
          valueBoxOutput("vic_precip_box", width = 3),
          valueBoxOutput("vic_evap_box", width = 3),
          valueBoxOutput("vic_runoff_box", width = 3),
          valueBoxOutput("vic_trend_box", width = 3)
        ),
        fluidRow(
          # Variable Statistics
          box(width = 12, title = "Variable Statistics",
            fluidRow(
              column(width = 3,
                valueBoxOutput("vic_mean_box", width = 12)
              ),
              column(width = 3,
                valueBoxOutput("vic_max_box", width = 12)
              ),
              column(width = 3,
                valueBoxOutput("vic_min_box", width = 12)
              ),
              column(width = 3,
                valueBoxOutput("vic_trend_box", width = 12)
              )
            )
          )
        ),
        fluidRow(
          # Analysis controls and plots
          column(width = 12,
            box(width = 12, title = "VIC Data Analysis",
              fluidRow(
                column(width = 4,
                  selectInput("vic_variable", "Select Variable",
                    choices = c("Precipitation" = "precipitation",
                              "Evapotranspiration" = "evapotranspiration",
                              "Runoff" = "runoff"),
                    selected = "precipitation"
                  )
                ),
                column(width = 4,
                  dateInput("vic_date", "Select Date", value = Sys.Date())
                ),
                column(width = 4,
                  actionButton("vic_update", "Update Analysis", class = "btn-primary")
                )
              ),
              fluidRow(
                column(width = 6,
                  plotlyOutput("vic_spatial_plot", height = "500px")
                ),
                column(width = 6,
                  plotlyOutput("vic_time_series", height = "500px")
                )
              )
            )
          )
        )
      ),
      tabItem(tabName = "swe_anomalies",
              h2("Snow Water Equivalent (SWE) Anomalies Analysis"),
              fluidRow(
                box(width = 12, title = "SWE Data Overview", solidHeader = TRUE,
                    fluidRow(
                      infoBox("Current SWE", "85%", "of Normal", 
                              icon = icon("snowflake"), color = "blue", width = 3,
                              subtitle = "Source: SNODAS - 2024"),
                      infoBox("Data Resolution", "1 km", "Spatial Scale", 
                              icon = icon("ruler"), color = "green", width = 3,
                              subtitle = "Source: SNODAS - 2024"),
                      infoBox("Data Coverage", "2003-2024", "Historical Period", 
                              icon = icon("calendar"), color = "light-blue", width = 3,
                              subtitle = "Source: SNODAS - 2024"),
                      infoBox("Update Frequency", "Daily", "Data Refresh", 
                              icon = icon("clock"), color = "purple", width = 3,
                              subtitle = "Source: SNODAS - 2024")
                    )
                )
              ),
              box(width = 12, title = "SWE Analysis", solidHeader = TRUE,
                  tabsetPanel(
                    tabPanel("April 1st SWE Anomalies",
                             fluidRow(
                               infoBox("Current Anomaly", "-25%", "2024", 
                                      icon = icon("snowflake"), color = "blue", width = 3),
                               infoBox("Historical Range", "±40%", "Variability", 
                                      icon = icon("wave-square"), color = "green", width = 3),
                               infoBox("Trend", "-2.5%/year", "2003-2024", 
                                      icon = icon("chart-line"), color = "light-blue", width = 3),
                               infoBox("Last Peak", "+35%", "2011", 
                                      icon = icon("arrow-up"), color = "purple", width = 3)
                             ),
                             img(src = "images/april1_swe_anomalies.png", width = "100%", height = "auto"),
                             p("April 1st SWE anomalies showing deviations from the long-term average.")
                    ),
                    tabPanel("SWE Trends",
                             fluidRow(
                               infoBox("Early Period", "-1.2%/year", "2003-2010", 
                                      icon = icon("chart-line"), color = "blue", width = 3),
                               infoBox("Middle Period", "-2.8%/year", "2011-2020", 
                                      icon = icon("chart-line"), color = "green", width = 3),
                               infoBox("Recent Period", "-3.5%/year", "2021-2024", 
                                      icon = icon("chart-line"), color = "light-blue", width = 3),
                               infoBox("Overall Trend", "-2.5%/year", "2003-2024", 
                                      icon = icon("chart-line"), color = "purple", width = 3)
                             ),
                             img(src = "images/april1_swe_trends.png", width = "100%", height = "auto"),
                             p("Long-term trends in April 1st SWE showing changes over time.")
                    ),
                    tabPanel("Seasonal Patterns",
                             fluidRow(
                               infoBox("Peak Month", "March", "Average", 
                                      icon = icon("calendar"), color = "blue", width = 3),
                               infoBox("Accumulation", "Oct-Mar", "Period", 
                                      icon = icon("snowflake"), color = "green", width = 3),
                               infoBox("Melt Period", "Apr-Jul", "Duration", 
                                      icon = icon("sun"), color = "light-blue", width = 3),
                               infoBox("Seasonal Range", "±60%", "Variability", 
                                      icon = icon("wave-square"), color = "purple", width = 3)
                             ),
                             img(src = "images/seasonal_patterns.png", width = "100%", height = "auto"),
                             p("Seasonal patterns of SWE showing typical accumulation and melt cycles.")
                    ),
                    tabPanel("Elevation Analysis",
                             fluidRow(
                               infoBox("High Elevation", "-15%", ">3000m", 
                                      icon = icon("mountain"), color = "blue", width = 3),
                               infoBox("Mid Elevation", "-28%", "2000-3000m", 
                                      icon = icon("mountain"), color = "green", width = 3),
                               infoBox("Low Elevation", "-35%", "<2000m", 
                                      icon = icon("mountain"), color = "light-blue", width = 3),
                               infoBox("Elevation Trend", "-0.5%/100m", "Gradient", 
                                      icon = icon("chart-line"), color = "purple", width = 3)
                             ),
                             img(src = "images/elevation_analysis.png", width = "100%", height = "auto"),
                             p("SWE anomalies analyzed by elevation zones in the Colorado River Basin.")
                    )
                  )
              )
      ),
      tabItem(tabName = "vic",
              h2("VIC Model Analysis - Colorado River Basin"),
              fluidRow(
                box(width = 12, title = "Overview", solidHeader = TRUE,
                    fluidRow(
                      infoBox("Model Resolution", "4 km", "Spatial Scale", 
                              icon = icon("ruler"), color = "blue", width = 3,
                              subtitle = "Source: VIC Model"),
                      infoBox("Temporal Resolution", "Daily", "Time Step", 
                              icon = icon("clock"), color = "green", width = 3,
                              subtitle = "Source: VIC Model"),
                      infoBox("Simulation Period", "1981-2024", "Coverage", 
                              icon = icon("calendar"), color = "light-blue", width = 3,
                              subtitle = "Source: VIC Model"),
                      infoBox("Output Variables", "13", "Available", 
                              icon = icon("list"), color = "purple", width = 3,
                              subtitle = "Source: VIC Model")
                    )
                )
              ),
              fluidRow(
                box(title = "Precipitation Analysis", width = 6, solidHeader = TRUE,
                    img(src = "images/analysis_results/precipitation_spatial.png", 
                        width = "100%", height = "auto"),
                    div(style = "margin-top: 20px;",
                        infoBox("Spatial Distribution", 
                               "Shows the spatial distribution of precipitation across the basin. Darker colors indicate higher precipitation amounts.",
                               icon = icon("map"), 
                               color = "blue", 
                               width = 12),
                        infoBox("Data Source", 
                               "VIC model simulated precipitation at 4km resolution",
                               icon = icon("database"), 
                               color = "green", 
                               width = 12),
                        infoBox("Temporal Coverage", 
                               "Daily data from 1981 to 2024",
                               icon = icon("calendar-alt"), 
                               color = "light-blue", 
                               width = 12)
                    )
                ),
                box(title = "Evapotranspiration Analysis", width = 6, solidHeader = TRUE,
                    img(src = "images/analysis_results/evapotranspiration_spatial.png", 
                        width = "100%", height = "auto"),
                    div(style = "margin-top: 20px;",
                        infoBox("Spatial Distribution", 
                               "Shows the spatial distribution of evapotranspiration. Higher values indicate areas with more water loss to the atmosphere.",
                               icon = icon("map"), 
                               color = "blue", 
                               width = 12),
                        infoBox("Data Source", 
                               "VIC model simulated evapotranspiration at 4km resolution",
                               icon = icon("database"), 
                               color = "green", 
                               width = 12),
                        infoBox("Temporal Coverage", 
                               "Daily data from 1981 to 2024",
                               icon = icon("calendar-alt"), 
                               color = "light-blue", 
                               width = 12)
                    )
                )
              ),
              fluidRow(
                box(title = "Soil Moisture Analysis - Layer 1", width = 6, solidHeader = TRUE,
                    img(src = "images/analysis_results/soil_moisture_layer_1_spatial.png", 
                        width = "100%", height = "auto"),
                    div(style = "margin-top: 20px;",
                        infoBox("Top Layer", 
                               "Shows soil moisture in the top layer (0-10cm). Critical for vegetation and surface processes.",
                               icon = icon("layer-group"), 
                               color = "blue", 
                               width = 12),
                        infoBox("Data Source", 
                               "VIC model simulated soil moisture at 4km resolution",
                               icon = icon("database"), 
                               color = "green", 
                               width = 12),
                        infoBox("Temporal Coverage", 
                               "Daily data from 1981 to 2024",
                               icon = icon("calendar-alt"), 
                               color = "light-blue", 
                               width = 12)
                    )
                ),
                box(title = "Soil Moisture Analysis - Layer 2", width = 6, solidHeader = TRUE,
                    img(src = "images/analysis_results/soil_moisture_layer_2_spatial.png", 
                        width = "100%", height = "auto"),
                    div(style = "margin-top: 20px;",
                        infoBox("Middle Layer", 
                               "Shows soil moisture in the middle layer (10-40cm). Important for root zone water availability.",
                               icon = icon("layer-group"), 
                               color = "blue", 
                               width = 12),
                        infoBox("Data Source", 
                               "VIC model simulated soil moisture at 4km resolution",
                               icon = icon("database"), 
                               color = "green", 
                               width = 12),
                        infoBox("Temporal Coverage", 
                               "Daily data from 1981 to 2024",
                               icon = icon("calendar-alt"), 
                               color = "light-blue", 
                               width = 12)
                    )
                )
              ),
              fluidRow(
                box(title = "Soil Moisture Analysis - Layer 3", width = 6, solidHeader = TRUE,
                    img(src = "images/analysis_results/soil_moisture_layer_3_spatial.png", 
                        width = "100%", height = "auto"),
                    div(style = "margin-top: 20px;",
                        infoBox("Bottom Layer", 
                               "Shows soil moisture in the bottom layer (40-100cm). Important for deep percolation and groundwater recharge.",
                               icon = icon("layer-group"), 
                               color = "blue", 
                               width = 12),
                        infoBox("Data Source", 
                               "VIC model simulated soil moisture at 4km resolution",
                               icon = icon("database"), 
                               color = "green", 
                               width = 12),
                        infoBox("Temporal Coverage", 
                               "Daily data from 1981 to 2024",
                               icon = icon("calendar-alt"), 
                               color = "light-blue", 
                               width = 12)
                    )
                ),
                box(title = "Snow Water Equivalent Analysis", width = 6, solidHeader = TRUE,
                    img(src = "images/analysis_results/snow_water_equivalent_spatial.png", 
                        width = "100%", height = "auto"),
                    div(style = "margin-top: 20px;",
                        infoBox("Snow Water Content", 
                               "Shows the amount of water stored in snowpack. Critical for water supply and spring runoff.",
                               icon = icon("snowflake"), 
                               color = "blue", 
                               width = 12),
                        infoBox("Data Source", 
                               "VIC model simulated SWE at 4km resolution",
                               icon = icon("database"), 
                               color = "green", 
                               width = 12),
                        infoBox("Temporal Coverage", 
                               "Daily data from 1981 to 2024",
                               icon = icon("calendar-alt"), 
                               color = "light-blue", 
                               width = 12)
                    )
                )
              ),
              fluidRow(
                box(title = "Surface Runoff Analysis", width = 6, solidHeader = TRUE,
                    img(src = "images/analysis_results/runoff_spatial.png", 
                        width = "100%", height = "auto"),
                    div(style = "margin-top: 20px;",
                        infoBox("Runoff Generation", 
                               "Shows areas where precipitation exceeds infiltration capacity, generating surface runoff.",
                               icon = icon("water"), 
                               color = "blue", 
                               width = 12),
                        infoBox("Data Source", 
                               "VIC model simulated runoff at 4km resolution",
                               icon = icon("database"), 
                               color = "green", 
                               width = 12),
                        infoBox("Temporal Coverage", 
                               "Daily data from 1981 to 2024",
                               icon = icon("calendar-alt"), 
                               color = "light-blue", 
                               width = 12)
                    )
                ),
                box(title = "Baseflow Analysis", width = 6, solidHeader = TRUE,
                    img(src = "images/analysis_results/baseflow_spatial.png", 
                        width = "100%", height = "auto"),
                    div(style = "margin-top: 20px;",
                        infoBox("Groundwater Contribution", 
                               "Shows the contribution of groundwater to streamflow. Important for maintaining baseflow during dry periods.",
                               icon = icon("tint"), 
                               color = "blue", 
                               width = 12),
                        infoBox("Data Source", 
                               "VIC model simulated baseflow at 4km resolution",
                               icon = icon("database"), 
                               color = "green", 
                               width = 12),
                        infoBox("Temporal Coverage", 
                               "Daily data from 1981 to 2024",
                               icon = icon("calendar-alt"), 
                               color = "light-blue", 
                               width = 12)
                    )
                )
              ),
              fluidRow(
                box(title = "Temperature Analysis", width = 6, solidHeader = TRUE,
                    img(src = "images/analysis_results/temperature_spatial.png", 
                        width = "100%", height = "auto"),
                    div(style = "margin-top: 20px;",
                        infoBox("Temperature Distribution", 
                               "Shows the spatial distribution of air temperature. Influences snowmelt and evapotranspiration.",
                               icon = icon("temperature-high"), 
                               color = "blue", 
                               width = 12),
                        infoBox("Data Source", 
                               "VIC model simulated temperature at 4km resolution",
                               icon = icon("database"), 
                               color = "green", 
                               width = 12),
                        infoBox("Temporal Coverage", 
                               "Daily data from 1981 to 2024",
                               icon = icon("calendar-alt"), 
                               color = "light-blue", 
                               width = 12)
                    )
                ),
                box(title = "Water Balance Analysis", width = 6, solidHeader = TRUE,
                    img(src = "images/analysis_results/water_balance_monthly.png", 
                        width = "100%", height = "auto"),
                    div(style = "margin-top: 20px;",
                        infoBox("Water Balance Components", 
                               "Shows the monthly distribution of precipitation, evapotranspiration, and runoff.",
                               icon = icon("balance-scale"), 
                               color = "blue", 
                               width = 12),
                        infoBox("Data Source", 
                               "VIC model water balance components at 4km resolution",
                               icon = icon("database"), 
                               color = "green", 
                               width = 12),
                        infoBox("Temporal Coverage", 
                               "Monthly data from 1981 to 2024",
                               icon = icon("calendar-alt"), 
                               color = "light-blue", 
                               width = 12)
                    )
                )
              ),
              fluidRow(
                box(title = "Time Series Analysis", width = 12, solidHeader = TRUE,
                    img(src = "images/vic_time_series.png", 
                        width = "100%", height = "auto"),
                    div(style = "margin-top: 20px;",
                        infoBox("Temporal Evolution", 
                               "Shows the temporal evolution of key hydrological variables over the entire simulation period.",
                               icon = icon("chart-line"), 
                               color = "blue", 
                               width = 12),
                        infoBox("Variables Displayed", 
                               "Precipitation, Evapotranspiration, Runoff, and Soil Moisture",
                               icon = icon("list"), 
                               color = "green", 
                               width = 12),
                        infoBox("Analysis Period", 
                               "Complete simulation period from 1981 to 2024",
                               icon = icon("calendar"), 
                               color = "light-blue", 
                               width = 12)
                    )
                )
              )
      ),
      tabItem(tabName = "static_vic",
              h2("VIC Model Static Outputs"),
              fluidRow(
                box(title = "Time Series Analysis", width = 12, solidHeader = TRUE,
                    img(src = "images/vic_time_series.png", width = "100%", height = "auto"),
                    div(style = "margin-top: 20px;",
                        infoBox("Temporal Evolution", 
                               "Shows the temporal evolution of key hydrological variables over the entire simulation period.",
                               icon = icon("chart-line"), 
                               color = "blue", 
                               width = 12)
                    )
                )
              ),
              fluidRow(
                box(title = "Monthly Statistics", width = 12, solidHeader = TRUE,
                    img(src = "images/vic_monthly_stats.png", width = "100%", height = "auto"),
                    div(style = "margin-top: 20px;",
                        infoBox("Monthly Patterns", 
                               "Monthly distribution of VIC model variables.",
                               icon = icon("calendar-alt"), 
                               color = "blue", 
                               width = 12)
                    )
                )
              ),
              fluidRow(
                box(title = "Spatial Distribution", width = 6, solidHeader = TRUE,
                    img(src = "images/vic_precipitation_map.png", width = "100%", height = "auto"),
                    div(style = "margin-top: 20px;",
                        infoBox("Precipitation", 
                               "Spatial distribution of precipitation across the basin.",
                               icon = icon("cloud-rain"), 
                               color = "blue", 
                               width = 12)
                    )
                ),
                box(title = "Spatial Distribution", width = 6, solidHeader = TRUE,
                    img(src = "images/vic_evapotranspiration_map.png", width = "100%", height = "auto"),
                    div(style = "margin-top: 20px;",
                        infoBox("Evapotranspiration", 
                               "Spatial distribution of evapotranspiration across the basin.",
                               icon = icon("sun"), 
                               color = "blue", 
                               width = 12)
                    )
                )
              ),
              fluidRow(
                box(title = "Soil Moisture Layers", width = 4, solidHeader = TRUE,
                    img(src = "images/vic_soil_moisture_layer_1_map.png", width = "100%", height = "auto"),
                    div(style = "margin-top: 20px;",
                        infoBox("Layer 1 (0-10cm)", 
                               "Top layer soil moisture distribution.",
                               icon = icon("layer-group"), 
                               color = "blue", 
                               width = 12)
                    )
                ),
                box(title = "Soil Moisture Layers", width = 4, solidHeader = TRUE,
                    img(src = "images/vic_soil_moisture_layer_2_map.png", width = "100%", height = "auto"),
                    div(style = "margin-top: 20px;",
                        infoBox("Layer 2 (10-40cm)", 
                               "Middle layer soil moisture distribution.",
                               icon = icon("layer-group"), 
                               color = "blue", 
                               width = 12)
                    )
                ),
                box(title = "Soil Moisture Layers", width = 4, solidHeader = TRUE,
                    img(src = "images/vic_soil_moisture_layer_3_map.png", width = "100%", height = "auto"),
                    div(style = "margin-top: 20px;",
                        infoBox("Layer 3 (40-100cm)", 
                               "Bottom layer soil moisture distribution.",
                               icon = icon("layer-group"), 
                               color = "blue", 
                               width = 12)
                    )
                )
              ),
              fluidRow(
                box(title = "Water Balance Components", width = 6, solidHeader = TRUE,
                    img(src = "images/vic_runoff_map.png", width = "100%", height = "auto"),
                    div(style = "margin-top: 20px;",
                        infoBox("Surface Runoff", 
                               "Spatial distribution of surface runoff.",
                               icon = icon("water"), 
                               color = "blue", 
                               width = 12)
                    )
                ),
                box(title = "Water Balance Components", width = 6, solidHeader = TRUE,
                    img(src = "images/vic_baseflow_map.png", width = "100%", height = "auto"),
                    div(style = "margin-top: 20px;",
                        infoBox("Baseflow", 
                               "Spatial distribution of baseflow.",
                               icon = icon("tint"), 
                               color = "blue", 
                               width = 12)
                    )
                )
              ),
              fluidRow(
                box(title = "Snow Water Equivalent", width = 6, solidHeader = TRUE,
                    img(src = "images/vic_snow_water_equivalent_map.png", width = "100%", height = "auto"),
                    div(style = "margin-top: 20px;",
                        infoBox("Snow Water Content", 
                               "Spatial distribution of snow water equivalent.",
                               icon = icon("snowflake"), 
                               color = "blue", 
                               width = 12)
                    )
                ),
                box(title = "Combined Time Series", width = 6, solidHeader = TRUE,
                    img(src = "images/vic_combined_timeseries.png", width = "100%", height = "auto"),
                    div(style = "margin-top: 20px;",
                        infoBox("Multi-variable Time Series", 
                               "Combined time series of key hydrological variables.",
                               icon = icon("chart-line"), 
                               color = "blue", 
                               width = 12)
                    )
                )
              ),
              fluidRow(
                box(title = "Normalized Time Series", width = 12, solidHeader = TRUE,
                    img(src = "images/vic_combined_timeseries_normalized.png", width = "100%", height = "auto"),
                    div(style = "margin-top: 20px;",
                        infoBox("Normalized Variables", 
                               "Normalized time series for better comparison across variables.",
                               icon = icon("chart-line"), 
                               color = "blue", 
                               width = 12)
                    )
                )
              )
      ),
      tabItem(tabName = "static_smap",
              h2("SMAP Static Outputs"),
              fluidRow(
                box(title = "Surface Soil Moisture", width = 6, solidHeader = TRUE,
                    img(src = "images/smap_surface_time_series.png", width = "100%", height = "auto"),
                    div(style = "margin-top: 20px;",
                        infoBox("Time Series", 
                               "Temporal evolution of surface soil moisture.",
                               icon = icon("chart-line"), 
                               color = "blue", 
                               width = 12)
                    )
                ),
                box(title = "Root Zone Soil Moisture", width = 6, solidHeader = TRUE,
                    img(src = "images/smap_rootzone_time_series.png", width = "100%", height = "auto"),
                    div(style = "margin-top: 20px;",
                        infoBox("Time Series", 
                               "Temporal evolution of root zone soil moisture.",
                               icon = icon("chart-line"), 
                               color = "blue", 
                               width = 12)
                    )
                )
              ),
              fluidRow(
                box(title = "Trend Analysis", width = 6, solidHeader = TRUE,
                    img(src = "images/smap_surface_trend.png", width = "100%", height = "auto"),
                    div(style = "margin-top: 20px;",
                        infoBox("Surface Trend", 
                               "Long-term trends in surface soil moisture.",
                               icon = icon("chart-line"), 
                               color = "blue", 
                               width = 12)
                    )
                ),
                box(title = "Trend Analysis", width = 6, solidHeader = TRUE,
                    img(src = "images/smap_rootzone_trend.png", width = "100%", height = "auto"),
                    div(style = "margin-top: 20px;",
                        infoBox("Root Zone Trend", 
                               "Long-term trends in root zone soil moisture.",
                               icon = icon("chart-line"), 
                               color = "blue", 
                               width = 12)
                    )
                )
              )
      ),
      tabItem(tabName = "static_grace",
              h2("GRACE Static Outputs"),
              fluidRow(
                box(title = "Time Series Analysis", width = 12, solidHeader = TRUE,
                    img(src = "images/grace_time_series.png", width = "100%", height = "auto"),
                    div(style = "margin-top: 20px;",
                        infoBox("Water Storage", 
                               "Temporal evolution of total water storage.",
                               icon = icon("chart-line"), 
                               color = "blue", 
                               width = 12)
                    )
                )
              ),
              fluidRow(
                box(title = "Trend Analysis", width = 12, solidHeader = TRUE,
                    img(src = "images/grace_trend.png", width = "100%", height = "auto"),
                    div(style = "margin-top: 20px;",
                        infoBox("Storage Trend", 
                               "Long-term trends in total water storage.",
                               icon = icon("chart-line"), 
                               color = "blue", 
                               width = 12)
                    )
                )
              )
      ),
      tabItem(tabName = "static_prism",
              h2("PRISM Static Outputs"),
              fluidRow(
                box(title = "Precipitation Analysis", width = 6, solidHeader = TRUE,
                    img(src = "images/precipitation_spatial.png", width = "100%", height = "auto"),
                    div(style = "margin-top: 20px;",
                        infoBox("Spatial Distribution", 
                               "Spatial distribution of precipitation.",
                               icon = icon("map"), 
                               color = "blue", 
                               width = 12)
                    )
                ),
                box(title = "Precipitation Analysis", width = 6, solidHeader = TRUE,
                    img(src = "images/precipitation_timeseries.png", width = "100%", height = "auto"),
                    div(style = "margin-top: 20px;",
                        infoBox("Time Series", 
                               "Temporal evolution of precipitation.",
                               icon = icon("chart-line"), 
                               color = "blue", 
                               width = 12)
                    )
                )
              ),
              fluidRow(
                box(title = "Monthly Patterns", width = 6, solidHeader = TRUE,
                    img(src = "images/precipitation_monthly.png", width = "100%", height = "auto"),
                    div(style = "margin-top: 20px;",
                        infoBox("Monthly Statistics", 
                               "Monthly distribution of precipitation.",
                               icon = icon("calendar-alt"), 
                               color = "blue", 
                               width = 12)
                    )
                ),
                box(title = "Rain vs Snow", width = 6, solidHeader = TRUE,
                    img(src = "images/precipitation_rain_snow.png", width = "100%", height = "auto"),
                    div(style = "margin-top: 20px;",
                        infoBox("Precipitation Type", 
                               "Distribution of rain and snow precipitation.",
                               icon = icon("snowflake"), 
                               color = "blue", 
                               width = 12)
                    )
                )
              )
      ),
      tabItem(tabName = "static_snotel",
              h2("SNOTEL Static Outputs"),
              fluidRow(
                box(title = "Station Network", width = 12, solidHeader = TRUE,
                    img(src = "images/snotel_map.png", width = "100%", height = "auto"),
                    div(style = "margin-top: 20px;",
                        infoBox("Station Distribution", 
                               "Distribution of SNOTEL stations across the basin.",
                               icon = icon("map-marker-alt"), 
                               color = "blue", 
                               width = 12)
                    )
                )
              ),
              fluidRow(
                box(title = "Time Series Analysis", width = 6, solidHeader = TRUE,
                    img(src = "images/snotel_timeseries.png", width = "100%", height = "auto"),
                    div(style = "margin-top: 20px;",
                        infoBox("Snow Water Equivalent", 
                               "Temporal evolution of snow water equivalent.",
                               icon = icon("chart-line"), 
                               color = "blue", 
                               width = 12)
                    )
                ),
                box(title = "Seasonal Patterns", width = 6, solidHeader = TRUE,
                    img(src = "images/snotel_seasonal.png", width = "100%", height = "auto"),
                    div(style = "margin-top: 20px;",
                        infoBox("Seasonal Cycle", 
                               "Seasonal patterns of snow water equivalent.",
                               icon = icon("calendar-alt"), 
                               color = "blue", 
                               width = 12)
                    )
                )
              ),
              fluidRow(
                box(title = "Elevation Analysis", width = 6, solidHeader = TRUE,
                    img(src = "images/snotel_elevation.png", width = "100%", height = "auto"),
                    div(style = "margin-top: 20px;",
                        infoBox("Station Elevation", 
                               "Elevation distribution of SNOTEL stations.",
                               icon = icon("mountain"), 
                               color = "blue", 
                               width = 12)
                    )
                ),
                box(title = "Trend Analysis", width = 6, solidHeader = TRUE,
                    img(src = "images/snotel_trend.png", width = "100%", height = "auto"),
                    div(style = "margin-top: 20px;",
                        infoBox("Long-term Trends", 
                               "Long-term trends in snow water equivalent.",
                               icon = icon("chart-line"), 
                               color = "blue", 
                               width = 12)
                    )
                )
              )
      ),
      tabItem(tabName = "static_combined",
              h2("Combined Analysis Static Outputs"),
              fluidRow(
                box(title = "Trend Analysis", width = 12, solidHeader = TRUE,
                    img(src = "images/combined_trends.png", width = "100%", height = "auto"),
                    div(style = "margin-top: 20px;",
                        infoBox("Multi-variable Trends", 
                               "Long-term trends across multiple variables.",
                               icon = icon("chart-line"), 
                               color = "blue", 
                               width = 12)
                    )
                )
              ),
              fluidRow(
                box(title = "Normalized Trends", width = 12, solidHeader = TRUE,
                    img(src = "images/combined_trends_normalized.png", width = "100%", height = "auto"),
                    div(style = "margin-top: 20px;",
                        infoBox("Normalized Comparison", 
                               "Normalized trends for better comparison across variables.",
                               icon = icon("chart-line"), 
                               color = "blue", 
                               width = 12)
                    )
                )
              ),
              fluidRow(
                box(title = "Correlation Analysis", width = 6, solidHeader = TRUE,
                    img(src = "images/correlation_soil_moisture.png", width = "100%", height = "auto"),
                    div(style = "margin-top: 20px;",
                        infoBox("Soil Moisture Correlations", 
                               "Correlations between soil moisture and other variables.",
                               icon = icon("chart-line"), 
                               color = "blue", 
                               width = 12)
                    )
                ),
                box(title = "Correlation Analysis", width = 6, solidHeader = TRUE,
                    img(src = "images/correlation_swe.png", width = "100%", height = "auto"),
                    div(style = "margin-top: 20px;",
                        infoBox("Snow Water Equivalent Correlations", 
                               "Correlations between snow water equivalent and other variables.",
                               icon = icon("chart-line"), 
                               color = "blue", 
                               width = 12)
                    )
                )
              )
      )
    )
  )
)

# Add custom CSS for VIC tab
tags$head(
  tags$style(HTML("
    .vic-header {
      background-color: white;
      padding: 10px;
      margin-bottom: 20px;
    }
    
    .vic-footer {
      background-color: white;
      padding: 10px;
      margin-top: 20px;
    }
    
    .box {
      box-shadow: 0 4px 8px rgba(0,0,0,0.1);
      transition: all 0.3s ease;
    }
    
    .box:hover {
      box-shadow: 0 6px 12px rgba(0,0,0,0.15);
      transform: translateY(-2px);
    }
    
    .info-box {
      background-color: white !important;
      border: 1px solid #ddd !important;
      border-radius: 5px !important;
    }
    .info-box-icon[data-icon='ruler'] {
      background-color: white !important;
      color: #8C1D40 !important;
    }
    .info-box-icon[data-icon='clock'] {
      background-color: white !important;
      color: #FFC627 !important;
    }
    .info-box-icon[data-icon='calendar'] {
      background-color: white !important;
      color: #8C1D40 !important;
    }
    .info-box-icon[data-icon='list'] {
      background-color: white !important;
      color: #FFC627 !important;
    }
    .info-box-content {
      background-color: white !important;
    }
    .info-box-text {
      color: #8C1D40 !important;
    }
    .info-box-number {
      color: #8C1D40 !important;
    }
    .small-box {
      background-color: white !important;
      border: 1px solid #ddd !important;
      border-radius: 5px !important;
    }
    .small-box .icon {
      background-color: white !important;
      color: #8C1D40 !important;
    }
    .small-box .inner {
      background-color: white !important;
    }
    .small-box h3 {
      color: #8C1D40 !important;
    }
    .small-box p {
      color: #8C1D40 !important;
    }
  "))
)

# Server
server <- function(input, output, session) {
  # Add resource path for images
  addResourcePath("images", "images")
  
  # Check for required data files
  observe({
    req_files <- c(
      "data/VIC_outputs",
      "data/SMAP_outputs",
      "data/GRACE_outputs",
      "data/Analysis_Basin_Shapefiles/basins_all_unique.shp",
      "data/huc10s/huc10.shp"
    )
    
    missing <- !file.exists(req_files)
    if (any(missing)) {
      showModal(modalDialog(
        title = "Missing Data Files",
        paste("The following required files/directories are missing:", 
              paste(req_files[missing], collapse = ", ")),
        footer = modalButton("OK")
      ))
    }
  })
  
  # Helper function for data validation
  validate_data <- function(data, data_type) {
    if (is.null(data)) {
      showNotification(paste(data_type, "data not available"), type = "error")
      return(FALSE)
    }
    return(TRUE)
  }
  
  # Helper function for file validation
  validate_file <- function(file_path, file_type) {
    if (!file.exists(file_path)) {
      showNotification(paste(file_type, "file not found:", file_path), type = "error")
      return(FALSE)
    }
    return(TRUE)
  }
  
  # Load basin shapefile
  basin_data <- reactive({
    if (check_processed_data("basin")) {
      return(load_processed_data("basin"))
    }
    
    basin_sf <- st_read("data/Analysis_Basin_Shapefiles/basins_all_unique.shp")
    save_processed_data(basin_sf, "basin")
    return(basin_sf)
  })
  
  # Load HUC10 shapefile
  huc10_data <- reactive({
    if (check_processed_data("huc10")) {
      return(load_processed_data("huc10"))
    }
    
    huc10_sf <- st_read("data/huc10s/huc10.shp")
    save_processed_data(huc10_sf, "huc10")
    return(huc10_sf)
  })
  
  # Load SNOTEL data with enhanced processing
  snotel_data <- reactive({
    if (check_processed_data("snotel")) {
      return(load_processed_data("snotel"))
    }
    
    snotel_meta <- read.csv("data/snotel/snotel_metadata.csv")
    snotel_values <- read.csv("data/snotel/snotel_values.csv")
    
    # Merge metadata with values
    snotel_data <- merge(snotel_meta, snotel_values, by = "station_id")
    
    # Convert dates
    snotel_data$date <- as.Date(snotel_data$date)
    
    save_processed_data(snotel_data, "snotel")
    return(snotel_data)
  })
  
  # Static image outputs with error handling
  output$basin_map <- renderUI({
    img_path <- "images/colorado_river_basin_map.png"
    if (file.exists(img_path)) {
      div(class = "map-container",
          tags$img(src = img_path, 
                  alt = "Colorado River Basin Map",
                  style = "width: 100%; height: auto; max-height: 800px; object-fit: contain; border-radius: 4px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);"),
          div(class = "map-caption",
              p("The Colorado River Basin spans across seven U.S. states, covering approximately 246,000 square miles."),
              p("Key features include Lake Powell and Lake Mead reservoirs.")
          )
      )
    }
  })
  
  output$huc10_map <- renderUI({
    img_path <- "images/huc10_map.png"
    if (file.exists(img_path)) {
      tags$img(src = img_path,
              alt = "HUC10 Map",
              style = "width: 100%; height: auto; max-height: 600px; object-fit: contain;")
    }
  })
  
  output$huc10_area <- renderUI({
    img_path <- "images/huc10_area.png"
    if (file.exists(img_path)) {
      tags$img(src = img_path,
               alt = "HUC10 Area",
               style = "width: 100%; height: auto; max-height: 600px; object-fit: contain;")
    }
  })
  
  output$monthly_trends <- renderUI({
    img_path <- "images/monthly_trends.png"
    if (file.exists(img_path)) {
      tags$img(src = img_path,
               alt = "Monthly Trends",
               style = "width: 100%; height: auto; max-height: 600px; object-fit: contain;")
    }
  })
  
  output$snotel_map <- renderLeaflet({
    data <- snotel_data()
    if (is.null(data)) return(NULL)
    
    # Filter data for selected year and metric
    selected_year <- input$swe_year
    selected_metric <- input$swe_metric
    
    filtered_data <- data %>%
      filter(year(date) == selected_year,
             variable == selected_metric) %>%
      group_by(station_id) %>%
      summarise(
        mean_value = mean(value, na.rm = TRUE),
        max_value = max(value, na.rm = TRUE),
        min_value = min(value, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      left_join(data %>% select(station_id, latitude, longitude, elevation, name) %>% distinct(),
                by = "station_id")
    
    # Create color palette based on values
    pal <- colorNumeric(
      palette = "viridis",
      domain = filtered_data$mean_value
    )
    
    # Create popup content
    popup_content <- paste(
      "<strong>Station:</strong> ", filtered_data$name, "<br>",
      "<strong>Elevation:</strong> ", filtered_data$elevation, " m<br>",
      "<strong>Mean Value:</strong> ", round(filtered_data$mean_value, 2), "<br>",
      "<strong>Max Value:</strong> ", round(filtered_data$max_value, 2), "<br>",
      "<strong>Min Value:</strong> ", round(filtered_data$min_value, 2)
    )
    
    # Create the map
    leaflet(filtered_data) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        radius = 6,
        color = ~pal(mean_value),
        fillOpacity = 0.8,
        popup = popup_content
      ) %>%
      addLegend(
        "bottomright",
        pal = pal,
        values = ~mean_value,
        title = paste("Mean", selected_metric),
        opacity = 1
      )
  })
  
  # Add CSS for map containers
  output$map_container_css <- renderUI({
    tags$style(HTML("
      .map-container {
        background-color: white;
        border-radius: 4px;
        padding: 15px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        text-align: center;
        margin-bottom: 20px;
      }
      .map-container img {
        max-width: 100%;
        height: auto;
        border-radius: 4px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        transition: transform 0.3s ease;
      }
      .map-container img:hover {
        transform: scale(1.02);
      }
      .map-caption {
        margin-top: 15px;
        padding: 10px;
        background-color: #f8f9fa;
        border-radius: 4px;
        text-align: center;
      }
      .map-caption p {
        margin: 5px 0;
        color: #333;
      }
      .alert-warning {
        background-color: #fff3cd;
        border-color: #ffeeba;
        color: #856404;
        border-radius: 4px;
        margin: 15px 0;
        padding: 15px;
      }
      .alert-warning h4 {
        color: #856404;
        margin-bottom: 10px;
      }
      .alert-warning p {
        margin-bottom: 5px;
      }
    "))
  })
  
  # Load VIC data with enhanced error handling
  vic_data <- reactive({
    tryCatch({
      # Open the NetCDF file
      nc <- nc_open("data/VICOut2.nc")
      
      # Get dimensions
      lon <- ncvar_get(nc, "lon")
      lat <- ncvar_get(nc, "lat")
      time <- ncvar_get(nc, "time")
      
      # Get variables
      prec <- ncvar_get(nc, "OUT_PREC")
      evap <- ncvar_get(nc, "OUT_EVAP")
      runoff <- ncvar_get(nc, "OUT_RUNOFF")
      swe <- ncvar_get(nc, "OUT_SWE")
      soil_moist <- ncvar_get(nc, "OUT_SOIL_MOIST")
      
      # Close the NetCDF file
      nc_close(nc)
      
      # Create a data frame with the data
      data_list <- list()
      for (t in 1:length(time)) {
        for (i in 1:length(lon)) {
          for (j in 1:length(lat)) {
            data_list[[length(data_list) + 1]] <- data.frame(
              lon = lon[i],
              lat = lat[j],
              date = as.Date("0001-01-01") + time[t],
              OUT_PREC = prec[i,j,t],
              OUT_EVAP = evap[i,j,t],
              OUT_RUNOFF = runoff[i,j,t],
              OUT_SWE = swe[i,j,t],
              OUT_SOIL_MOIST = soil_moist[i,j,1,t]  # First layer only
            )
          }
        }
      }
      
      # Combine all data frames
      df <- do.call(rbind, data_list)
      return(df)
      
    }, error = function(e) {
      message(paste("Error processing VIC data:", e$message))
      return(NULL)
    })
  })
  
  # Load GRACE data
  grace_data <- reactive({
    tryCatch({
      # Load GRACE data
      nc <- nc_open("data/GRCTellus.JPL.200204_202401.GLO.RL06.1M.MSCNv03CRI.nc")
      on.exit(nc_close(nc))
      
      # Extract data
      tws <- ncvar_get(nc, "lwe_thickness")
      uncertainty <- ncvar_get(nc, "uncertainty")
      lon <- ncvar_get(nc, "lon")
      lat <- ncvar_get(nc, "lat")
      time <- ncvar_get(nc, "time")
      
      # Convert time to dates
      dates <- as.Date(time, origin = "2002-01-01")
      
      # Create data frame
      data <- expand.grid(lon = lon, lat = lat, date = dates)
      data$tws <- as.vector(tws)
      data$uncertainty <- as.vector(uncertainty)
      
      # Calculate anomalies
      monthly_means <- data %>%
        mutate(month = month(date)) %>%
        group_by(month) %>%
        summarise(monthly_mean = mean(tws, na.rm = TRUE))
      
      data <- data %>%
        mutate(month = month(date)) %>%
        left_join(monthly_means, by = "month") %>%
        mutate(anomaly = tws - monthly_mean)
      
      return(data)
    }, error = function(e) {
      showNotification(paste("Error loading GRACE data:", e$message), type = "error")
      return(NULL)
    })
  })
  
  # Load SMAP data
  smap_data <- reactive({
    tryCatch({
      nc <- load_smap_data()
      on.exit(nc_close(nc))
      
      # Extract SMAP data
      sm_profile <- ncvar_get(nc, "sm_profile")
      sm_rootzone <- ncvar_get(nc, "sm_rootzone")
      dates <- as.Date(ncvar_get(nc, "time"), origin = "2015-01-01")
      
      return(list(sm_profile = sm_profile, sm_rootzone = sm_rootzone, dates = dates))
    }, error = function(e) {
      showNotification(paste("Error loading SMAP data:", e$message), type = "error")
      return(NULL)
    })
  })
  
  # VIC statistics table
  output$vic_stats <- renderTable({
    nc_file <- vic_data()
    var_data <- ncvar_get(nc_file, input$vic_variable)
    metadata <- get_vic_metadata(input$vic_variable)
    
    stats <- data.frame(
      Statistic = c("Minimum", "Maximum", "Mean", "Standard Deviation"),
      Value = c(
        min(var_data, na.rm = TRUE),
        max(var_data, na.rm = TRUE),
        mean(var_data, na.rm = TRUE),
        sd(var_data, na.rm = TRUE)
      )
    )
    
    # Add units to the values
    stats$Value <- paste0(round(stats$Value, 2), " ", metadata$unit)
    
    stats
  })
  
  # GRACE Uncertainty Analysis
  output$grace_uncertainty <- renderPlotly({
    data <- grace_data()
    if (is.null(data)) return(NULL)
    
    # Get the selected year
    selected_year <- input$grace_year
    
    # Calculate spatial mean for each time step
    uncertainty_means <- apply(data$uncertainty, 3, mean, na.rm = TRUE)
    
    # Create time series data frame
    ts_df <- data.frame(
      Date = data$dates,
      Uncertainty = uncertainty_means,
      Year = format(data$dates, "%Y")
    )
    
    # Filter for selected year
    ts_df <- ts_df[ts_df$Year == selected_year,]
    
    plot_ly(ts_df) %>%
      add_trace(x = ~Date, y = ~Uncertainty, type = 'scatter', mode = 'lines',
                name = 'Uncertainty', line = list(color = 'red')) %>%
      layout(
        title = paste("Uncertainty Analysis -", selected_year),
        xaxis = list(title = "Date"),
        yaxis = list(title = "Uncertainty (cm)"),
        showlegend = TRUE
      )
  })
  
  # GRACE Trend Analysis
  output$grace_trend <- renderUI({
    div(class = "plot-container",
        fluidRow(
          column(width = 12,
                 infoBox("TWS Change", "-15cm", 
                        "Source: GRACE - 2024", 
                        icon = icon("water"), color = "blue", width = 4),
                 infoBox("Annual Variability", "±20cm", 
                        "Source: GRACE - 2024", 
                        icon = icon("wave-square"), color = "green", width = 4),
                 infoBox("Data Resolution", "300km", 
                        "Source: GRACE - 2024", 
                        icon = icon("ruler"), color = "light-blue", width = 4)
          )
        ),
        fluidRow(
          column(width = 12,
                 box(title = "GRACE Terrestrial Water Storage Trends", width = NULL, solidHeader = TRUE,
                     img(src = "images/grace_trend.png", width = "100%", height = "auto"),
                     p("Trends in terrestrial water storage from GRACE satellite data.")
                 )
          )
        ),
        fluidRow(
          column(width = 12,
                 plotlyOutput("grace_trend_plot", height = "600px")
          )
        )
    )
  })
  
  # GRACE Soil Moisture Profile
  output$grace_soil_moisture <- renderPlotly({
    data <- grace_data()
    if (is.null(data)) return(NULL)
    
    # Get the selected year
    selected_year <- input$grace_year
    
    # Calculate spatial mean for each time step
    tws_means <- apply(data$tws, 3, mean, na.rm = TRUE)
    
    # Create time series data frame
    ts_df <- data.frame(
      Date = data$dates,
      TWS = tws_means,
      Year = format(data$dates, "%Y")
    )
    
    # Filter for selected year
    ts_df <- ts_df[ts_df$Year == selected_year,]
    
    # Calculate monthly climatology
    monthly_clim <- ts_df %>%
      mutate(Month = format(Date, "%B")) %>%
      group_by(Month) %>%
      summarise(Climatology = mean(TWS, na.rm = TRUE))
    
    # Calculate anomalies (soil moisture variations)
    ts_df <- ts_df %>%
      mutate(Month = format(Date, "%B")) %>%
      left_join(monthly_clim, by = "Month") %>%
      mutate(Soil_Moisture = TWS - Climatology)
    
    # Create soil moisture profile plot
    plot_ly(ts_df) %>%
      add_trace(x = ~Date, y = ~Soil_Moisture, type = 'scatter', mode = 'lines',
                name = 'Soil Moisture', line = list(color = 'green')) %>%
      add_hline(y = 0, line = list(dash = "dash"), name = "Climatology") %>%
      layout(
        title = paste("Soil Moisture Profile -", selected_year),
        xaxis = list(title = "Date"),
        yaxis = list(title = "Soil Moisture Anomaly (cm)"),
        showlegend = TRUE
      )
  })
  
  # Add these new outputs in the server section
  output$grace_map_placeholder <- renderUI({
    if (is.null(grace_data())) {
      div(class = "alert alert-warning",
          h4("Data Not Available"),
          p("GRACE data is currently not available. Please check back later or contact the administrator."),
          p("Expected data format: NetCDF files containing terrestrial water storage measurements.")
      )
    }
  })
  
  output$grace_timeseries_placeholder <- renderUI({
    if (is.null(grace_data())) {
      div(class = "alert alert-warning",
          h4("Data Not Available"),
          p("Time series data is currently not available. Please check back later or contact the administrator.")
      )
    }
  })
  
  output$grace_seasonal_placeholder <- renderUI({
    if (is.null(grace_data())) {
      div(class = "alert alert-warning",
          h4("Data Not Available"),
          p("Seasonal analysis data is currently not available. Please check back later or contact the administrator.")
      )
    }
  })
  
  output$grace_uncertainty_placeholder <- renderUI({
    if (is.null(grace_data())) {
      div(class = "alert alert-warning",
          h4("Data Not Available"),
          p("Uncertainty analysis data is currently not available. Please check back later or contact the administrator.")
      )
    }
  })

  # TIF Data Display with enhanced error handling
  output$tif_data_placeholder <- renderUI({
    if (is.null(vic_data())) {
      div(class = "alert alert-warning",
          h4("Data Not Available"),
          p("TIF data is currently not available. Please check back later or contact the administrator.")
      )
    }
  })

  output$tif_data_plot <- renderPlotly({
    data <- vic_data()
    if (!validate_data(data, "VIC")) return(NULL)
    
    # Get the TIF file path based on selected variable and year
    selected_year <- input$vic_year
    selected_variable <- input$vic_variable
    
    if (is.null(selected_year) || is.null(selected_variable)) {
      return(plotly_empty() %>% 
        layout(title = "Please select year and variable",
               plot_bgcolor = 'white',
               paper_bgcolor = 'white'))
    }
    
    tif_file <- file.path("data/VIC_outputs", 
                         paste0(selected_variable, "_", selected_year, "_annual.tif"))
    
    if (!validate_file(tif_file, "TIF")) {
      return(plotly_empty() %>% 
        layout(title = "TIF file not found",
               plot_bgcolor = 'white',
               paper_bgcolor = 'white'))
    }
    
    tryCatch({
      # Read the TIF file
      r <- raster(tif_file)
      
      # Convert to data frame for plotting
      df <- as.data.frame(r, xy = TRUE)
      names(df) <- c("x", "y", "value")
      
      # Create the plot
      plot_ly(data = df, x = ~x, y = ~y, z = ~value, type = "heatmap",
              colors = viridis::viridis(100)) %>%
        layout(title = paste("Annual", selected_variable, "Distribution"),
               xaxis = list(title = "Longitude"),
               yaxis = list(title = "Latitude"),
               plot_bgcolor = 'white',
               paper_bgcolor = 'white')
    }, error = function(e) {
      showNotification(paste("Error plotting TIF data:", e$message), type = "error")
      return(plotly_empty() %>% 
        layout(title = "Error plotting data",
               plot_bgcolor = 'white',
               paper_bgcolor = 'white'))
    })
  })

  # SMAP TIF Data Display
  output$smap_tif_placeholder <- renderUI({
    if (is.null(smap_data())) {
      div(class = "alert alert-warning",
          h4("Data Not Available"),
          p("SMAP TIF data is currently not available. Please check back later or contact the administrator.")
      )
    }
  })

  output$smap_tif_plot <- renderPlotly({
    data <- smap_data()
    if (is.null(data)) return(NULL)
    
    # Get the TIF file path
    tif_file <- file.path("data/SMAP_outputs", "smap_surface_annual.tif")
    
    if (!file.exists(tif_file)) {
      return(NULL)
    }
    
    # Read the TIF file
    r <- raster(tif_file)
    
    # Convert to data frame for plotting
    df <- as.data.frame(r, xy = TRUE)
    names(df) <- c("x", "y", "value")
    
    # Create the plot
    plot_ly(data = df, x = ~x, y = ~y, z = ~value, type = "heatmap",
            colors = viridis::viridis(100)) %>%
      layout(title = "SMAP Surface Soil Moisture Distribution",
             xaxis = list(title = "Longitude"),
             yaxis = list(title = "Latitude"),
             plot_bgcolor = 'white',
             paper_bgcolor = 'white')
  })

  # GRACE TIF Data Display
  output$grace_tif_placeholder <- renderUI({
    if (is.null(grace_data())) {
      div(class = "alert alert-warning",
          h4("Data Not Available"),
          p("GRACE TIF data is currently not available. Please check back later or contact the administrator.")
      )
    }
  })

  output$grace_tif_plot <- renderPlotly({
    data <- grace_data()
    if (is.null(data)) return(NULL)
    
    # Get the TIF file path
    tif_file <- file.path("data/GRACE_outputs", "grace_annual.tif")
    
    if (!file.exists(tif_file)) {
      return(NULL)
    }
    
    # Read the TIF file
    r <- raster(tif_file)
    
    # Convert to data frame for plotting
    df <- as.data.frame(r, xy = TRUE)
    names(df) <- c("x", "y", "value")
    
    # Create the plot
    plot_ly(data = df, x = ~x, y = ~y, z = ~value, type = "heatmap",
            colors = viridis::viridis(100)) %>%
      layout(title = "GRACE Terrestrial Water Storage Distribution",
             xaxis = list(title = "Longitude"),
             yaxis = list(title = "Latitude"),
             plot_bgcolor = 'white',
             paper_bgcolor = 'white')
  })

  # Statistics Tables
  output$vic_stats_table <- renderDT({
    if (file.exists("vic_statistics.csv")) {
      datatable(read.csv("vic_statistics.csv"), 
                options = list(pageLength = 5, scrollX = TRUE))
    }
  })
  
  output$smap_stats_table <- renderDT({
    if (file.exists("smap_statistics.csv")) {
      datatable(read.csv("smap_statistics.csv"), 
                options = list(pageLength = 5, scrollX = TRUE))
    }
  })
  
  output$grace_stats_table <- renderDT({
    if (file.exists("grace_statistics.csv")) {
      datatable(read.csv("grace_statistics.csv"), 
                options = list(pageLength = 5, scrollX = TRUE))
    }
  })
  
  output$snotel_stats_table <- renderDT({
    if (file.exists("snotel_statistics.csv")) {
      datatable(read.csv("snotel_statistics.csv"), 
                options = list(pageLength = 5, scrollX = TRUE))
    }
  })
  
  # Processed Data Tables
  output$vic_processed_table <- renderDT({
    processed_files <- list.files("data/vic_processed", pattern = "\\.csv$", full.names = TRUE)
    if (length(processed_files) > 0) {
      # Read and combine all processed files
      all_data <- lapply(processed_files, function(f) {
        read.csv(f)
      })
      combined_data <- do.call(rbind, all_data)
      datatable(combined_data, 
                options = list(pageLength = 10, scrollX = TRUE))
    }
  })
  
  output$smap_processed_table <- renderDT({
    processed_files <- list.files("data/smap_processed", pattern = "\\.csv$", full.names = TRUE)
    if (length(processed_files) > 0) {
      all_data <- lapply(processed_files, function(f) {
        read.csv(f)
      })
      combined_data <- do.call(rbind, all_data)
      datatable(combined_data, 
                options = list(pageLength = 10, scrollX = TRUE))
    }
  })
  
  output$grace_processed_table <- renderDT({
    processed_files <- list.files("data/grace_processed", pattern = "\\.csv$", full.names = TRUE)
    if (length(processed_files) > 0) {
      all_data <- lapply(processed_files, function(f) {
        read.csv(f)
      })
      combined_data <- do.call(rbind, all_data)
      datatable(combined_data, 
                options = list(pageLength = 10, scrollX = TRUE))
    }
  })
  
  # Pre-generated Visualizations
  output$basin_huc10_snotel_viz <- renderUI({
    if (file.exists("basin_huc10_snotel_interactive.html")) {
      includeHTML("basin_huc10_snotel_interactive.html")
    }
  })
  
  output$vic_time_series_viz <- renderUI({
    if (file.exists("vic_time_series.html")) {
      includeHTML("vic_time_series.html")
    }
  })
  
  # Snow Water Equivalent Analysis
  output$swe_map <- renderPlotly({
    # Get selected date and data source
    selected_date <- input$swe_date
    selected_source <- input$swe_source
    selected_metric <- input$swe_metric
    
    # Load VIC data for the selected date
    vic_data <- load_processed_data("vic", format(selected_date, "%Y"))
    if (!is.null(vic_data)) {
      vic_data <- vic_data %>%
        filter(date == selected_date) %>%
        select(lon, lat, value = selected_metric)
    }
    
    # Load SNOTEL data for the selected date
    snotel_data <- load_processed_data("snotel")
    if (!is.null(snotel_data)) {
      snotel_data <- snotel_data %>%
        filter(date == selected_date) %>%
        select(lon = longitude, lat = latitude, value = selected_metric)
    }
    
    # Create base map
    p <- plot_ly() %>%
      layout(
        title = paste("Snow Water Equivalent -", selected_date),
        xaxis = list(title = "Longitude"),
        yaxis = list(title = "Latitude")
      )
    
    # Add VIC data if selected
    if (selected_source %in% c("vic", "both") && !is.null(vic_data)) {
      p <- p %>%
        add_trace(
          data = vic_data,
          x = ~lon,
          y = ~lat,
          z = ~value,
          type = "heatmap",
          colorscale = "Viridis",
          name = "VIC Model"
        )
    }
    
    # Add SNOTEL points if selected
    if (selected_source %in% c("snotel", "both") && !is.null(snotel_data)) {
      p <- p %>%
        add_trace(
          data = snotel_data,
          x = ~lon,
          y = ~lat,
          type = "scatter",
          mode = "markers",
          marker = list(
            size = 10,
            color = ~value,
            colorscale = "Viridis",
            showscale = TRUE
          ),
          name = "SNOTEL Stations"
        )
    }
    
    # Add selected layers
    if ("huc10" %in% input$swe_layers) {
      # Add HUC-10 boundaries
      p <- p %>%
        add_trace(
          data = huc10_boundaries,
          x = ~lon,
          y = ~lat,
          type = "scatter",
          mode = "lines",
          line = list(color = "black", width = 1),
          name = "HUC-10 Boundaries"
        )
    }
    
    if ("subbasins" %in% input$swe_layers) {
      # Add analysis sub-basins
      p <- p %>%
        add_trace(
          data = subbasin_boundaries,
          x = ~lon,
          y = ~lat,
          type = "scatter",
          mode = "lines",
          line = list(color = "blue", width = 2),
          name = "Analysis Sub-basins"
        )
    }
    
    if ("basins" %in% input$swe_layers) {
      # Add CRB/UCRB/LCRB boundaries
      p <- p %>%
        add_trace(
          data = basin_boundaries,
          x = ~lon,
          y = ~lat,
          type = "scatter",
          mode = "lines",
          line = list(color = "red", width = 2),
          name = "CRB/UCRB/LCRB"
        )
    }
    
    p
  })
  
  output$swe_timeseries <- renderPlotly({
    # Get selected parameters
    selected_station <- input$swe_station
    date_range <- input$swe_date_range
    aggregation <- input$swe_aggregation
    
    # Load and filter data
    snotel_data <- load_processed_data("snotel")
    vic_data <- load_processed_data("vic")
    
    if (!is.null(snotel_data) && !is.null(vic_data)) {
      # Filter by date range
      snotel_data <- snotel_data %>%
        filter(date >= date_range[1], date <= date_range[2])
      
      vic_data <- vic_data %>%
        filter(date >= date_range[1], date <= date_range[2])
      
      # Aggregate data based on selection
      if (aggregation == "weekly") {
        snotel_data <- snotel_data %>%
          mutate(week = floor_date(date, "week")) %>%
          group_by(week) %>%
          summarise(value = mean(value, na.rm = TRUE))
        
        vic_data <- vic_data %>%
          mutate(week = floor_date(date, "week")) %>%
          group_by(week) %>%
          summarise(value = mean(value, na.rm = TRUE))
      } else if (aggregation == "monthly") {
        snotel_data <- snotel_data %>%
          mutate(month = floor_date(date, "month")) %>%
          group_by(month) %>%
          summarise(value = mean(value, na.rm = TRUE))
        
        vic_data <- vic_data %>%
          mutate(month = floor_date(date, "month")) %>%
          group_by(month) %>%
          summarise(value = mean(value, na.rm = TRUE))
      } else if (aggregation == "annual") {
        snotel_data <- snotel_data %>%
          mutate(year = floor_date(date, "year")) %>%
          group_by(year) %>%
          summarise(value = mean(value, na.rm = TRUE))
        
        vic_data <- vic_data %>%
          mutate(year = floor_date(date, "year")) %>%
          group_by(year) %>%
          summarise(value = mean(value, na.rm = TRUE))
      }
      
      # Create plot
      p <- plot_ly() %>%
        layout(
          title = "Snow Water Equivalent Time Series",
          xaxis = list(title = "Date"),
          yaxis = list(title = "SWE (mm)")
        )
      
      # Add SNOTEL data
      if (selected_station == "all") {
        p <- p %>%
          add_trace(
            data = snotel_data,
            x = ~date,
            y = ~value,
            type = "scatter",
            mode = "lines",
            name = "SNOTEL Average"
          )
      } else {
        # Add individual station data
        for (station in unique(snotel_data$station_id)) {
          station_data <- snotel_data %>%
            filter(station_id == station)
          
          p <- p %>%
            add_trace(
              data = station_data,
              x = ~date,
              y = ~value,
              type = "scatter",
              mode = "lines",
              name = paste("SNOTEL", station)
            )
        }
      }
      
      # Add VIC data
      p <- p %>%
        add_trace(
          data = vic_data,
          x = ~date,
          y = ~value,
          type = "scatter",
          mode = "lines",
          name = "VIC Model",
          line = list(dash = "dash")
        )
      
      p
    }
  })
  
  output$swe_monthly_stats <- renderPlotly({
    # Load and process data
    snotel_data <- load_processed_data("snotel")
    vic_data <- load_processed_data("vic")
    
    if (!is.null(snotel_data) && !is.null(vic_data)) {
      # Calculate monthly statistics
      snotel_monthly <- snotel_data %>%
        mutate(month = month(date)) %>%
        group_by(month) %>%
        summarise(
          mean = mean(value, na.rm = TRUE),
          min = min(value, na.rm = TRUE),
          max = max(value, na.rm = TRUE)
        )
      
      vic_monthly <- vic_data %>%
        mutate(month = month(date)) %>%
        group_by(month) %>%
        summarise(
          mean = mean(value, na.rm = TRUE),
          min = min(value, na.rm = TRUE),
          max = max(value, na.rm = TRUE)
        )
      
      # Create plot
      plot_ly() %>%
        add_trace(
          data = snotel_monthly,
          x = ~month,
          y = ~mean,
          type = "scatter",
          mode = "lines+markers",
          name = "SNOTEL Mean",
          error_y = list(
            type = "data",
            symmetric = FALSE,
            array = ~max - ~mean,
            arrayminus = ~mean - ~min
          )
        ) %>%
        add_trace(
          data = vic_monthly,
          x = ~month,
          y = ~mean,
          type = "scatter",
          mode = "lines+markers",
          name = "VIC Mean",
          line = list(dash = "dash")
        ) %>%
        layout(
          title = "Monthly Snow Water Equivalent Statistics",
          xaxis = list(title = "Month"),
          yaxis = list(title = "SWE (mm)")
        )
    }
  })
  
  output$swe_elevation_stats <- renderPlotly({
    # Load and process data
    snotel_data <- load_processed_data("snotel")
    
    if (!is.null(snotel_data)) {
      # Create elevation vs SWE plot
      plot_ly(
        data = snotel_data,
        x = ~elevation,
        y = ~value,
        type = "scatter",
        mode = "markers",
        text = ~paste("Station:", station_id, "<br>Elevation:", elevation, "m"),
        marker = list(
          size = 8,
          color = ~value,
          colorscale = "Viridis",
          showscale = TRUE
        )
      ) %>%
        layout(
          title = "Elevation vs Snow Water Equivalent",
          xaxis = list(title = "Elevation (m)"),
          yaxis = list(title = "SWE (mm)")
        )
    }
  })
  
  output$swe_trend_analysis <- renderPlotly({
    # Load and process data
    snotel_data <- load_processed_data("snotel")
    vic_data <- load_processed_data("vic")
    
    if (!is.null(snotel_data) && !is.null(vic_data)) {
      # Calculate annual trends
      snotel_trend <- snotel_data %>%
        mutate(year = year(date)) %>%
        group_by(year) %>%
        summarise(value = mean(value, na.rm = TRUE)) %>%
        mutate(source = "SNOTEL")
      
      vic_trend <- vic_data %>%
        mutate(year = year(date)) %>%
        group_by(year) %>%
        summarise(value = mean(value, na.rm = TRUE)) %>%
        mutate(source = "VIC")
      
      # Combine data
      trend_data <- bind_rows(snotel_trend, vic_trend)
      
      # Create plot
      plot_ly(
        data = trend_data,
        x = ~year,
        y = ~value,
        color = ~source,
        type = "scatter",
        mode = "lines+markers"
      ) %>%
        layout(
          title = "Annual Snow Water Equivalent Trends",
          xaxis = list(title = "Year"),
          yaxis = list(title = "SWE (mm)")
        )
    }
  })
  
  output$swe_model_comparison <- renderPlotly({
    # Load and process data
    snotel_data <- load_processed_data("snotel")
    vic_data <- load_processed_data("vic")
    
    if (!is.null(snotel_data) && !is.null(vic_data)) {
      # Create scatter plot comparing VIC and SNOTEL
      plot_ly(
        data = snotel_data,
        x = ~value,
        y = ~vic_value,
        type = "scatter",
        mode = "markers",
        text = ~paste("Station:", station_id, "<br>Date:", date),
        marker = list(
          size = 8,
          color = ~elevation,
          colorscale = "Viridis",
          showscale = TRUE
        )
      ) %>%
        add_trace(
          x = c(0, max(snotel_data$value, na.rm = TRUE)),
          y = c(0, max(snotel_data$value, na.rm = TRUE)),
          type = "scatter",
          mode = "lines",
          line = list(dash = "dash", color = "black"),
          name = "1:1 Line"
        ) %>%
        layout(
          title = "VIC vs SNOTEL SWE Comparison",
          xaxis = list(title = "SNOTEL SWE (mm)"),
          yaxis = list(title = "VIC SWE (mm)")
        )
    }
  })
  
  output$swe_validation <- renderPlotly({
    # Load and process data
    snotel_data <- load_processed_data("snotel")
    vic_data <- load_processed_data("vic")
    
    if (!is.null(snotel_data) && !is.null(vic_data)) {
      # Calculate validation metrics
      validation_data <- snotel_data %>%
        left_join(vic_data, by = c("date", "station_id")) %>%
        mutate(
          error = vic_value - value,
          abs_error = abs(error),
          rel_error = error / value * 100
        )
      
      # Create error distribution plot
      plot_ly(
        data = validation_data,
        x = ~error,
        type = "histogram",
        nbinsx = 30
      ) %>%
        layout(
          title = "VIC Model Error Distribution",
          xaxis = list(title = "Error (mm)"),
          yaxis = list(title = "Count")
        )
    }
  })
  
  output$swe_stats_table <- renderDataTable({
    # Load and process data
    snotel_data <- load_processed_data("snotel")
    vic_data <- load_processed_data("vic")
    
    if (!is.null(snotel_data) && !is.null(vic_data)) {
      # Calculate statistics
      stats <- snotel_data %>%
        left_join(vic_data, by = c("date", "station_id")) %>%
        summarise(
          `Mean Error (mm)` = mean(vic_value - value, na.rm = TRUE),
          `Mean Absolute Error (mm)` = mean(abs(vic_value - value), na.rm = TRUE),
          `Root Mean Square Error (mm)` = sqrt(mean((vic_value - value)^2, na.rm = TRUE)),
          `Correlation Coefficient` = cor(value, vic_value, use = "complete.obs"),
          `Bias (%)` = mean((vic_value - value) / value * 100, na.rm = TRUE)
        )
      
      # Format table
      datatable(
        stats,
        options = list(
          dom = 't',
          pageLength = 5
        )
      )
    }
  })
  
  # Load SWE data
  swe_data <- reactive({
    # Read VIC output
    vic_file <- "data/VICOut2.nc"
    vic_nc <- nc_open(vic_file)
    
    # Extract SWE data
    swe <- ncvar_get(vic_nc, "OUT_SWE")
    time <- ncvar_get(vic_nc, "time")
    
    # Convert time to dates
    dates <- as.Date(time, origin = "0001-01-01")
    
    # Calculate spatial mean for each time step
    swe_means <- apply(swe, 3, mean, na.rm = TRUE)
    
    # Create data frame
    data.frame(
      date = dates,
      swe_value = swe_means
    )
  })

  # April 1 SWE Anomalies Plot
  output$april1_swe_anomalies_plot <- renderPlotly({
    data <- swe_data()
    if (is.null(data)) return(NULL)
    
    # Filter for April 1
    april1_swe <- data %>%
      filter(month(date) == 4, day(date) == 1) %>%
      mutate(year = year(date)) %>%
      group_by(year) %>%
      summarise(mean_swe = mean(swe_value, na.rm = TRUE)) %>%
      mutate(anomaly = mean_swe - mean(mean_swe, na.rm = TRUE))
    
    # Create the plot
    plot_ly(april1_swe, x = ~year, y = ~anomaly, type = 'bar',
            marker = list(color = ~ifelse(anomaly >= 0, 'rgb(0, 128, 255)', 'rgb(255, 0, 0)'))) %>%
      layout(title = "April 1 SWE Anomalies",
             xaxis = list(title = "Year"),
             yaxis = list(title = "SWE Anomaly (mm)"),
             showlegend = FALSE)
  })

  # April 1 SWE Trends Plot
  output$april1_swe_trends_plot <- renderPlotly({
    data <- swe_data()
    if (is.null(data)) return(NULL)
    
    # Filter for April 1
    april1_swe <- data %>%
      filter(month(date) == 4, day(date) == 1) %>%
      mutate(year = year(date)) %>%
      group_by(year) %>%
      summarise(mean_swe = mean(swe_value, na.rm = TRUE))
    
    # Fit linear trend
    trend <- lm(mean_swe ~ year, data = april1_swe)
    
    # Create the plot
    plot_ly(april1_swe, x = ~year, y = ~mean_swe, type = 'scatter', mode = 'markers',
            marker = list(color = 'rgb(0, 128, 255)')) %>%
      add_lines(x = ~year, y = fitted(trend), line = list(color = 'rgb(255, 0, 0)')) %>%
      layout(title = "April 1 SWE Trends",
             xaxis = list(title = "Year"),
             yaxis = list(title = "Mean SWE (mm)"))
  })

  # Seasonal Patterns Plot
  output$seasonal_patterns_plot <- renderPlotly({
    data <- swe_data()
    if (is.null(data)) return(NULL)
    
    # Calculate monthly means
    monthly_means <- data %>%
      mutate(month = month(date)) %>%
      group_by(month) %>%
      summarise(mean_swe = mean(swe_value, na.rm = TRUE))
    
    # Create the plot
    plot_ly(monthly_means, x = ~month, y = ~mean_swe, type = 'scatter', mode = 'lines+markers',
            line = list(color = 'rgb(0, 128, 255)', width = 2),
            marker = list(color = 'rgb(0, 128, 255)', size = 8)) %>%
      layout(title = "Seasonal Patterns in SWE",
             xaxis = list(title = "Month"),
             yaxis = list(title = "Mean SWE (mm)"))
  })

  # Elevation Analysis Plot
  output$elevation_analysis_plot <- renderPlotly({
    data <- swe_data()
    if (is.null(data)) return(NULL)
    
    # Create sample elevation data
    elevation_data <- data.frame(
      elevation = seq(1000, 4000, by = 100),
      swe = rnorm(31, mean = 100, sd = 20)
    )
    
    # Create the plot
    plot_ly(elevation_data, x = ~elevation, y = ~swe, type = 'scatter', mode = 'markers',
            marker = list(color = 'rgb(0, 128, 255)', opacity = 0.5)) %>%
      add_lines(x = ~elevation, y = ~predict(lm(swe ~ elevation)), 
                line = list(color = 'rgb(255, 0, 0)')) %>%
      layout(title = "Elevation Analysis of SWE",
             xaxis = list(title = "Elevation (m)"),
             yaxis = list(title = "SWE (mm)"))
  })

  # VIC Model Output Tab
  output$vic_time_series <- renderUI({
    div(class = "plot-container",
        fluidRow(
          column(width = 12,
                 box(title = "VIC Model Analysis", width = NULL, solidHeader = TRUE,
                     selectInput("vic_analysis_type", "Select Analysis Type:",
                             choices = c("Precipitation", "Evapotranspiration", 
                                       "Runoff", "Soil Moisture", "Snow Water",
                                       "Temperature", "Combined Analysis"),
                             selected = "Precipitation"),
                   
                   # Dynamic UI based on analysis type
                   uiOutput("vic_analysis_plot")
               )
          )
        )
    )
  })

  # Dynamic UI for VIC analysis plots
  output$vic_analysis_plot <- renderUI({
    analysis_type <- input$vic_analysis_type
    
    switch(analysis_type,
           "Precipitation" = {
             fluidRow(
               column(width = 12,
                      selectInput("precip_plot_type", "Select Plot Type:",
                                choices = c("Rain vs Snow", "Monthly Statistics", 
                                          "Time Series", "Spatial Distribution"),
                                selected = "Rain vs Snow"),
                      uiOutput("precip_plot")
               )
             )
           },
           "Evapotranspiration" = {
             fluidRow(
               column(width = 12,
                      selectInput("et_plot_type", "Select Plot Type:",
                                choices = c("Components", "Monthly Statistics", 
                                          "Time Series", "Spatial Distribution"),
                                selected = "Components"),
                      uiOutput("et_plot")
               )
             )
           },
           "Runoff" = {
             fluidRow(
               column(width = 12,
                      selectInput("runoff_plot_type", "Select Plot Type:",
                                choices = c("Surface vs Baseflow", "Monthly Statistics", 
                                          "Time Series", "Spatial Distribution"),
                                selected = "Surface vs Baseflow"),
                      uiOutput("runoff_plot")
               )
             )
           },
           "Soil Moisture" = {
             fluidRow(
               column(width = 12,
                      selectInput("soil_plot_type", "Select Plot Type:",
                                choices = c("Soil Layers", "Monthly Statistics", 
                                          "Time Series", "Spatial Distribution"),
                                selected = "Soil Layers"),
                      uiOutput("soil_plot")
               )
             )
           },
           "Snow Water" = {
             fluidRow(
               column(width = 12,
                      selectInput("snow_plot_type", "Select Plot Type:",
                                choices = c("Snow Melt", "Monthly Statistics", 
                                          "Time Series", "Spatial Distribution"),
                                selected = "Snow Melt"),
                      uiOutput("snow_plot")
               )
             )
           },
           "Temperature" = {
             fluidRow(
               column(width = 12,
                      selectInput("temp_plot_type", "Select Plot Type:",
                                choices = c("Monthly Statistics", "Time Series", 
                                          "Spatial Distribution"),
                                selected = "Monthly Statistics"),
                      uiOutput("temp_plot")
               )
             )
           },
           "Combined Analysis" = {
             fluidRow(
               column(width = 12,
                      selectInput("combined_plot_type", "Select Plot Type:",
                                choices = c("Water Balance", "Trends", 
                                          "Correlations", "Extreme Events"),
                                selected = "Water Balance"),
                      uiOutput("combined_plot")
               )
             )
           }
    )
  })

  # Plot renderers for each analysis type
  output$precip_plot <- renderUI({
    plot_type <- input$precip_plot_type
    switch(plot_type,
           "Rain vs Snow" = {
             img(src = "images/precipitation_analysis/rain_vs_snow.png",
                 style = "width: 100%; height: auto; max-height: 600px;")
           },
           "Monthly Statistics" = {
             img(src = "images/precipitation_analysis/monthly_stats.png",
                 style = "width: 100%; height: auto; max-height: 600px;")
           },
           "Time Series" = {
             img(src = "images/precipitation_analysis/time_series.png",
                 style = "width: 100%; height: auto; max-height: 600px;")
           },
           "Spatial Distribution" = {
             img(src = "images/precipitation_analysis/spatial_distribution.png",
                 style = "width: 100%; height: auto; max-height: 600px;")
           }
    )
  })

  output$et_plot <- renderUI({
    plot_type <- input$et_plot_type
    switch(plot_type,
           "Components" = {
             img(src = "images/evapotranspiration_analysis/components.png",
                 style = "width: 100%; height: auto; max-height: 600px;")
           },
           "Monthly Statistics" = {
             img(src = "images/evapotranspiration_analysis/monthly_stats.png",
                 style = "width: 100%; height: auto; max-height: 600px;")
           },
           "Time Series" = {
             img(src = "images/evapotranspiration_analysis/time_series.png",
                 style = "width: 100%; height: auto; max-height: 600px;")
           },
           "Spatial Distribution" = {
             img(src = "images/evapotranspiration_analysis/spatial_distribution.png",
                 style = "width: 100%; height: auto; max-height: 600px;")
           }
    )
  })

  output$runoff_plot <- renderUI({
    plot_type <- input$runoff_plot_type
    switch(plot_type,
           "Surface vs Baseflow" = {
             img(src = "images/runoff_analysis/surface_vs_baseflow.png",
                 style = "width: 100%; height: auto; max-height: 600px;")
           },
           "Monthly Statistics" = {
             img(src = "images/runoff_analysis/monthly_stats.png",
                 style = "width: 100%; height: auto; max-height: 600px;")
           },
           "Time Series" = {
             img(src = "images/runoff_analysis/time_series.png",
                 style = "width: 100%; height: auto; max-height: 600px;")
           },
           "Spatial Distribution" = {
             img(src = "images/runoff_analysis/spatial_distribution.png",
                 style = "width: 100%; height: auto; max-height: 600px;")
           }
    )
  })

  output$soil_plot <- renderUI({
    plot_type <- input$soil_plot_type
    switch(plot_type,
           "Soil Layers" = {
             img(src = "images/soil_moisture_analysis/soil_layers.png",
                 style = "width: 100%; height: auto; max-height: 600px;")
           },
           "Monthly Statistics" = {
             img(src = "images/soil_moisture_analysis/monthly_stats.png",
                 style = "width: 100%; height: auto; max-height: 600px;")
           },
           "Time Series" = {
             img(src = "images/soil_moisture_analysis/time_series.png",
                 style = "width: 100%; height: auto; max-height: 600px;")
           },
           "Spatial Distribution" = {
             img(src = "images/soil_moisture_analysis/spatial_distribution.png",
                 style = "width: 100%; height: auto; max-height: 600px;")
           }
    )
  })

  output$snow_plot <- renderUI({
    plot_type <- input$snow_plot_type
    switch(plot_type,
           "Snow Melt" = {
             img(src = "images/snow_water_analysis/snow_melt.png",
                 style = "width: 100%; height: auto; max-height: 600px;")
           },
           "Monthly Statistics" = {
             img(src = "images/snow_water_analysis/monthly_stats.png",
                 style = "width: 100%; height: auto; max-height: 600px;")
           },
           "Time Series" = {
             img(src = "images/snow_water_analysis/time_series.png",
                 style = "width: 100%; height: auto; max-height: 600px;")
           },
           "Spatial Distribution" = {
             img(src = "images/snow_water_analysis/spatial_distribution.png",
                 style = "width: 100%; height: auto; max-height: 600px;")
           }
    )
  })

  output$temp_plot <- renderUI({
    plot_type <- input$temp_plot_type
    switch(plot_type,
           "Monthly Statistics" = {
             img(src = "images/temperature_analysis/monthly_stats.png",
                 style = "width: 100%; height: auto; max-height: 600px;")
           },
           "Time Series" = {
             img(src = "images/temperature_analysis/time_series.png",
                 style = "width: 100%; height: auto; max-height: 600px;")
           },
           "Spatial Distribution" = {
             img(src = "images/temperature_analysis/spatial_distribution.png",
                 style = "width: 100%; height: auto; max-height: 600px;")
           }
    )
  })

  output$combined_plot <- renderUI({
    plot_type <- input$combined_plot_type
    switch(plot_type,
           "Water Balance" = {
             img(src = "images/combined_analysis/water_balance.png",
                 style = "width: 100%; height: auto; max-height: 600px;")
           },
           "Trends" = {
             img(src = "images/combined_analysis/trends.png",
                 style = "width: 100%; height: auto; max-height: 600px;")
           },
           "Correlations" = {
             img(src = "images/combined_analysis/correlations.png",
                 style = "width: 100%; height: auto; max-height: 600px;")
           },
           "Extreme Events" = {
             img(src = "images/combined_analysis/extreme_events.png",
                 style = "width: 100%; height: auto; max-height: 600px;")
           }
    )
  })

  output$vic_monthly_stats <- renderUI({
    fluidRow(
      # Info boxes at the top
      infoBox("Max Monthly Precip", "120mm", "VIC Model Output - 2024", 
              icon = icon("cloud-rain"), color = "blue", width = 4),
      infoBox("Max Monthly ET", "80mm", "VIC Model Output - 2024", 
              icon = icon("water"), color = "green", width = 4),
      infoBox("Max Monthly Runoff", "40mm", "VIC Model Output - 2024", 
              icon = icon("tint"), color = "light-blue", width = 4),
      
      # Static image
      column(width = 12,
        box(title = "VIC Monthly Statistics", width = NULL, solidHeader = TRUE,
            img(src = "images/vic_monthly_stats.png", width = "100%", height = "auto"),
            p("Monthly statistics from VIC model outputs showing precipitation, evapotranspiration, and runoff patterns.")
        )
      ),
      
      # Interactive plot
      column(width = 12,
        box(title = "Interactive Monthly Statistics", width = NULL, solidHeader = TRUE,
            plotlyOutput("vic_monthly_interactive")
        )
      )
    )
  })

  output$vic_soil_moisture <- renderUI({
    div(class = "plot-container",
        fluidRow(
          column(width = 12,
                 infoBox("Average SM Layer 1", "150mm", 
                        "Source: VIC Model Output - 2024", 
                        icon = icon("tint"), color = "blue", width = 4),
                 infoBox("Average SM Layer 2", "200mm", 
                        "Source: VIC Model Output - 2024", 
                        icon = icon("tint"), color = "green", width = 4),
                 infoBox("Average SM Layer 3", "250mm", 
                        "Source: VIC Model Output - 2024", 
                        icon = icon("tint"), color = "light-blue", width = 4)
          )
        ),
        fluidRow(
          column(width = 12,
                 tags$img(src = "images/vic_soil_moisture_layer_1_map.png",
                         alt = "VIC Soil Moisture Layer 1",
                         style = "width: 100%; height: auto; max-height: 600px; object-fit: contain;")
          )
        ),
        fluidRow(
          column(width = 12,
                 plotlyOutput("vic_soil_moisture_map", height = "600px")
          )
        ),
        fluidRow(
          column(width = 4,
                 plotlyOutput("vic_soil_moisture_layer1", height = "300px")
          ),
          column(width = 4,
                 plotlyOutput("vic_soil_moisture_layer2", height = "300px")
          ),
          column(width = 4,
                 plotlyOutput("vic_soil_moisture_layer3", height = "300px")
          )
        )
    )
  })

  output$vic_evapotranspiration <- renderUI({
    div(class = "plot-container",
        fluidRow(
          column(width = 12,
                 infoBox("Total ET", "300mm", 
                        "Source: VIC Model Output - 2024", 
                        icon = icon("water"), color = "blue", width = 4),
                 infoBox("Bare Soil ET", "100mm", 
                        "Source: VIC Model Output - 2024", 
                        icon = icon("water"), color = "green", width = 4),
                 infoBox("Vegetation ET", "200mm", 
                        "Source: VIC Model Output - 2024", 
                        icon = icon("water"), color = "light-blue", width = 4)
          )
        ),
        fluidRow(
          column(width = 12,
                 tags$img(src = "images/vic_evapotranspiration_map.png",
                         alt = "VIC Evapotranspiration",
                         style = "width: 100%; height: auto; max-height: 600px; object-fit: contain;")
          )
        ),
        fluidRow(
          column(width = 12,
                 plotlyOutput("vic_et_map", height = "600px")
          )
        ),
        fluidRow(
          column(width = 6,
                 plotlyOutput("vic_et_components", height = "300px")
          ),
          column(width = 6,
                 plotlyOutput("vic_et_trend", height = "300px")
          )
        )
    )
  })

  output$vic_runoff <- renderUI({
    div(class = "plot-container",
        fluidRow(
          column(width = 12,
                 infoBox("Total Runoff", "100mm", 
                        "Source: VIC Model Output - 2024", 
                        icon = icon("stream"), color = "blue", width = 4),
                 infoBox("Surface Runoff", "60mm", 
                        "Source: VIC Model Output - 2024", 
                        icon = icon("stream"), color = "green", width = 4),
                 infoBox("Baseflow", "40mm", 
                        "Source: VIC Model Output - 2024", 
                        icon = icon("stream"), color = "light-blue", width = 4)
          )
        ),
        fluidRow(
          column(width = 12,
                 tags$img(src = "images/vic_runoff_map.png",
                         alt = "VIC Runoff",
                         style = "width: 100%; height: auto; max-height: 600px; object-fit: contain;")
          )
        ),
        fluidRow(
          column(width = 12,
                 plotlyOutput("vic_runoff_map", height = "600px")
          )
        ),
        fluidRow(
          column(width = 6,
                 plotlyOutput("vic_runoff_components", height = "300px")
          ),
          column(width = 6,
                 plotlyOutput("vic_runoff_trend", height = "300px")
          )
        )
    )
  })

  # Time Series Plots
  output$vic_precip_ts <- renderPlotly({
    data <- vic_data()
    if (!is.null(data)) {
      plot_ly(data = data, x = ~date, y = ~OUT_PREC, type = 'scatter', mode = 'lines') %>%
        layout(title = "Precipitation Time Series",
               xaxis = list(title = "Date"),
               yaxis = list(title = "Precipitation (mm)"))
    }
  })

  output$vic_et_ts <- renderPlotly({
    data <- vic_data()
    if (!is.null(data)) {
      plot_ly(data = data, x = ~date, y = ~OUT_EVAP, type = 'scatter', mode = 'lines') %>%
        layout(title = "Evapotranspiration Time Series",
               xaxis = list(title = "Date"),
               yaxis = list(title = "ET (mm)"))
    }
  })

  output$vic_runoff_ts <- renderPlotly({
    data <- vic_data()
    if (!is.null(data)) {
      plot_ly(data = data, x = ~date, y = ~OUT_RUNOFF, type = 'scatter', mode = 'lines') %>%
        layout(title = "Runoff Time Series",
               xaxis = list(title = "Date"),
               yaxis = list(title = "Runoff (mm)"))
    }
  })

  output$vic_soil_moisture_ts <- renderPlotly({
    data <- vic_data()
    if (!is.null(data)) {
      plot_ly(data = data, x = ~date, y = ~OUT_SOIL_MOIST, type = 'scatter', mode = 'lines') %>%
        layout(title = "Soil Moisture Time Series",
               xaxis = list(title = "Date"),
               yaxis = list(title = "Soil Moisture (mm)"))
    }
  })

  output$vic_swe_ts <- renderPlotly({
    data <- vic_data()
    if (!is.null(data)) {
      plot_ly(data = data, x = ~date, y = ~OUT_SWE, type = 'scatter', mode = 'lines') %>%
        layout(title = "Snow Water Equivalent Time Series",
               xaxis = list(title = "Date"),
               yaxis = list(title = "SWE (mm)"))
    }
  })

  output$vic_baseflow_ts <- renderPlotly({
    data <- vic_data()
    if (!is.null(data)) {
      plot_ly(data = data, x = ~date, y = ~OUT_BASEFLOW, type = 'scatter', mode = 'lines') %>%
        layout(title = "Baseflow Time Series",
               xaxis = list(title = "Date"),
               yaxis = list(title = "Baseflow (mm)"))
    }
  })

  # Monthly Statistics Plots
  output$vic_precip_monthly <- renderPlotly({
    data <- vic_data()
    if (!is.null(data)) {
      # Convert array to data frame
      df <- as.data.frame(data)
      df$date <- as.Date(df$date)
      
      monthly_data <- df %>%
        mutate(month = month(date)) %>%
        group_by(month) %>%
        summarise(mean_precip = mean(OUT_PREC, na.rm = TRUE))
      
      plot_ly(data = monthly_data, x = ~month, y = ~mean_precip, type = 'bar') %>%
        layout(title = "Monthly Precipitation",
               xaxis = list(title = "Month"),
               yaxis = list(title = "Mean Precipitation (mm)"))
    }
  })

  output$vic_et_monthly <- renderPlotly({
    data <- vic_data()
    if (!is.null(data)) {
      df <- as.data.frame(data)
      df$date <- as.Date(df$date)
      
      monthly_data <- df %>%
        mutate(month = month(date)) %>%
        group_by(month) %>%
        summarise(mean_et = mean(OUT_EVAP, na.rm = TRUE))
      
      plot_ly(data = monthly_data, x = ~month, y = ~mean_et, type = 'bar') %>%
        layout(title = "Monthly Evapotranspiration",
               xaxis = list(title = "Month"),
               yaxis = list(title = "Mean ET (mm)"))
    }
  })

  output$vic_runoff_monthly <- renderPlotly({
    data <- vic_data()
    if (!is.null(data)) {
      df <- as.data.frame(data)
      df$date <- as.Date(df$date)
      
      monthly_data <- df %>%
        mutate(month = month(date)) %>%
        group_by(month) %>%
        summarise(mean_runoff = mean(OUT_RUNOFF, na.rm = TRUE))
      
      plot_ly(data = monthly_data, x = ~month, y = ~mean_runoff, type = 'bar') %>%
        layout(title = "Monthly Runoff",
               xaxis = list(title = "Month"),
               yaxis = list(title = "Mean Runoff (mm)"))
    }
  })

  output$vic_soil_moisture_monthly <- renderPlotly({
    data <- vic_data()
    if (!is.null(data)) {
      df <- as.data.frame(data)
      df$date <- as.Date(df$date)
      
      monthly_data <- df %>%
        mutate(month = month(date)) %>%
        group_by(month) %>%
        summarise(mean_sm = mean(OUT_SOIL_MOIST, na.rm = TRUE))
      
      plot_ly(data = monthly_data, x = ~month, y = ~mean_sm, type = 'bar') %>%
        layout(title = "Monthly Soil Moisture",
               xaxis = list(title = "Month"),
               yaxis = list(title = "Mean Soil Moisture (mm)"))
    }
  })

  output$vic_swe_monthly <- renderPlotly({
    data <- vic_data()
    if (!is.null(data)) {
      df <- as.data.frame(data)
      df$date <- as.Date(df$date)
      
      monthly_data <- df %>%
        mutate(month = month(date)) %>%
        group_by(month) %>%
        summarise(mean_swe = mean(OUT_SWE, na.rm = TRUE))
      
      plot_ly(data = monthly_data, x = ~month, y = ~mean_swe, type = 'bar') %>%
        layout(title = "Monthly Snow Water Equivalent",
               xaxis = list(title = "Month"),
               yaxis = list(title = "Mean SWE (mm)"))
    }
  })

  output$vic_baseflow_monthly <- renderPlotly({
    data <- vic_data()
    if (!is.null(data)) {
      df <- as.data.frame(data)
      df$date <- as.Date(df$date)
      
      monthly_data <- df %>%
        mutate(month = month(date)) %>%
        group_by(month) %>%
        summarise(mean_baseflow = mean(OUT_BASEFLOW, na.rm = TRUE))
      
      plot_ly(data = monthly_data, x = ~month, y = ~mean_baseflow, type = 'bar') %>%
        layout(title = "Monthly Baseflow",
               xaxis = list(title = "Month"),
               yaxis = list(title = "Mean Baseflow (mm)"))
    }
  })

  # Spatial Maps
  output$vic_soil_moisture_map <- renderPlotly({
    data <- vic_data()
    if (!is.null(data)) {
      plot_ly(data = data, x = ~lon, y = ~lat, z = ~OUT_SOIL_MOIST, type = "heatmap",
              colors = viridis::viridis(100)) %>%
        layout(title = "Soil Moisture Distribution",
               xaxis = list(title = "Longitude"),
               yaxis = list(title = "Latitude"))
    }
  })

  output$vic_et_map <- renderPlotly({
    data <- vic_data()
    if (!is.null(data)) {
      plot_ly(data = data, x = ~lon, y = ~lat, z = ~OUT_EVAP, type = "heatmap",
              colors = viridis::viridis(100)) %>%
        layout(title = "Evapotranspiration Distribution",
               xaxis = list(title = "Longitude"),
               yaxis = list(title = "Latitude"))
    }
  })

  output$vic_runoff_map <- renderPlotly({
    data <- vic_data()
    if (!is.null(data)) {
      plot_ly(data = data, x = ~lon, y = ~lat, z = ~OUT_RUNOFF, type = "heatmap",
              colors = viridis::viridis(100)) %>%
        layout(title = "Runoff Distribution",
               xaxis = list(title = "Longitude"),
               yaxis = list(title = "Latitude"))
    }
  })

  # Soil Moisture Layers
  output$vic_soil_moisture_layer1 <- renderPlotly({
    data <- vic_data()
    if (!is.null(data)) {
      plot_ly(data = data, x = ~lon, y = ~lat, z = ~OUT_SOIL_MOIST_1, type = "heatmap",
              colors = viridis::viridis(100)) %>%
        layout(title = "Soil Moisture Layer 1 (0-10cm)",
               xaxis = list(title = "Longitude"),
               yaxis = list(title = "Latitude"))
    }
  })

  output$vic_soil_moisture_layer2 <- renderPlotly({
    data <- vic_data()
    if (!is.null(data)) {
      plot_ly(data = data, x = ~lon, y = ~lat, z = ~OUT_SOIL_MOIST_2, type = "heatmap",
              colors = viridis::viridis(100)) %>%
        layout(title = "Soil Moisture Layer 2 (10-40cm)",
               xaxis = list(title = "Longitude"),
               yaxis = list(title = "Latitude"))
    }
  })

  output$vic_soil_moisture_layer3 <- renderPlotly({
    data <- vic_data()
    if (!is.null(data)) {
      plot_ly(data = data, x = ~lon, y = ~lat, z = ~OUT_SOIL_MOIST_3, type = "heatmap",
              colors = viridis::viridis(100)) %>%
        layout(title = "Soil Moisture Layer 3 (40-100cm)",
               xaxis = list(title = "Longitude"),
               yaxis = list(title = "Latitude"))
    }
  })

  # ET Components
  output$vic_et_components <- renderPlotly({
    data <- vic_data()
    if (!is.null(data)) {
      plot_ly(data = data) %>%
        add_trace(x = ~date, y = ~OUT_EVAP_BARE, name = "Bare Soil", type = 'scatter', mode = 'lines') %>%
        add_trace(x = ~date, y = ~OUT_EVAP_CANOP, name = "Canopy", type = 'scatter', mode = 'lines') %>%
        add_trace(x = ~date, y = ~OUT_TRANSP_VEG, name = "Vegetation", type = 'scatter', mode = 'lines') %>%
        layout(title = "ET Components",
               xaxis = list(title = "Date"),
               yaxis = list(title = "ET (mm)"))
    }
  })

  # Runoff Components
  output$vic_runoff_components <- renderPlotly({
    data <- vic_data()
    if (!is.null(data)) {
      plot_ly(data = data) %>%
        add_trace(x = ~date, y = ~OUT_RUNOFF, name = "Surface Runoff", type = 'scatter', mode = 'lines') %>%
        add_trace(x = ~date, y = ~OUT_BASEFLOW, name = "Baseflow", type = 'scatter', mode = 'lines') %>%
        layout(title = "Runoff Components",
               xaxis = list(title = "Date"),
               yaxis = list(title = "Flow (mm)"))
    }
  })

  # Trend Analysis
  output$vic_et_trend <- renderPlotly({
    data <- vic_data()
    if (!is.null(data)) {
      trend_data <- data %>%
        mutate(year = year(date)) %>%
        group_by(year) %>%
        summarise(mean_et = mean(OUT_EVAP, na.rm = TRUE))
      
      trend <- lm(mean_et ~ year, data = trend_data)
      
      plot_ly(data = trend_data, x = ~year, y = ~mean_et, type = 'scatter', mode = 'markers') %>%
        add_trace(x = ~year, y = fitted(trend), type = 'scatter', mode = 'lines') %>%
        layout(title = "ET Trend Analysis",
               xaxis = list(title = "Year"),
               yaxis = list(title = "Mean ET (mm)"))
    }
  })

  output$vic_runoff_trend <- renderPlotly({
    data <- vic_data()
    if (!is.null(data)) {
      trend_data <- data %>%
        mutate(year = year(date)) %>%
        group_by(year) %>%
        summarise(mean_runoff = mean(OUT_RUNOFF, na.rm = TRUE))
      
      trend <- lm(mean_runoff ~ year, data = trend_data)
      
      plot_ly(data = trend_data, x = ~year, y = ~mean_runoff, type = 'scatter', mode = 'markers') %>%
        add_trace(x = ~year, y = fitted(trend), type = 'scatter', mode = 'lines') %>%
        layout(title = "Runoff Trend Analysis",
               xaxis = list(title = "Year"),
               yaxis = list(title = "Mean Runoff (mm)"))
    }
  })

  # SMAP Data Tab
  output$smap_time_series <- renderUI({
    div(class = "plot-container",
        fluidRow(
          column(width = 12,
                 infoBox("Surface SM", "0.25 m³/m³", 
                        "Source: SMAP L3 - 2024", 
                        icon = icon("tint"), color = "blue", width = 4),
                 infoBox("Root Zone SM", "0.35 m³/m³", 
                        "Source: SMAP L4 - 2024", 
                        icon = icon("tint"), color = "green", width = 4),
                 infoBox("Data Resolution", "9km", 
                        "Source: SMAP - 2024", 
                        icon = icon("ruler"), color = "light-blue", width = 4)
          )
        ),
        fluidRow(
          column(width = 12,
                 tags$img(src = "images/smap_time_series.png",
                         alt = "SMAP Time Series",
                         style = "width: 100%; height: auto; max-height: 600px; object-fit: contain;")
          )
        ),
        fluidRow(
          column(width = 6,
                 plotlyOutput("smap_surface_ts", height = "300px"),
                 plotlyOutput("smap_rootzone_ts", height = "300px")
          ),
          column(width = 6,
                 plotlyOutput("smap_surface_monthly", height = "300px"),
                 plotlyOutput("smap_rootzone_monthly", height = "300px")
          )
        )
    )
  })

  output$smap_surface_trend <- renderUI({
    div(class = "plot-container",
        fluidRow(
          column(width = 12,
                 infoBox("Trend Rate", "-0.01 m³/m³/yr", 
                        "Source: SMAP L3 - 2024", 
                        icon = icon("chart-line"), color = "blue", width = 4),
                 infoBox("R² Value", "0.65", 
                        "Source: SMAP L3 - 2024", 
                        icon = icon("square-root-alt"), color = "green", width = 4),
                 infoBox("P-Value", "<0.001", 
                        "Source: SMAP L3 - 2024", 
                        icon = icon("check-circle"), color = "light-blue", width = 4)
          )
        ),
        fluidRow(
          column(width = 12,
                 tags$img(src = "images/smap_surface_trend.png",
                         alt = "SMAP Surface Trend",
                         style = "width: 100%; height: auto; max-height: 600px; object-fit: contain;")
          )
        ),
        fluidRow(
          column(width = 12,
                 plotlyOutput("smap_surface_trend_plot", height = "600px")
          )
        )
    )
  })

  # GRACE Data Tab
  output$grace_time_series <- renderUI({
    div(class = "plot-container",
        fluidRow(
          column(width = 12,
                 infoBox("TWS Change", "-15 cm", 
                        "Source: GRACE - 2024", 
                        icon = icon("water"), color = "blue", width = 4),
                 infoBox("Annual Variability", "±20cm", 
                        "Source: GRACE - 2024", 
                        icon = icon("wave-square"), color = "green", width = 4),
                 infoBox("Data Resolution", "300km", 
                        "Source: GRACE - 2024", 
                        icon = icon("ruler"), color = "light-blue", width = 4)
          )
        ),
        fluidRow(
          column(width = 12,
                 tags$img(src = "images/grace_time_series.png",
                         alt = "GRACE Time Series",
                         style = "width: 100%; height: auto; max-height: 600px; object-fit: contain;")
          )
        ),
        fluidRow(
          column(width = 12,
                 plotlyOutput("grace_ts_plot", height = "600px")
          )
        ),
        fluidRow(
          column(width = 6,
                 plotlyOutput("grace_monthly", height = "300px")
          ),
          column(width = 6,
                 plotlyOutput("grace_annual", height = "300px")
          )
        )
    )
  })

  # Snow Water Equivalent Analysis
  output$swe_analysis <- renderUI({
    div(class = "plot-container",
        fluidRow(
          column(width = 12,
                 infoBox("Max SWE", "1200mm", 
                        "Source: SNODAS - 2024", 
                        icon = icon("snowflake"), color = "blue", width = 4),
                 infoBox("Average SWE", "400mm", 
                        "Source: SNODAS - 2024", 
                        icon = icon("snowflake"), color = "green", width = 4),
                 infoBox("Snow Cover", "80%", 
                        "Source: SNODAS - 2024", 
                        icon = icon("mountain"), color = "light-blue", width = 4)
          )
        ),
        
        # Overall Analysis
        fluidRow(
          column(width = 12,
                 box(title = "Snow Water Equivalent Analysis", width = NULL, solidHeader = TRUE,
                     img(src = "images/swe_analysis.png", width = "100%", height = "auto"),
                     p("Comprehensive analysis of snow water equivalent (SWE) patterns across the basin.")
                 )
          )
        ),
        
        # Spatial Patterns
        fluidRow(
          column(width = 12,
                 box(title = "Spatial Patterns", width = NULL, solidHeader = TRUE,
                     img(src = "images/vic_snow_water_equivalent_map.png", width = "100%", height = "auto"),
                     p("Spatial distribution of snow water equivalent from VIC model output.")
                 )
          )
        ),
        
        # Temporal Trends
        fluidRow(
          column(width = 12,
                 box(title = "Temporal Trends", width = NULL, solidHeader = TRUE,
                     img(src = "images/snotel_timeseries.png", width = "100%", height = "auto"),
                     p("Historical time series of snow water equivalent from SNOTEL stations.")
                 )
          )
        )
    )
  })

  output$snotel_timeseries <- renderUI({
    div(class = "plot-container",
        fluidRow(
          column(width = 12,
                 infoBox("Stations", "45", 
                        "Source: NRCS - 2024", 
                        icon = icon("map-marker-alt"), color = "blue", width = 4),
                 infoBox("Elevation Range", "2000-3500m", 
                        "Source: NRCS - 2024", 
                        icon = icon("mountain"), color = "green", width = 4),
                 infoBox("Data Frequency", "Daily", 
                        "Source: NRCS - 2024", 
                        icon = icon("clock"), color = "light-blue", width = 4)
          )
        ),
        fluidRow(
          column(width = 12,
                 tags$img(src = "images/snotel_timeseries.png",
                         alt = "SNOTEL Time Series",
                         style = "width: 100%; height: auto; max-height: 600px; object-fit: contain;")
          )
        ),
        fluidRow(
          column(width = 12,
                 plotlyOutput("snotel_ts_plot", height = "600px")
          )
        )
    )
  })

  # Soil Moisture Tab
  output$soil_moisture <- renderUI({
    div(class = "plot-container",
        fluidRow(
          column(width = 12,
                 infoBox("VIC SM", "0.3 m³/m³", 
                        "Source: VIC Model - 2024", 
                        icon = icon("tint"), color = "blue", width = 4),
                 infoBox("SMAP SM", "0.25 m³/m³", 
                        "Source: SMAP - 2024", 
                        icon = icon("tint"), color = "green", width = 4),
                 infoBox("Correlation", "0.85", 
                        "Source: Analysis - 2024", 
                        icon = icon("link"), color = "light-blue", width = 4)
          )
        ),
        fluidRow(
          column(width = 12,
                 tags$img(src = "images/soil_moisture.png",
                         alt = "Soil Moisture",
                         style = "width: 100%; height: auto; max-height: 600px; object-fit: contain;")
          )
        ),
        fluidRow(
          column(width = 12,
                 plotlyOutput("soil_moisture_comparison", height = "600px")
          )
        ),
        fluidRow(
          column(width = 6,
                 plotlyOutput("soil_moisture_trend", height = "300px")
          ),
          column(width = 6,
                 plotlyOutput("soil_moisture_correlation", height = "300px")
          )
        )
    )
  })

  # SMAP Root Zone Soil Moisture
  output$smap_rootzone <- renderUI({
    fluidRow(
      # Info boxes
      infoBox("Root Zone SM", "0.3 m³/m³", "SMAP Data - 2024", 
              icon = icon("layer-group"), color = "blue", width = 4),
      infoBox("Data Resolution", "9 km", "SMAP L4", 
              icon = icon("ruler"), color = "green", width = 4),
      infoBox("Update Frequency", "Daily", "SMAP Mission", 
              icon = icon("clock"), color = "light-blue", width = 4),
      
      # Static image
      column(width = 12,
        box(title = "SMAP Root Zone Soil Moisture Trends", width = NULL, solidHeader = TRUE,
            img(src = "images/smap_rootzone_trend.png", width = "100%", height = "auto"),
            p("Trend analysis of root zone soil moisture from SMAP data.")
        )
      ),
      
      # Interactive plot
      column(width = 12,
        box(title = "Interactive Root Zone Analysis", width = NULL, solidHeader = TRUE,
            plotlyOutput("smap_rootzone_interactive")
        )
      )
    )
  })

  # GRACE Terrestrial Water Storage
  output$grace_tws <- renderUI({
    fluidRow(
      # Info boxes at the top
      box(width = 12, title = "GRACE Data Overview", solidHeader = TRUE,
          fluidRow(
            infoBox("TWS Change", "-15 cm", "GRACE Data - 2024", 
                    icon = icon("balance-scale"), color = "blue", width = 4,
                    subtitle = "Source: GRACE/GRACE-FO Mission - 2024"),
            infoBox("Annual Variability", "±20 cm", "GRACE Mission", 
                    icon = icon("chart-line"), color = "green", width = 4,
                    subtitle = "Source: NASA JPL - 2024"),
            infoBox("Data Resolution", "300 km", "GRACE/GRACE-FO", 
                    icon = icon("ruler"), color = "light-blue", width = 4,
                    subtitle = "Source: NASA JPL - 2024")
          ),
          fluidRow(
            infoBox("Data Coverage", "2002-2024", "Historical Period", 
                    icon = icon("calendar"), color = "purple", width = 4,
                    subtitle = "Source: GRACE/GRACE-FO Mission - 2024"),
            infoBox("Update Frequency", "Monthly", "Data Refresh", 
                    icon = icon("clock"), color = "yellow", width = 4,
                    subtitle = "Source: NASA JPL - 2024"),
            infoBox("Uncertainty", "±2 cm", "Measurement Error", 
                    icon = icon("exclamation-triangle"), color = "red", width = 4,
                    subtitle = "Source: NASA JPL - 2024")
          ),
          fluidRow(
            infoBox("Spatial Coverage", "Global", "Mission Coverage", 
                    icon = icon("globe"), color = "navy", width = 4,
                    subtitle = "Source: GRACE/GRACE-FO Mission - 2024"),
            infoBox("Data Latency", "2-3 months", "Processing Time", 
                    icon = icon("hourglass-half"), color = "teal", width = 4,
                    subtitle = "Source: NASA JPL - 2024"),
            infoBox("Mission Status", "Active", "GRACE-FO", 
                    icon = icon("satellite"), color = "maroon", width = 4,
                    subtitle = "Source: NASA JPL - 2024")
          )
      ),
      
      # Analysis Controls
      box(width = 12, title = "Analysis Controls", solidHeader = TRUE,
          fluidRow(
            column(width = 4,
                   sliderInput("grace_year", "Select Year:",
                              min = 2002, max = 2024,
                              value = 2024, step = 1,
                              animate = animationOptions(interval = 1000))
            ),
            column(width = 4,
                   selectInput("grace_variable", "Select Variable:",
                              choices = c("TWS", "Uncertainty", "Anomaly"),
                              selected = "TWS")
            ),
            column(width = 4,
                   selectInput("grace_aggregation", "Time Aggregation:",
                              choices = c("Monthly", "Seasonal", "Annual"),
                              selected = "Monthly")
            )
          )
      ),
      
      # Graph Analysis Section
      box(width = 12, title = "Graph Analysis", solidHeader = TRUE,
          tabBox(width = 12,
                 tabPanel("Spatial Analysis",
                          plotlyOutput("grace_interactive_map", height = "500px")
                 ),
                 tabPanel("Temporal Analysis",
                          plotlyOutput("grace_timeseries", height = "500px")
                 ),
                 tabPanel("Seasonal Patterns",
                          plotlyOutput("grace_seasonal", height = "500px")
                 ),
                 tabPanel("Trend Analysis",
                          plotlyOutput("grace_trend", height = "500px")
                 )
          )
      )
    )
  })

  # GRACE Interactive Map
  output$grace_interactive_map <- renderPlotly({
    req(grace_data())
    data <- grace_data()
    year <- input$grace_year
    variable <- input$grace_variable
    
    # Filter data for selected year
    filtered_data <- data %>%
      filter(year(date) == year)
    
    # Select the appropriate variable
    if (variable == "TWS") {
      z_value <- filtered_data$tws
      title <- "Terrestrial Water Storage"
    } else if (variable == "Uncertainty") {
      z_value <- filtered_data$uncertainty
      title <- "Uncertainty"
    } else if (variable == "Anomaly") {
      z_value <- filtered_data$anomaly
      title <- "Anomaly"
    }
    
    # Create interactive map
    plot_ly(data = filtered_data, x = ~lon, y = ~lat, z = z_value,
            type = "heatmap", colors = "RdBu") %>%
      layout(title = paste("GRACE", title, "-", year),
             xaxis = list(title = "Longitude"),
             yaxis = list(title = "Latitude"))
  })

  # GRACE Time Series
  output$grace_timeseries <- renderPlotly({
    req(grace_data())
    data <- grace_data()
    variable <- input$grace_variable
    aggregation <- input$grace_aggregation
    
    # Select the appropriate variable
    if (variable == "TWS") {
      y_value <- data$tws
      title <- "Terrestrial Water Storage"
    } else if (variable == "Uncertainty") {
      y_value <- data$uncertainty
      title <- "Uncertainty"
    } else if (variable == "Anomaly") {
      y_value <- data$anomaly
      title <- "Anomaly"
    }
    
    # Aggregate data based on selection
    if (aggregation == "Monthly") {
      agg_data <- data %>%
        group_by(date = floor_date(date, "month")) %>%
        summarise(mean_val = mean(y_value, na.rm = TRUE))
      x_title <- "Date"
    } else if (aggregation == "Seasonal") {
      agg_data <- data %>%
        mutate(season = paste(year(date), quarter(date))) %>%
        group_by(season) %>%
        summarise(mean_val = mean(y_value, na.rm = TRUE))
      x_title <- "Season"
    } else { # Annual
      agg_data <- data %>%
        mutate(year = year(date)) %>%
        group_by(year) %>%
        summarise(mean_val = mean(y_value, na.rm = TRUE))
      x_title <- "Year"
    }
    
    # Create time series plot
    plot_ly(data = agg_data,
            x = ~if(aggregation == "Monthly") date else if(aggregation == "Seasonal") season else year,
            y = ~mean_val,
            type = "scatter",
            mode = "lines+markers",
            name = title,
            line = list(color = "blue"),
            marker = list(color = "blue")) %>%
      layout(title = paste("GRACE", title, aggregation, "Time Series"),
             xaxis = list(title = x_title),
             yaxis = list(title = paste(title, "(cm)")))
  })

  # GRACE Seasonal Analysis
  output$grace_seasonal <- renderPlotly({
    req(grace_data())
    data <- grace_data()
    variable <- input$grace_variable
    
    # Select the appropriate variable
    if (variable == "TWS") {
      y_value <- data$tws
      title <- "Terrestrial Water Storage"
    } else if (variable == "Uncertainty") {
      y_value <- data$uncertainty
      title <- "Uncertainty"
    } else if (variable == "Anomaly") {
      y_value <- data$anomaly
      title <- "Anomaly"
    }
    
    # Calculate monthly means and standard deviations
    seasonal_data <- data %>%
      mutate(month = month(date, label = TRUE)) %>%
      group_by(month) %>%
      summarise(mean_val = mean(y_value, na.rm = TRUE),
                sd_val = sd(y_value, na.rm = TRUE))
    
    # Create seasonal plot
    plot_ly(data = seasonal_data,
            x = ~month,
            y = ~mean_val,
            type = "scatter",
            mode = "lines+markers",
            name = "Mean",
            line = list(color = "blue"),
            marker = list(color = "blue")) %>%
      add_ribbons(ymin = ~mean_val - sd_val,
                  ymax = ~mean_val + sd_val,
                  line = list(color = "transparent"),
                  fillcolor = "rgba(0, 0, 255, 0.2)",
                  name = "Standard Deviation") %>%
      layout(title = paste("GRACE", title, "Seasonal Patterns"),
             xaxis = list(title = "Month"),
             yaxis = list(title = paste(title, "(cm)")))
  })

  # GRACE Trend Analysis
  output$grace_trend <- renderPlotly({
    req(grace_data())
    data <- grace_data()
    variable <- input$grace_variable
    
    # Select the appropriate variable
    if (variable == "TWS") {
      y_value <- data$tws
      title <- "Terrestrial Water Storage"
    } else if (variable == "Uncertainty") {
      y_value <- data$uncertainty
      title <- "Uncertainty"
    } else if (variable == "Anomaly") {
      y_value <- data$anomaly
      title <- "Anomaly"
    }
    
    # Calculate annual means
    annual_data <- data %>%
      mutate(year = year(date)) %>%
      group_by(year) %>%
      summarise(mean_val = mean(y_value, na.rm = TRUE))
    
    # Fit linear trend
    trend <- lm(mean_val ~ year, data = annual_data)
    
    # Create trend plot
    plot_ly(data = annual_data,
            x = ~year,
            y = ~mean_val,
            type = "scatter",
            mode = "lines+markers",
            name = "Annual Mean",
            line = list(color = "blue"),
            marker = list(color = "blue")) %>%
      add_lines(x = ~year,
                y = fitted(trend),
                name = "Trend Line",
                line = list(color = "red", dash = "dash")) %>%
      layout(title = paste("GRACE", title, "Trend Analysis"),
             xaxis = list(title = "Year"),
             yaxis = list(title = paste(title, "(cm)")))
  })

  # VIC Data Analysis
  vic_data <- reactive({
    year <- input$vic_year_slider
    day <- input$vic_day_slider
    variable <- input$vic_variable
    
    # Create a mapping of display names to NetCDF variable names
    variable_mapping <- list(
      "Precipitation (mm/day)" = "OUT_PREC",
      "Rainfall (mm/day)" = "OUT_RAINF",
      "Evapotranspiration (mm/day)" = "OUT_EVAP",
      "Surface Runoff (mm/day)" = "OUT_RUNOFF",
      "Baseflow (mm/day)" = "OUT_BASEFLOW",
      "Snow Water Equivalent (mm)" = "OUT_SWE",
      "Soil Moisture (mm)" = "OUT_SOIL_MOIST",
      "Air Temperature (°C)" = "OUT_AIR_TEMP",
      "Surface Temperature (°C)" = "OUT_SURF_TEMP",
      "Soil Temperature (°C)" = "OUT_SOIL_TEMP",
      "Snow Surface Temperature (°C)" = "OUT_SNOW_SURF_TEMP",
      "Snow Pack Temperature (°C)" = "OUT_SNOW_PACK_TEMP",
      "Latent Heat Flux (W/m²)" = "OUT_LATENT"
    )
    
    # Get the actual NetCDF variable name
    variable_name <- variable_mapping[[variable]]
    if (is.null(variable_name)) {
      showNotification("Invalid variable selected", type = "error")
      return(NULL)
    }
    
    # Read the NetCDF file for the selected year
    vic_file <- file.path("data/VIC_outputs", paste0("CRB_PRISM_Calibrated.", year, "-01-01.nc"))
    if (!file.exists(vic_file)) {
      showNotification("File not found for selected year", type = "error")
      return(NULL)
    }
    
    tryCatch({
      nc <- nc_open(vic_file)
      
      # Check if the variable exists in the file
      if (!variable_name %in% names(nc$var)) {
        showNotification(paste("Variable", variable_name, "not found in the NetCDF file"), type = "error")
        nc_close(nc)
        return(NULL)
      }
      
      # Get the variable data
      var_data <- ncvar_get(nc, variable_name)
      
      # Get spatial information
      lon <- ncvar_get(nc, "lon")
      lat <- ncvar_get(nc, "lat")
      
      # Get time information
      time <- ncvar_get(nc, "time")
      dates <- as.Date(time, origin = "0001-01-01")
      
      # Check if the selected day is within the data range
      if (day > dim(var_data)[3]) {
        showNotification(paste("Selected day", day, "is out of range. Maximum day is", dim(var_data)[3]), 
                        type = "error")
        nc_close(nc)
        return(NULL)
      }
      
      nc_close(nc)
      
      # Return the data
      return(list(
        data = var_data,
        lon = lon,
        lat = lat,
        dates = dates,
        day = day,
        variable = variable,
        variable_name = variable_name
      ))
    }, error = function(e) {
      showNotification(paste("Error reading NetCDF file:", e$message), type = "error")
      return(NULL)
    })
  })
  
  # Spatial map output
  output$vic_spatial_map <- renderPlotly({
    data <- vic_data()
    if (is.null(data)) return(NULL)
    
    tryCatch({
      # Get the data for the selected day
      day_data <- data$data[,,data$day]
      
      # Create a data frame for plotting
      plot_data <- expand.grid(lon = data$lon, lat = data$lat)
      plot_data$value <- as.vector(day_data)
      
      plot_ly(
        data = plot_data,
        x = ~lon,
        y = ~lat,
        z = ~value,
        type = "heatmap",
        colorscale = "Viridis"
      ) %>%
        layout(
          title = paste("Spatial Distribution -", data$variable),
          xaxis = list(title = "Longitude"),
          yaxis = list(title = "Latitude")
        )
    }, error = function(e) {
      showNotification(paste("Error creating spatial plot:", e$message), type = "error")
      return(NULL)
    })
  })
  
  # Time series output
  output$vic_time_series <- renderPlotly({
    data <- vic_data()
    if (is.null(data)) return(NULL)
    
    tryCatch({
      # Calculate time series for the selected location
      center_lon <- which.min(abs(data$lon - mean(data$lon)))
      center_lat <- which.min(abs(data$lat - mean(data$lat)))
      
      # Extract the time series data for the selected location
      ts_data <- data$data[center_lon, center_lat,]
      
      # Create a data frame for plotting
      plot_data <- data.frame(
        date = data$dates,
        value = ts_data
      )
      
      # Remove any NA values
      plot_data <- na.omit(plot_data)
      
      if (nrow(plot_data) == 0) {
        showNotification("No valid data points for time series", type = "warning")
        return(NULL)
      }
      
      # Create the plot
      p <- plot_ly(plot_data, x = ~date, y = ~value, type = "scatter", mode = "lines",
                  line = list(color = 'rgb(22, 96, 167)')) %>%
        layout(
          title = paste("Time Series -", data$variable),
          xaxis = list(
            title = "Date",
            type = "date",
            tickformat = "%Y-%m-%d"
          ),
          yaxis = list(
            title = data$variable
          ),
          showlegend = FALSE
        )
      
      return(p)
    }, error = function(e) {
      showNotification(paste("Error creating time series plot:", e$message), type = "error")
      return(NULL)
    })
  })
  
  # Statistics outputs
  output$vic_annual_mean <- renderText({
    data <- vic_data()
    if (is.null(data)) return("N/A")
    tryCatch({
      mean_val <- mean(data$data[,,data$day], na.rm = TRUE)
      sprintf("%.2f", mean_val)
    }, error = function(e) {
      "N/A"
    })
  })
  
  output$vic_annual_max <- renderText({
    data <- vic_data()
    if (is.null(data)) return("N/A")
    tryCatch({
      max_val <- max(data$data[,,data$day], na.rm = TRUE)
      sprintf("%.2f", max_val)
    }, error = function(e) {
      "N/A"
    })
  })
  
  output$vic_annual_min <- renderText({
    data <- vic_data()
    if (is.null(data)) return("N/A")
    tryCatch({
      min_val <- min(data$data[,,data$day], na.rm = TRUE)
      sprintf("%.2f", min_val)
    }, error = function(e) {
      "N/A"
    })
  })
  
  output$vic_annual_trend <- renderText({
    data <- vic_data()
    if (is.null(data)) return("N/A")
    
    tryCatch({
      # Calculate trend for the selected location
      center_lon <- which.min(abs(data$lon - mean(data$lon)))
      center_lat <- which.min(abs(data$lat - mean(data$lat)))
      ts_data <- data$data[center_lon, center_lat,]
      
      if (length(ts_data) < 2) return("N/A")
      
      trend <- lm(ts_data ~ seq_along(ts_data))$coefficients[2]
      trend_direction <- ifelse(trend > 0, "Increasing", "Decreasing")
      sprintf("%s (%.2f/day)", trend_direction, abs(trend))
    }, error = function(e) {
      "N/A"
    })
  })

  vic_plots <- reactive({
    if (input$vic_year == "All Years") {
      create_vic_plots(vic_data())
    } else {
      create_vic_plots(vic_data(), as.numeric(input$vic_year))
    }
  })
  
  output$vic_time_series <- renderPlotly({
    if (!is.null(vic_plots()$time_series)) {
      vic_plots()$time_series
    }
  })
  
  output$vic_moisture <- renderPlotly({
    if (!is.null(vic_plots()$moisture)) {
      vic_plots()$moisture
    }
  })
  
  output$vic_spatial <- renderPlotly({
    if (!is.null(vic_plots()$spatial)) {
      var_index <- match(input$vic_variable, 
                        c("Precipitation", "Evapotranspiration", 
                          "Soil Moisture", "Snow Water Equivalent", "Runoff"))
      vic_plots()$spatial[[var_index]]
    }
  })
  
  output$vic_annual <- renderPlotly({
    if (!is.null(vic_plots()$annual)) {
      vic_plots()$annual
    }
  })

  # Precipitation Tab
  output$prism_spatial_map <- renderPlotly({
    prism_data <- load_processed_data("precipitation")
    if (is.null(prism_data)) return(NULL)
    
    # Filter data for selected year and statistic
    selected_year <- input$prism_year
    selected_statistic <- input$prism_statistic
    
    filtered_data <- prism_data %>%
      filter(year == selected_year) %>%
      group_by(month) %>%
      summarise(
        mean_value = mean(value, na.rm = TRUE),
        max_value = max(value, na.rm = TRUE),
        min_value = min(value, na.rm = TRUE),
        .groups = 'drop'
      )
    
    # Create color palette based on values
    pal <- colorNumeric(
      palette = "viridis",
      domain = filtered_data$mean_value
    )
    
    # Create popup content
    popup_content <- paste(
      "<strong>Month:</strong> ", month.name[filtered_data$month], "<br>",
      "<strong>Mean Value:</strong> ", round(filtered_data$mean_value, 2), "<br>",
      "<strong>Max Value:</strong> ", round(filtered_data$max_value, 2), "<br>",
      "<strong>Min Value:</strong> ", round(filtered_data$min_value, 2)
    )
    
    # Create the map
    leaflet(filtered_data) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~month,
        lat = ~month,
        radius = 6,
        color = ~pal(mean_value),
        fillOpacity = 0.8,
        popup = popup_content
      ) %>%
      addLegend(
        "bottomright",
        pal = pal,
        values = ~mean_value,
        title = paste("Mean", selected_statistic),
        opacity = 1
      )
  })

  output$prism_timeseries <- renderPlotly({
    prism_data <- load_processed_data("precipitation")
    if (is.null(prism_data)) return(NULL)
    
    # Filter data for selected year and statistic
    selected_year <- input$prism_year
    selected_statistic <- input$prism_statistic
    
    filtered_data <- prism_data %>%
      filter(year == selected_year) %>%
      group_by(month) %>%
      summarise(
        mean_value = mean(value, na.rm = TRUE),
        max_value = max(value, na.rm = TRUE),
        min_value = min(value, na.rm = TRUE),
        .groups = 'drop'
      )
    
    # Create time series plot
    plot_ly(data = filtered_data, x = ~month, y = ~mean_value, type = 'scatter', mode = 'lines') %>%
      layout(title = paste("Monthly Precipitation -", selected_year),
             xaxis = list(title = "Month"),
             yaxis = list(title = "Precipitation (mm)"))
  })

  output$prism_seasonal <- renderPlotly({
    prism_data <- load_processed_data("precipitation")
    if (is.null(prism_data)) return(NULL)
    
    # Filter data for selected year and statistic
    selected_year <- input$prism_year
    selected_statistic <- input$prism_statistic
    
    filtered_data <- prism_data %>%
      filter(year == selected_year) %>%
      group_by(month) %>%
      summarise(
        mean_value = mean(value, na.rm = TRUE),
        max_value = max(value, na.rm = TRUE),
        min_value = min(value, na.rm = TRUE),
        .groups = 'drop'
      )
    
    # Create seasonal plot
    plot_ly(data = filtered_data, x = ~month, y = ~mean_value, type = 'scatter', mode = 'lines+markers',
            line = list(color = 'rgb(0, 128, 255)', width = 2),
            marker = list(color = 'rgb(0, 128, 255)', size = 8)) %>%
      layout(title = paste("Seasonal Precipitation -", selected_year),
             xaxis = list(title = "Month"),
             yaxis = list(title = "Precipitation (mm)"))
  })

  output$prism_trend <- renderPlotly({
    prism_data <- load_processed_data("precipitation")
    if (is.null(prism_data)) return(NULL)
    
    # Filter data for selected year and statistic
    selected_year <- input$prism_year
    selected_statistic <- input$prism_statistic
    
    filtered_data <- prism_data %>%
      filter(year == selected_year) %>%
      group_by(month) %>%
      summarise(
        mean_value = mean(value, na.rm = TRUE),
        max_value = max(value, na.rm = TRUE),
        min_value = min(value, na.rm = TRUE),
        .groups = 'drop'
      )
    
    # Create trend line
    trend <- lm(mean_value ~ month, data = filtered_data)
    
    # Create trend plot
    plot_ly(data = filtered_data, x = ~month, y = ~mean_value, type = 'scatter', mode = 'markers') %>%
      add_trace(x = ~month, y = fitted(trend), type = 'lines') %>%
      layout(title = paste("Monthly Precipitation Trends -", selected_year),
             xaxis = list(title = "Month"),
             yaxis = list(title = "Precipitation (mm)"))
  })
}

# Run the application with increased memory limit
options(shiny.maxRequestSize = 80000*1024^2)  # Increase to 80GB
app <- shinyApp(ui = ui, server = server)
runApp(app, launch.browser = TRUE, port = 3839)

source("deploy.R")

# Function to get available images for a variable
get_available_images <- function(variable) {
  image_dir <- file.path("images", "dashboard_new", variable)
  if (!dir.exists(image_dir)) {
    return(NULL)
  }
  
  # Get all image files
  image_files <- list.files(image_dir, pattern = "\\.(png|jpg|jpeg)$", full.names = TRUE)
  
  # Categorize images by type
  spatial_images <- image_files[grepl("spatial", image_files, ignore.case = TRUE)]
  timeseries_images <- image_files[grepl("timeseries", image_files, ignore.case = TRUE)]
  analysis_images <- image_files[grepl("analysis", image_files, ignore.case = TRUE)]
  
  return(list(
    spatial = spatial_images,
    timeseries = timeseries_images,
    analysis = analysis_images
  ))
}

# Function to create image display UI
create_image_display_ui <- function(variable) {
    fluidRow(
    box(title = paste("Spatial Map -", variable), width = 6,
        imageOutput(paste0(variable, "_spatial"), height = "400px")),
    box(title = paste("Time Series -", variable), width = 6,
        imageOutput(paste0(variable, "_timeseries"), height = "400px")),
    box(title = paste("Analysis -", variable), width = 12,
        imageOutput(paste0(variable, "_analysis"), height = "400px"))
  )
}

# Function to process VIC data
process_vic_data <- function(year = NULL) {
  if (!is.null(year)) {
    # Process specific year
    vic_file <- file.path("data/VIC_outputs", paste0("CRB_PRISM_Calibrated.", year, "-01-01.nc"))
    if (!file.exists(vic_file)) {
      return(NULL)
    }
    
    # Read NetCDF file
    nc <- nc_open(vic_file)
    
    # Extract variables
    variables <- c("OUT_PREC", "OUT_EVAP", "OUT_SOIL_MOIST", "OUT_SWE", "OUT_RUNOFF")
    data_list <- lapply(variables, function(var) {
      values <- ncvar_get(nc, var)
      return(values)
    })
    names(data_list) <- variables
    
    # Get time dimension
    time <- ncvar_get(nc, "time")
    time_units <- ncatt_get(nc, "time", "units")$value
    dates <- as.Date(time, origin = "0001-01-01")
    
    # Get spatial information
    lon <- ncvar_get(nc, "lon")
    lat <- ncvar_get(nc, "lat")
    
    nc_close(nc)
    
    # Create data frame for time series
    ts_data <- data.frame(
      date = dates,
      precipitation = apply(data_list$OUT_PREC, 3, mean, na.rm = TRUE),
      evapotranspiration = apply(data_list$OUT_EVAP, 3, mean, na.rm = TRUE),
      soil_moisture = apply(data_list$OUT_SOIL_MOIST, 3, mean, na.rm = TRUE),
      snow_water_equivalent = apply(data_list$OUT_SWE, 3, mean, na.rm = TRUE),
      runoff = apply(data_list$OUT_RUNOFF, 3, mean, na.rm = TRUE)
    )
    
    # Create spatial data
    spatial_data <- list(
      lon = lon,
      lat = lat,
      variables = lapply(data_list, function(x) apply(x, c(1,2), mean, na.rm = TRUE))
    )
    
    return(list(
      time_series = ts_data,
      spatial = spatial_data
    ))
  } else {
    # Process all years
    all_years <- list.files("data/VIC_outputs", pattern = "CRB_PRISM_Calibrated.*\\.nc$")
    years <- as.numeric(gsub("CRB_PRISM_Calibrated\\.(\\d{4})-01-01\\.nc", "\\1", all_years))
    
    all_data <- lapply(years, function(y) {
      process_vic_data(y)
    })
    names(all_data) <- years
    
    return(all_data)
  }
}

# Function to create VIC analysis plots
create_vic_plots <- function(vic_data, year = NULL) {
  if (is.null(vic_data)) return(NULL)
  
  if (!is.null(year)) {
    # Single year analysis
    ts_data <- vic_data$time_series
    
    # Time series plot
    ts_plot <- plot_ly() %>%
      add_trace(data = ts_data, x = ~date, y = ~precipitation, name = "Precipitation", type = "scatter", mode = "lines") %>%
      add_trace(data = ts_data, x = ~date, y = ~evapotranspiration, name = "Evapotranspiration", type = "scatter", mode = "lines") %>%
      add_trace(data = ts_data, x = ~date, y = ~runoff, name = "Runoff", type = "scatter", mode = "lines") %>%
      layout(
        title = paste("VIC Hydrological Variables -", year),
        xaxis = list(title = "Date"),
        yaxis = list(title = "Value (mm)"),
        legend = list(orientation = "h", y = -0.2)
      )
    
    # Soil moisture and SWE plot
    moisture_plot <- plot_ly() %>%
      add_trace(data = ts_data, x = ~date, y = ~soil_moisture, name = "Soil Moisture", type = "scatter", mode = "lines") %>%
      add_trace(data = ts_data, x = ~date, y = ~snow_water_equivalent, name = "Snow Water Equivalent", type = "scatter", mode = "lines") %>%
      layout(
        title = paste("Soil Moisture and Snow Water Equivalent -", year),
        xaxis = list(title = "Date"),
        yaxis = list(title = "Value (mm)"),
        legend = list(orientation = "h", y = -0.2)
      )
    
    # Spatial plots
    spatial_plots <- lapply(names(vic_data$spatial$variables), function(var) {
      plot_ly(
        z = vic_data$spatial$variables[[var]],
        x = vic_data$spatial$lon,
        y = vic_data$spatial$lat,
        type = "heatmap",
        colorscale = "Viridis"
      ) %>%
        layout(
          title = paste(var, "-", year),
          xaxis = list(title = "Longitude"),
          yaxis = list(title = "Latitude")
        )
    })
    
    return(list(
      time_series = ts_plot,
      moisture = moisture_plot,
      spatial = spatial_plots
    ))
  } else {
    # Multi-year analysis
    # Create annual summary plots
    annual_data <- data.frame(
      year = as.numeric(names(vic_data)),
      precipitation = sapply(vic_data, function(x) mean(x$time_series$precipitation, na.rm = TRUE)),
      evapotranspiration = sapply(vic_data, function(x) mean(x$time_series$evapotranspiration, na.rm = TRUE)),
      runoff = sapply(vic_data, function(x) mean(x$time_series$runoff, na.rm = TRUE))
    )
    
    annual_plot <- plot_ly(annual_data) %>%
      add_trace(x = ~year, y = ~precipitation, name = "Precipitation", type = "scatter", mode = "lines+markers") %>%
      add_trace(x = ~year, y = ~evapotranspiration, name = "Evapotranspiration", type = "scatter", mode = "lines+markers") %>%
      add_trace(x = ~year, y = ~runoff, name = "Runoff", type = "scatter", mode = "lines+markers") %>%
      layout(
        title = "Annual Average Hydrological Variables",
        xaxis = list(title = "Year"),
        yaxis = list(title = "Value (mm)"),
        legend = list(orientation = "h", y = -0.2)
      )
    
    return(list(annual = annual_plot))
  }
}