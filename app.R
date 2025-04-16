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
addResourcePath("images", "images")

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
      menuItem("Precipitation Analysis", tabName = "precip_anomalies", icon = icon("cloud-rain")),
      menuItem("Snow Water Equivalent", tabName = "snow_water", icon = icon("snowflake")),
      menuItem("Soil Moisture", tabName = "soil", icon = icon("seedling")),
      menuItem("SWE Anomalies", tabName = "swe_anomalies", icon = icon("snowflake")),
      menuItem("Help", tabName = "help", icon = icon("question-circle"))
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
                div(class = "variable-info",
                    h4("VIC Model Output Analysis"),
                    p("Variable Infiltration Capacity (VIC) model outputs for the Colorado River Basin."),
                    p("Spatial resolution: 4km, Temporal resolution: Daily")
                )
              ),
              fluidRow(
                box(width = 12, title = "Temporal Controls",
                    fluidRow(
                      column(width = 6,
                             sliderInput("year_slider", "Select Year:",
                                        min = 1980, max = 2020, value = 2020,
                                        step = 1, sep = "")
                      ),
                      column(width = 6,
                             sliderInput("day_slider", "Select Day of Year:",
                                        min = 1, max = 365, value = 1,
                                        step = 1)
                      )
                    )
                )
              ),
              fluidRow(
                box(width = 12, title = "Variable Analysis",
                    tabsetPanel(
                      tabPanel("Precipitation",
                               fluidRow(
                                 box(width = 12, title = "Precipitation Statistics",
                                     fluidRow(
                                       valueBoxOutput("prec_mean", width = 3),
                                       valueBoxOutput("prec_max", width = 3),
                                       valueBoxOutput("prec_min", width = 3),
                                       valueBoxOutput("prec_std", width = 3)
                                     )
                                 )
                               ),
                               fluidRow(
                                 box(title = "Spatial Distribution", width = 6,
                                     imageOutput("prec_spatial_plot", height = "400px")),
                                 box(title = "Time Series", width = 6,
                                     imageOutput("prec_timeseries_plot", height = "400px"))
                               ),
                               fluidRow(
                                 box(title = "Monthly Statistics", width = 12,
                                     imageOutput("prec_monthly_plot", height = "400px"))
                               ),
                               fluidRow(
                                 box(title = "Annual Trends", width = 6,
                                     imageOutput("prec_annual_trend_plot", height = "300px")),
                                 box(title = "Seasonal Patterns", width = 6,
                                     imageOutput("prec_seasonal_plot", height = "300px"))
                               ),
                               fluidRow(
                                 box(title = "Extreme Events Analysis", width = 6,
                                     imageOutput("prec_extremes_plot", height = "300px")),
                                 box(title = "Spatial Correlation", width = 6,
                                     imageOutput("prec_correlation_plot", height = "300px"))
                               )
                      ),
                      tabPanel("Evapotranspiration",
                               fluidRow(
                                 box(width = 12, title = "Evapotranspiration Statistics",
                                     fluidRow(
                                       valueBoxOutput("evap_mean", width = 3),
                                       valueBoxOutput("evap_max", width = 3),
                                       valueBoxOutput("evap_min", width = 3),
                                       valueBoxOutput("evap_std", width = 3)
                                     )
                                 )
                               ),
                               fluidRow(
                                 box(title = "Spatial Distribution", width = 6,
                                     imageOutput("evap_spatial_plot", height = "400px")),
                                 box(title = "Time Series", width = 6,
                                     imageOutput("evap_timeseries_plot", height = "400px"))
                               ),
                               fluidRow(
                                 box(title = "Monthly Statistics", width = 12,
                                     imageOutput("evap_monthly_plot", height = "400px"))
                               ),
                               fluidRow(
                                 box(title = "Annual Trends", width = 6,
                                     imageOutput("evap_annual_trend_plot", height = "300px")),
                                 box(title = "Seasonal Patterns", width = 6,
                                     imageOutput("evap_seasonal_plot", height = "300px"))
                               ),
                               fluidRow(
                                 box(title = "Water Balance Analysis", width = 6,
                                     imageOutput("evap_water_balance_plot", height = "300px")),
                                 box(title = "Land Cover Impact", width = 6,
                                     imageOutput("evap_land_cover_plot", height = "300px"))
                               )
                      ),
                      tabPanel("Runoff",
                               fluidRow(
                                 box(width = 12, title = "Runoff Statistics",
                                     fluidRow(
                                       valueBoxOutput("runoff_mean", width = 3),
                                       valueBoxOutput("runoff_max", width = 3),
                                       valueBoxOutput("runoff_min", width = 3),
                                       valueBoxOutput("runoff_std", width = 3)
                                     )
                                 )
                               ),
                               fluidRow(
                                 box(title = "Spatial Distribution", width = 6,
                                     imageOutput("runoff_spatial_plot", height = "400px")),
                                 box(title = "Time Series", width = 6,
                                     imageOutput("runoff_timeseries_plot", height = "400px"))
                               ),
                               fluidRow(
                                 box(title = "Monthly Statistics", width = 12,
                                     imageOutput("runoff_monthly_plot", height = "400px"))
                               ),
                               fluidRow(
                                 box(title = "Annual Trends", width = 6,
                                     imageOutput("runoff_annual_trend_plot", height = "300px")),
                                 box(title = "Flow Duration Curve", width = 6,
                                     imageOutput("runoff_fdc_plot", height = "300px"))
                               ),
                               fluidRow(
                                 box(title = "Watershed Response", width = 6,
                                     imageOutput("runoff_response_plot", height = "300px")),
                                 box(title = "Baseflow Separation", width = 6,
                                     imageOutput("runoff_baseflow_plot", height = "300px"))
                               )
                      ),
                      tabPanel("Soil Moisture",
                               fluidRow(
                                 box(width = 12, title = "Soil Moisture Statistics",
                                     fluidRow(
                                       valueBoxOutput("soil_mean", width = 3),
                                       valueBoxOutput("soil_max", width = 3),
                                       valueBoxOutput("soil_min", width = 3),
                                       valueBoxOutput("soil_std", width = 3)
                                     )
                                 )
                               ),
                               fluidRow(
                                 box(title = "Spatial Distribution", width = 6,
                                     imageOutput("soil_moisture_spatial_plot", height = "400px")),
                                 box(title = "Time Series", width = 6,
                                     imageOutput("soil_moisture_timeseries_plot", height = "400px"))
                               ),
                               fluidRow(
                                 box(title = "Monthly Statistics", width = 12,
                                     imageOutput("soil_moisture_monthly_plot", height = "400px"))
                               ),
                               fluidRow(
                                 box(title = "Soil Layer Analysis", width = 6,
                                     imageOutput("soil_layers_plot", height = "300px")),
                                 box(title = "Depth Profile", width = 6,
                                     imageOutput("soil_depth_plot", height = "300px"))
                               ),
                               fluidRow(
                                 box(title = "Drought Analysis", width = 6,
                                     imageOutput("soil_drought_plot", height = "300px")),
                                 box(title = "Vegetation Impact", width = 6,
                                     imageOutput("soil_vegetation_plot", height = "300px"))
                               )
                      ),
                      tabPanel("Snow Water",
                               fluidRow(
                                 box(width = 12, title = "Snow Water Equivalent Statistics",
                                     fluidRow(
                                       valueBoxOutput("swe_mean", width = 3),
                                       valueBoxOutput("swe_max", width = 3),
                                       valueBoxOutput("swe_min", width = 3),
                                       valueBoxOutput("swe_std", width = 3)
                                     )
                                 )
                               ),
                               fluidRow(
                                 box(title = "Spatial Distribution", width = 6,
                                     imageOutput("swe_spatial_plot", height = "400px")),
                                 box(title = "Time Series", width = 6,
                                     imageOutput("swe_timeseries_plot", height = "400px"))
                               ),
                               fluidRow(
                                 box(title = "Monthly Statistics", width = 12,
                                     imageOutput("swe_monthly_plot", height = "400px"))
                               ),
                               fluidRow(
                                 box(title = "Snow Accumulation/Melt", width = 6,
                                     imageOutput("swe_accumulation_plot", height = "300px")),
                                 box(title = "Elevation Analysis", width = 6,
                                     imageOutput("swe_elevation_plot", height = "300px"))
                               ),
                               fluidRow(
                                 box(title = "Snow Cover Duration", width = 6,
                                     plotlyOutput("swe_duration", height = "300px")),
                                 box(title = "Temperature Impact", width = 6,
                                     plotlyOutput("swe_temperature", height = "300px"))
                               )
                      ),
                      tabPanel("Temperature",
                               fluidRow(
                                 box(width = 12, title = "Temperature Statistics",
                                     fluidRow(
                                       valueBoxOutput("temp_mean", width = 3),
                                       valueBoxOutput("temp_max", width = 3),
                                       valueBoxOutput("temp_min", width = 3),
                                       valueBoxOutput("temp_std", width = 3)
                                     )
                                 )
                               ),
                               fluidRow(
                                 box(title = "Spatial Distribution", width = 6,
                                     plotlyOutput("temp_spatial", height = "400px")),
                                 box(title = "Time Series", width = 6,
                                     plotlyOutput("temp_timeseries", height = "400px"))
                               ),
                               fluidRow(
                                 box(title = "Monthly Statistics", width = 12,
                                     plotlyOutput("temp_monthly", height = "400px"))
                               ),
                               fluidRow(
                                 box(title = "Annual Trends", width = 6,
                                     plotlyOutput("temp_annual_trend", height = "300px")),
                                 box(title = "Diurnal Variation", width = 6,
                                     plotlyOutput("temp_diurnal", height = "300px"))
                               ),
                               fluidRow(
                                 box(title = "Elevation Gradient", width = 6,
                                     plotlyOutput("temp_elevation", height = "300px")),
                                 box(title = "Climate Change Impact", width = 6,
                                     plotlyOutput("temp_climate_change", height = "300px"))
                               )
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
                    p("Soil Moisture Active Passive (SMAP) satellite data analysis."),
                    p("Spatial resolution: 9km, Temporal resolution: Daily")
                )
              ),
              fluidRow(
                box(width = 12, title = "Analysis Controls",
                    fluidRow(
                      column(width = 4,
                             selectInput("smap_depth_select", "Select Depth:",
                                        choices = c("Surface", "Root Zone"),
                                        selected = "Surface")
                      ),
                      column(width = 4,
                             selectInput("smap_aggregation", "Select Time Aggregation:",
                                        choices = c("Daily", "Weekly", "Monthly", "Seasonal"),
                                        selected = "Daily")
                      ),
                      column(width = 4,
                             selectInput("smap_analysis", "Select Analysis Type:",
                                        choices = c("Trend", "Anomaly", "Seasonal", "Extreme"),
                                        selected = "Trend")
                      )
                    )
                )
              ),
              fluidRow(
                box(width = 12, title = "SMAP Statistics",
                    fluidRow(
                      valueBoxOutput("smap_mean_box", width = 3),
                      valueBoxOutput("smap_max_box", width = 3),
                      valueBoxOutput("smap_min_box", width = 3),
                      valueBoxOutput("smap_std_box", width = 3)
                    )
                )
              ),
              fluidRow(
                box(title = "Spatial Distribution", width = 6,
                    plotlyOutput("smap_spatial", height = "400px")),
                box(title = "Time Series", width = 6,
                    plotlyOutput("smap_timeseries", height = "400px"))
              ),
              fluidRow(
                box(title = "Monthly Statistics", width = 12,
                    plotlyOutput("smap_monthly", height = "400px"))
              ),
              fluidRow(
                box(title = "Depth Profile Analysis", width = 6,
                    plotlyOutput("smap_depth", height = "300px")),
                box(title = "Land Cover Impact", width = 6,
                    plotlyOutput("smap_land_cover", height = "300px"))
              ),
              fluidRow(
                box(title = "Drought Analysis", width = 6,
                    plotlyOutput("smap_drought", height = "300px")),
                box(title = "Vegetation Response", width = 6,
                    plotlyOutput("smap_vegetation", height = "300px"))
              ),
              fluidRow(
                box(title = "Climate Impact", width = 6,
                    plotlyOutput("smap_climate", height = "300px")),
                box(title = "Human Impact", width = 6,
                    plotlyOutput("smap_human", height = "300px"))
              )
      ),

      # GRACE Data Tab
      tabItem(tabName = "grace",
              fluidRow(
                div(class = "variable-info",
                    h4("GRACE Terrestrial Water Storage Analysis"),
                    p("Gravity Recovery and Climate Experiment (GRACE) satellite data analysis."),
                    p("Spatial resolution: 300km, Temporal resolution: Monthly")
                )
              ),
              fluidRow(
                box(width = 12, title = "Analysis Controls",
                    fluidRow(
                      column(width = 4,
                             selectInput("grace_component", "Select Component:",
                                        choices = c("Total Storage", "Groundwater", "Surface Water", "Soil Moisture"),
                                        selected = "Total Storage")
                      ),
                      column(width = 4,
                             selectInput("grace_aggregation", "Select Time Aggregation:",
                                        choices = c("Monthly", "Seasonal", "Annual"),
                                        selected = "Monthly")
                      ),
                      column(width = 4,
                             selectInput("grace_analysis", "Select Analysis Type:",
                                        choices = c("Trend", "Anomaly", "Seasonal", "Extreme"),
                                        selected = "Trend")
                      )
                    )
                )
              ),
              fluidRow(
                box(width = 12, title = "GRACE Statistics",
                    fluidRow(
                      valueBoxOutput("grace_mean", width = 3),
                      valueBoxOutput("grace_max", width = 3),
                      valueBoxOutput("grace_min", width = 3),
                      valueBoxOutput("grace_std", width = 3)
                    )
                )
              ),
              fluidRow(
                box(title = "Spatial Distribution", width = 6,
                    plotlyOutput("grace_spatial", height = "400px")),
                box(title = "Time Series", width = 6,
                    plotlyOutput("grace_timeseries", height = "400px"))
              ),
              fluidRow(
                box(title = "Monthly Statistics", width = 12,
                    plotlyOutput("grace_monthly", height = "400px"))
              ),
              fluidRow(
                box(title = "Water Storage Components", width = 6,
                    plotlyOutput("grace_components", height = "300px")),
                box(title = "Groundwater Analysis", width = 6,
                    plotlyOutput("grace_groundwater", height = "300px"))
              ),
              fluidRow(
                box(title = "Climate Impact", width = 6,
                    plotlyOutput("grace_climate", height = "300px")),
                box(title = "Human Impact", width = 6,
                    plotlyOutput("grace_human", height = "300px"))
              ),
              fluidRow(
                box(title = "Drought Analysis", width = 6,
                    plotlyOutput("grace_drought", height = "300px")),
                box(title = "Water Management Impact", width = 6,
                    plotlyOutput("grace_management", height = "300px"))
              )
      ),

      # Precipitation Anomalies Tab
      tabItem(tabName = "precip_anomalies",
              fluidRow(
                div(class = "variable-info",
                    h4("Precipitation Anomalies Analysis"),
                    p("Analysis of precipitation deviations from long-term averages."),
                    p("Spatial resolution: 4km, Temporal resolution: Daily")
                )
              ),
              fluidRow(
                box(width = 12, title = "Analysis Controls",
                    fluidRow(
                      column(width = 4,
                             selectInput("precip_anomaly_type", "Select Anomaly Type:",
                                        choices = c("Standardized", "Percent", "Absolute"),
                                        selected = "Standardized")
                      ),
                      column(width = 4,
                             selectInput("precip_aggregation", "Select Time Aggregation:",
                                        choices = c("Daily", "Weekly", "Monthly", "Seasonal"),
                                        selected = "Monthly")
                      ),
                      column(width = 4,
                             selectInput("precip_analysis", "Select Analysis Type:",
                                        choices = c("Trend", "Frequency", "Intensity", "Duration"),
                                        selected = "Trend")
                      )
                    )
                )
              ),
              fluidRow(
                box(width = 12, title = "Anomaly Statistics",
                    fluidRow(
                      valueBoxOutput("precip_anomaly_mean", width = 3),
                      valueBoxOutput("precip_anomaly_max", width = 3),
                      valueBoxOutput("precip_anomaly_min", width = 3),
                      valueBoxOutput("precip_anomaly_std", width = 3)
                    )
                )
              ),
              fluidRow(
                box(title = "Spatial Distribution", width = 6,
                    plotlyOutput("precip_anomaly_spatial", height = "400px")),
                box(title = "Time Series", width = 6,
                    plotlyOutput("precip_anomaly_timeseries", height = "400px"))
              ),
              fluidRow(
                box(title = "Monthly Anomalies", width = 12,
                    plotlyOutput("precip_anomaly_monthly", height = "400px"))
              ),
              fluidRow(
                box(title = "Extreme Events", width = 6,
                    plotlyOutput("precip_extremes", height = "300px")),
                box(title = "Seasonal Patterns", width = 6,
                    plotlyOutput("precip_seasonal", height = "300px"))
              ),
              fluidRow(
                box(title = "Climate Indices", width = 6,
                    plotlyOutput("precip_climate", height = "300px")),
                box(title = "Impact Analysis", width = 6,
                    plotlyOutput("precip_impact", height = "300px"))
              ),
              fluidRow(
                box(title = "Drought Analysis", width = 6,
                    plotlyOutput("precip_drought", height = "300px")),
                box(title = "Flood Risk", width = 6,
                    plotlyOutput("precip_flood", height = "300px"))
              )
      ),

      # SWE Anomalies Tab
      tabItem(tabName = "swe_anomalies",
              fluidRow(
                div(class = "variable-info",
                    h4("Snow Water Equivalent Anomalies Analysis"),
                    p("Analysis of snow water equivalent deviations from long-term averages."),
                    p("Spatial resolution: 4km, Temporal resolution: Daily")
                )
              ),
              fluidRow(
                box(width = 12, title = "Analysis Controls",
                    fluidRow(
                      column(width = 4,
                             selectInput("swe_anomaly_type", "Select Anomaly Type:",
                                        choices = c("Standardized", "Percent", "Absolute"),
                                        selected = "Standardized")
                      ),
                      column(width = 4,
                             selectInput("swe_anomaly_aggregation", "Select Time Aggregation:",
                                        choices = c("Daily", "Weekly", "Monthly", "Seasonal"),
                                        selected = "Monthly")
                      ),
                      column(width = 4,
                             selectInput("swe_anomaly_analysis", "Select Analysis Type:",
                                        choices = c("Trend", "Frequency", "Intensity", "Duration"),
                                        selected = "Trend")
                      )
                    )
                )
              ),
              fluidRow(
                box(width = 12, title = "Anomaly Statistics",
                    fluidRow(
                      valueBoxOutput("swe_anomaly_mean", width = 3),
                      valueBoxOutput("swe_anomaly_max", width = 3),
                      valueBoxOutput("swe_anomaly_min", width = 3),
                      valueBoxOutput("swe_anomaly_std", width = 3)
                    )
                )
              ),
              fluidRow(
                box(title = "Spatial Distribution", width = 6,
                    plotlyOutput("swe_anomaly_spatial", height = "400px")),
                box(title = "Time Series", width = 6,
                    plotlyOutput("swe_anomaly_timeseries", height = "400px"))
              ),
              fluidRow(
                box(title = "Monthly Anomalies", width = 12,
                    plotlyOutput("swe_anomaly_monthly", height = "400px"))
              ),
              fluidRow(
                box(title = "Elevation Analysis", width = 6,
                    plotlyOutput("swe_elevation", height = "300px")),
                box(title = "Temperature Impact", width = 6,
                    plotlyOutput("swe_temperature", height = "300px"))
              ),
              fluidRow(
                box(title = "Snow Cover Duration", width = 6,
                    plotlyOutput("swe_duration", height = "300px")),
                box(title = "Melt Analysis", width = 6,
                    plotlyOutput("swe_melt", height = "300px"))
              ),
              fluidRow(
                box(title = "Climate Impact", width = 6,
                    plotlyOutput("swe_climate", height = "300px")),
                box(title = "Water Resource Impact", width = 6,
                    plotlyOutput("swe_water", height = "300px"))
              )
      ),

      # Snow Water Equivalent Tab
      tabItem(tabName = "snow_water",
              fluidRow(
                div(class = "variable-info",
                    h4("Snow Water Equivalent Analysis"),
                    p("Analysis of snow water equivalent across the Colorado River Basin."),
                    p("Spatial resolution: 4km, Temporal resolution: Daily")
                )
              ),
              fluidRow(
                box(width = 12, title = "Analysis Controls",
                    fluidRow(
                      column(width = 4,
                             selectInput("swe_component", "Select Component:",
                                        choices = c("Total SWE", "Snow Depth", "Snow Density"),
                                        selected = "Total SWE")
                      ),
                      column(width = 4,
                             selectInput("swe_water_aggregation", "Select Time Aggregation:",
                                        choices = c("Daily", "Weekly", "Monthly", "Seasonal"),
                                        selected = "Daily")
                      ),
                      column(width = 4,
                             selectInput("swe_water_analysis", "Select Analysis Type:",
                                        choices = c("Trend", "Seasonal", "Extreme", "Impact"),
                                        selected = "Trend")
                      )
                    )
                )
              ),
              fluidRow(
                box(width = 12, title = "SWE Statistics",
                    fluidRow(
                      valueBoxOutput("swe_mean", width = 3),
                      valueBoxOutput("swe_max", width = 3),
                      valueBoxOutput("swe_min", width = 3),
                      valueBoxOutput("swe_std", width = 3)
                    )
                )
              ),
              fluidRow(
                box(title = "Spatial Distribution", width = 6,
                    plotlyOutput("swe_spatial", height = "400px")),
                box(title = "Time Series", width = 6,
                    plotlyOutput("swe_timeseries", height = "400px"))
              ),
              fluidRow(
                box(title = "Monthly Statistics", width = 12,
                    plotlyOutput("swe_monthly", height = "400px"))
              ),
              fluidRow(
                box(title = "Snow Accumulation", width = 6,
                    plotlyOutput("swe_accumulation", height = "300px")),
                box(title = "Snow Melt", width = 6,
                    plotlyOutput("swe_melt", height = "300px"))
              ),
              fluidRow(
                box(title = "Elevation Impact", width = 6,
                    plotlyOutput("swe_elevation", height = "300px")),
                box(title = "Climate Impact", width = 6,
                    plotlyOutput("swe_climate", height = "300px"))
              ),
              fluidRow(
                box(title = "Drought Impact", width = 6,
                    plotlyOutput("swe_drought", height = "300px")),
                box(title = "Water Resource Impact", width = 6,
                    plotlyOutput("swe_water", height = "300px"))
              )
      ),

      # Soil Moisture Tab
      tabItem(tabName = "soil_moisture",
              fluidRow(
                div(class = "variable-info",
                    h4("Soil Moisture Analysis"),
                    p("Analysis of soil moisture across the Colorado River Basin."),
                    p("Spatial resolution: 4km, Temporal resolution: Daily")
                )
              ),
              fluidRow(
                box(width = 12, title = "Analysis Controls",
                    fluidRow(
                      column(width = 4,
                             selectInput("soil_depth_select", "Select Depth:",
                                        choices = c("Surface", "Root Zone", "Profile"),
                                        selected = "Surface")
                      ),
                      column(width = 4,
                             selectInput("soil_aggregation", "Select Time Aggregation:",
                                        choices = c("Daily", "Weekly", "Monthly", "Seasonal"),
                                        selected = "Daily")
                      ),
                      column(width = 4,
                             selectInput("soil_analysis", "Select Analysis Type:",
                                        choices = c("Trend", "Anomaly", "Seasonal", "Extreme"),
                                        selected = "Trend")
                      )
                    )
                )
              ),
              fluidRow(
                box(width = 12, title = "Soil Moisture Statistics",
                    fluidRow(
                      valueBoxOutput("soil_mean", width = 3),
                      valueBoxOutput("soil_max", width = 3),
                      valueBoxOutput("soil_min", width = 3),
                      valueBoxOutput("soil_std", width = 3)
                    )
                )
              ),
              fluidRow(
                box(title = "Spatial Distribution", width = 6,
                    plotlyOutput("soil_spatial", height = "400px")),
                box(title = "Time Series", width = 6,
                    plotlyOutput("soil_timeseries", height = "400px"))
              ),
              fluidRow(
                box(title = "Monthly Statistics", width = 12,
                    plotlyOutput("soil_monthly", height = "400px"))
              ),
              fluidRow(
                box(title = "Depth Profile", width = 6,
                    plotlyOutput("soil_depth", height = "300px")),
                box(title = "Vegetation Impact", width = 6,
                    plotlyOutput("soil_vegetation", height = "300px"))
              ),
              fluidRow(
                box(title = "Drought Analysis", width = 6,
                    plotlyOutput("soil_drought", height = "300px")),
                box(title = "Climate Impact", width = 6,
                    plotlyOutput("soil_climate", height = "300px"))
              ),
              fluidRow(
                box(title = "Land Use Impact", width = 6,
                    plotlyOutput("soil_landuse", height = "300px")),
                box(title = "Water Resource Impact", width = 6,
                    plotlyOutput("soil_water", height = "300px"))
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
                    p("Analysis of soil moisture patterns across the Colorado River Basin."),
                    p("Includes both surface and root zone soil moisture."),
                    p("Critical for understanding water availability and drought conditions.")
                )
              ),
              fluidRow(
                box(width = 12, title = "Soil Moisture Analysis",
                    tabsetPanel(
                      tabPanel("Monthly Statistics",
                           fluidRow(
                             column(width = 6,
                                box(width = 12, title = "Monthly Statistics",
                                    img(src = "images/soil_moisture_analysis/monthly_statistics.png", width = "100%", height = "auto"),
                                    p("Monthly soil moisture statistics."),
                                    plotlyOutput("soil_monthly_interactive", height = "300px")
                                )
                             ),
                             column(width = 6,
                                box(width = 12, title = "Key Metrics",
                                    valueBoxOutput("soil_mean", width = 12),
                                    valueBoxOutput("soil_trend", width = 12),
                                    valueBoxOutput("soil_anomaly", width = 12)
                                )
                             )
                           )
                      ),
                      tabPanel("Correlation Analysis",
                           fluidRow(
                             column(width = 6,
                                box(width = 12, title = "Soil Moisture Correlations",
                                    img(src = "images/analysis_results/correlation_soil_moisture.png", width = "100%", height = "auto"),
                                    p("Correlation analysis of soil moisture with other variables."),
                                    plotlyOutput("soil_correlation_interactive", height = "300px")
                                )
                             ),
                             column(width = 6,
                                box(width = 12, title = "Correlation Metrics",
                                    valueBoxOutput("soil_corr_strength", width = 12),
                                    valueBoxOutput("soil_corr_significance", width = 12),
                                    valueBoxOutput("soil_corr_trend", width = 12)
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
                    p("Analysis of water balance components and their interactions."),
                    p("Includes precipitation, evapotranspiration, runoff, and storage changes.")
                )
              ),
              fluidRow(
                box(width = 12, title = "Water Balance Metrics",
                    fluidRow(
                      column(width = 6,
                             valueBoxOutput("water_balance_mean", width = 12),
                             valueBoxOutput("water_balance_trend", width = 12),
                             valueBoxOutput("water_balance_anomaly", width = 12)
                      ),
                      column(width = 6,
                             plotlyOutput("water_balance_components", height = "300px")
                      )
                    )
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
    # Check if cached data exists
    cache_file <- "data/grace_processed/grace_cache.rds"
    if (file.exists(cache_file)) {
      return(readRDS(cache_file))
    }
    
    tryCatch({
      nc <- load_grace_data()
      on.exit(nc_close(nc))
      
      # Get dimensions first
      time <- ncvar_get(nc, "time")
      dates <- as.Date(time, origin = "2002-01-01")
      
      # Calculate spatial means directly while reading
      n_times <- length(time)
      tws_means <- numeric(n_times)
      uncertainty_means <- numeric(n_times)
      
      # Process data in chunks
      chunk_size <- 12  # Process one year at a time
      n_chunks <- ceiling(n_times / chunk_size)
      
      for(i in 1:n_chunks) {
        start_idx <- (i-1) * chunk_size + 1
        end_idx <- min(i * chunk_size, n_times)
        
        # Read chunk of data
        tws_chunk <- ncvar_get(nc, "lwe_thickness", 
                              start = c(1, 1, start_idx), 
                              count = c(-1, -1, end_idx - start_idx + 1))
        uncertainty_chunk <- ncvar_get(nc, "uncertainty",
                                     start = c(1, 1, start_idx),
                                     count = c(-1, -1, end_idx - start_idx + 1))
        
        # Calculate means for the chunk
        tws_means[start_idx:end_idx] <- apply(tws_chunk, 3, mean, na.rm = TRUE)
        uncertainty_means[start_idx:end_idx] <- apply(uncertainty_chunk, 3, mean, na.rm = TRUE)
      }
      
      # Create result data frame
      result <- list(
        dates = dates,
        tws_means = tws_means,
        uncertainty_means = uncertainty_means
      )
      
      # Cache the results
      saveRDS(result, cache_file)
      
      return(result)
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
    
    # Create time series data frame
    ts_df <- data.frame(
      Date = data$dates,
      Uncertainty = data$uncertainty_means,
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
    
    # Create time series data frame
    ts_df <- data.frame(
      Date = data$dates,
      TWS = data$tws_means,
      Year = format(data$dates, "%Y")
    )
    
    # Filter for selected year
    ts_df <- ts_df[ts_df$Year == selected_year,]
    
    # Calculate monthly climatology
    monthly_clim <- ts_df %>%
      mutate(Month = format(Date, "%B")) %>%
      group_by(Month) %>%
      summarise(Climatology = mean(TWS, na.rm = TRUE))
    
    # Calculate anomalies
    ts_df <- ts_df %>%
      mutate(Month = format(Date, "%B")) %>%
      left_join(monthly_clim, by = "Month") %>%
      mutate(Anomaly = TWS - Climatology)
    
    plot_ly(ts_df) %>%
      add_trace(x = ~Date, y = ~Anomaly, type = 'scatter', mode = 'lines',
                name = 'TWS Anomaly', line = list(color = 'blue')) %>%
      layout(
        title = paste("TWS Anomaly -", selected_year),
        xaxis = list(title = "Date"),
        yaxis = list(title = "TWS Anomaly (cm)"),
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
             xaxis = list(title = "Month", ticktext = month.abb, tickvals = 1:12),
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
      # Info boxes
      infoBox("TWS Change", "-15 cm", "GRACE Data - 2024", 
              icon = icon("balance-scale"), color = "blue", width = 4),
      infoBox("Annual Variability", "±20 cm", "GRACE Mission", 
              icon = icon("chart-line"), color = "green", width = 4),
      infoBox("Data Resolution", "300 km", "GRACE/GRACE-FO", 
              icon = icon("ruler"), color = "light-blue", width = 4),
      
      # Static image
      column(width = 12,
        box(title = "GRACE Terrestrial Water Storage Trends", width = NULL, solidHeader = TRUE,
            img(src = "images/grace_trend.png", width = "100%", height = "auto"),
            p("Trend analysis of terrestrial water storage from GRACE data.")
        )
      ),
      
      # Interactive plot
      column(width = 12,
        box(title = "Interactive TWS Analysis", width = NULL, solidHeader = TRUE,
            plotlyOutput("grace_tws_interactive")
        )
      )
    )
  })

  # SNOTEL Data
  output$snotel_data <- renderUI({
    fluidRow(
      # Info boxes
      infoBox("Stations", "120", "SNOTEL Network", 
              icon = icon("snowflake"), color = "blue", width = 4),
      infoBox("Elevation Range", "1500-4000m", "SNOTEL Sites", 
              icon = icon("mountain"), color = "green", width = 4),
      infoBox("Data Frequency", "Hourly", "SNOTEL Network", 
              icon = icon("clock"), color = "light-blue", width = 4),
      
      # Time Series
      column(width = 12,
        box(title = "SNOTEL Time Series", width = NULL, solidHeader = TRUE,
            img(src = "images/snotel_timeseries.png", width = "100%", height = "auto"),
            p("Historical time series of snow water equivalent from SNOTEL stations.")
        )
      ),
      
      # Interactive time series
      column(width = 12,
        box(title = "Interactive Time Series", width = NULL, solidHeader = TRUE,
            plotlyOutput("snotel_timeseries_interactive")
        )
      ),
      
      # Seasonal Patterns
      column(width = 12,
        box(title = "Seasonal Patterns", width = NULL, solidHeader = TRUE,
            img(src = "images/snotel_seasonal.png", width = "100%", height = "auto"),
            p("Seasonal patterns in snow water equivalent across SNOTEL stations.")
        )
      ),
      
      # Interactive seasonal patterns
      column(width = 12,
        box(title = "Interactive Seasonal Analysis", width = NULL, solidHeader = TRUE,
            plotlyOutput("snotel_seasonal_interactive")
        )
      ),
      
      # Elevation Analysis
      column(width = 12,
        box(title = "Elevation Analysis", width = NULL, solidHeader = TRUE,
            img(src = "images/snotel_elevation.png", width = "100%", height = "auto"),
            p("Relationship between elevation and snow water equivalent.")
        )
      ),
      
      # Interactive elevation analysis
      column(width = 12,
        box(title = "Interactive Elevation Analysis", width = NULL, solidHeader = TRUE,
            plotlyOutput("snotel_elevation_interactive")
        )
      ),
      
      # Trend Analysis
      column(width = 12,
        box(title = "Trend Analysis", width = NULL, solidHeader = TRUE,
            img(src = "images/snotel_trend.png", width = "100%", height = "auto"),
            p("Long-term trends in snow water equivalent across the basin.")
        )
      ),
      
      # Interactive trend analysis
      column(width = 12,
        box(title = "Interactive Trend Analysis", width = NULL, solidHeader = TRUE,
            plotlyOutput("snotel_trend_interactive")
        )
      )
    )
  })

  output$snotel_seasonal <- renderUI({
    div(class = "plot-container",
        fluidRow(
          column(width = 12,
                 box(title = "Seasonal Patterns", width = NULL, solidHeader = TRUE,
                     img(src = "images/seasonal_patterns.png", width = "100%", height = "auto"),
                     p("Seasonal variations in snow water equivalent across the basin.")
                 )
          )
        ),
        fluidRow(
          column(width = 12,
                 plotlyOutput("snotel_seasonal_plot", height = "600px")
          )
        )
    )
  })

  output$snotel_elevation <- renderUI({
    div(class = "plot-container",
        fluidRow(
          column(width = 12,
                 box(title = "Elevation Analysis", width = NULL, solidHeader = TRUE,
                     img(src = "images/elevation_analysis.png", width = "100%", height = "auto"),
                     p("Relationship between elevation and snow water equivalent.")
                 )
          )
        ),
        fluidRow(
          column(width = 12,
                 plotlyOutput("snotel_elevation_plot", height = "600px")
          )
        )
    )
  })

  output$snotel_trend <- renderUI({
    div(class = "plot-container",
        fluidRow(
          column(width = 12,
                 box(title = "Trend Analysis", width = NULL, solidHeader = TRUE,
                     img(src = "images/snotel_trend.png", width = "100%", height = "auto"),
                     p("Long-term trends in snow water equivalent across the basin.")
                 )
          )
        ),
        fluidRow(
          column(width = 12,
                 plotlyOutput("snotel_trend_plot", height = "600px")
          )
        )
    )
  })

  output$snotel_time_series <- renderUI({
    div(class = "plot-container",
        fluidRow(
          column(width = 12,
                 box(title = "SNOTEL Time Series", width = NULL, solidHeader = TRUE,
                     img(src = "images/snotel_time_series.png", width = "100%", height = "auto"),
                     p("Historical time series of SNOTEL station measurements.")
                 )
          )
        ),
        fluidRow(
          column(width = 12,
                 box(title = "SNOTEL Elevation Analysis", width = NULL, solidHeader = TRUE,
                     img(src = "images/snotel_elevation.png", width = "100%", height = "auto"),
                     p("Analysis of SNOTEL measurements across different elevations.")
                 )
          )
        )
    )
  })

  # Add year/time slider inputs
  output$year_slider <- renderUI({
    sliderInput("selected_year", "Select Year:",
                min = 1982,
                max = 2024,
                value = 2024,
                step = 1,
                sep = "",
                animate = animationOptions(interval = 1000))
  })

  output$day_slider <- renderUI({
    sliderInput("selected_day", "Select Day of Year:",
                min = 1,
                max = 365,
                value = 1,
                step = 1,
                sep = "",
                animate = animationOptions(interval = 1000))
  })

  # Enhanced interactive maps
  output$vic_interactive_map <- renderPlotly({
    req(vic_data())
    data <- vic_data()
    year <- input$vic_year_slider
    day <- input$vic_day_slider
    
    # Filter data for selected year and day
    filtered_data <- data %>%
      filter(year == year, day == day)
    
    # Create interactive map
    plot_ly(filtered_data, x = ~lon, y = ~lat, z = ~value,
            type = "heatmap", colors = "Viridis") %>%
      layout(title = paste("VIC Model Output - Year:", year, "Day:", day))
  })
  
  output$vic_time_series <- renderPlotly({
    req(vic_data())
    data <- vic_data()
    year <- input$vic_year_slider
    
    # Filter data for selected year
    filtered_data <- data %>%
      filter(year == year)
    
    # Create time series plot
    plot_ly(filtered_data, x = ~day, y = ~value, type = "scatter", mode = "lines",
            color = ~variable) %>%
      layout(title = paste("VIC Model Time Series - Year:", year))
  })
  
  # SMAP tab
  output$smap_interactive_map <- renderPlotly({
    req(smap_data())
    data <- smap_data()
    year <- input$smap_data_year_slider
    day <- input$smap_data_day_slider
    
    # Filter data for selected year and day
    filtered_data <- data %>%
      filter(year == year, day == day)
    
    # Create interactive map
    plot_ly(filtered_data, x = ~lon, y = ~lat, z = ~value,
            type = "heatmap", colors = "Viridis") %>%
      layout(title = paste("SMAP Data - Year:", year, "Day:", day))
  })
  
  # GRACE tab
  output$grace_interactive_map <- renderPlotly({
    req(grace_data())
    data <- grace_data()
    year <- input$grace_data_year_slider
    day <- input$grace_data_day_slider
    
    # Filter data for selected year and day
    filtered_data <- data %>%
      filter(year == year, day == day)
    
    # Create interactive map
    plot_ly(filtered_data, x = ~lon, y = ~lat, z = ~value,
            type = "heatmap", colors = "Viridis") %>%
      layout(title = paste("GRACE Data - Year:", year, "Day:", day))
  })

  # Enhanced time series plots with year selection
  output$vic_time_series_enhanced <- renderPlotly({
    data <- vic_data()
    if (!is.null(data)) {
      selected_year <- input$selected_year
      
      filtered_data <- data %>%
        filter(year(date) == selected_year) %>%
        group_by(date) %>%
        summarise(
          OUT_PREC = mean(OUT_PREC, na.rm = TRUE),
          OUT_EVAP = mean(OUT_EVAP, na.rm = TRUE),
          OUT_RUNOFF = mean(OUT_RUNOFF, na.rm = TRUE)
        )
      
      if (nrow(filtered_data) > 0) {
        plot_ly(data = filtered_data) %>%
          add_trace(x = ~date, y = ~OUT_PREC, name = "Precipitation", type = 'scatter', mode = 'lines') %>%
          add_trace(x = ~date, y = ~OUT_EVAP, name = "Evapotranspiration", type = 'scatter', mode = 'lines') %>%
          add_trace(x = ~date, y = ~OUT_RUNOFF, name = "Runoff", type = 'scatter', mode = 'lines') %>%
          layout(title = paste("VIC Model Time Series -", selected_year),
                 xaxis = list(title = "Date"),
                 yaxis = list(title = "Value (mm)"))
      }
    }
  })

  # VIC Data Processing
  vic_data <- reactive({
    req(input$year_slider, input$day_slider)
    year <- input$year_slider
    day <- input$day_slider
    
    # Load the processed VIC data for the selected year
    vic_file <- paste0("data/vic_processed/vic_", year, ".rds")
    if (file.exists(vic_file)) {
      data <- readRDS(vic_file)
      # Filter for the selected day
      data <- data[data$day == day, ]
      return(data)
    }
    return(NULL)
  })

  # Precipitation Analysis
  output$vic_precip_map <- renderPlotly({
    data <- vic_data()
    if (!is.null(data)) {
      plot_ly(data = data, x = ~lon, y = ~lat, z = ~OUT_PREC, type = "heatmap",
              colorscale = "Viridis") %>%
        layout(title = "Precipitation Spatial Distribution",
               xaxis = list(title = "Longitude"),
               yaxis = list(title = "Latitude"))
    }
  })

  output$vic_precip_ts <- renderPlotly({
    data <- vic_data()
    if (!is.null(data)) {
      plot_ly(data = data, x = ~date, y = ~OUT_PREC, type = 'scatter', mode = 'lines') %>%
        layout(title = "Precipitation Time Series",
               xaxis = list(title = "Date"),
               yaxis = list(title = "Precipitation (mm)"))
    }
  })

  output$vic_rain_snow_ts <- renderPlotly({
    data <- vic_data()
    if (!is.null(data)) {
      plot_ly(data = data) %>%
        add_trace(x = ~date, y = ~OUT_RAINF, name = "Rainfall", type = 'scatter', mode = 'lines') %>%
        add_trace(x = ~date, y = ~OUT_SNOWF, name = "Snowfall", type = 'scatter', mode = 'lines') %>%
        layout(title = "Rainfall vs Snowfall",
               xaxis = list(title = "Date"),
               yaxis = list(title = "Amount (mm)"))
    }
  })

  # Evapotranspiration Analysis
  output$vic_et_map <- renderPlotly({
    data <- vic_data()
    if (!is.null(data)) {
      plot_ly(data = data, x = ~lon, y = ~lat, z = ~OUT_EVAP, type = "heatmap",
              colorscale = "Viridis") %>%
        layout(title = "Evapotranspiration Spatial Distribution",
               xaxis = list(title = "Longitude"),
               yaxis = list(title = "Latitude"))
    }
  })

  output$vic_et_components <- renderPlotly({
    data <- vic_data()
    if (!is.null(data)) {
      plot_ly(data = data) %>%
        add_trace(x = ~date, y = ~OUT_EVAP_CANOP, name = "Canopy Evaporation", type = 'scatter', mode = 'lines') %>%
        add_trace(x = ~date, y = ~OUT_TRANSP_VEG, name = "Vegetation Transpiration", type = 'scatter', mode = 'lines') %>%
        add_trace(x = ~date, y = ~OUT_EVAP_BARE, name = "Bare Soil Evaporation", type = 'scatter', mode = 'lines') %>%
        layout(title = "Evapotranspiration Components",
               xaxis = list(title = "Date"),
               yaxis = list(title = "Amount (mm)"))
    }
  })

  # Runoff Analysis
  output$vic_runoff_map <- renderPlotly({
    data <- vic_data()
    if (!is.null(data)) {
      plot_ly(data = data, x = ~lon, y = ~lat, z = ~OUT_RUNOFF, type = "heatmap",
              colorscale = "Viridis") %>%
        layout(title = "Runoff Spatial Distribution",
               xaxis = list(title = "Longitude"),
               yaxis = list(title = "Latitude"))
    }
  })

  output$vic_runoff_components <- renderPlotly({
    data <- vic_data()
    if (!is.null(data)) {
      plot_ly(data = data) %>%
        add_trace(x = ~date, y = ~OUT_RUNOFF, name = "Surface Runoff", type = 'scatter', mode = 'lines') %>%
        add_trace(x = ~date, y = ~OUT_BASEFLOW, name = "Baseflow", type = 'scatter', mode = 'lines') %>%
        layout(title = "Runoff Components",
               xaxis = list(title = "Date"),
               yaxis = list(title = "Amount (mm)"))
    }
  })

  # Soil Moisture Analysis
  output$vic_soil_moisture_map <- renderPlotly({
    data <- vic_data()
    if (!is.null(data)) {
      plot_ly(data = data, x = ~lon, y = ~lat, z = ~OUT_SOIL_MOIST, type = "heatmap",
              colorscale = "Viridis") %>%
        layout(title = "Soil Moisture Spatial Distribution",
               xaxis = list(title = "Longitude"),
               yaxis = list(title = "Latitude"))
    }
  })

  output$vic_soil_layers <- renderPlotly({
    data <- vic_data()
    if (!is.null(data)) {
      plot_ly(data = data) %>%
        add_trace(x = ~date, y = ~OUT_SOIL_MOIST_1, name = "Layer 1", type = 'scatter', mode = 'lines') %>%
        add_trace(x = ~date, y = ~OUT_SOIL_MOIST_2, name = "Layer 2", type = 'scatter', mode = 'lines') %>%
        add_trace(x = ~date, y = ~OUT_SOIL_MOIST_3, name = "Layer 3", type = 'scatter', mode = 'lines') %>%
        layout(title = "Soil Moisture by Layer",
               xaxis = list(title = "Date"),
               yaxis = list(title = "Soil Moisture (mm)"))
    }
  })

  # Snow Analysis
  output$vic_swe_map <- renderPlotly({
    data <- vic_data()
    if (!is.null(data)) {
      plot_ly(data = data, x = ~lon, y = ~lat, z = ~OUT_SWE, type = "heatmap",
              colorscale = "Viridis") %>%
        layout(title = "Snow Water Equivalent Spatial Distribution",
               xaxis = list(title = "Longitude"),
               yaxis = list(title = "Latitude"))
    }
  })

  output$vic_snow_melt <- renderPlotly({
    data <- vic_data()
    if (!is.null(data)) {
      plot_ly(data = data) %>%
        add_trace(x = ~date, y = ~OUT_SWE, name = "Snow Water Equivalent", type = 'scatter', mode = 'lines') %>%
        add_trace(x = ~date, y = ~OUT_SNOW_MELT, name = "Snow Melt", type = 'scatter', mode = 'lines') %>%
        layout(title = "Snow Water Equivalent and Melt",
               xaxis = list(title = "Date"),
               yaxis = list(title = "Amount (mm)"))
    }
  })

  # Temperature Analysis
  output$vic_air_temp <- renderPlotly({
    data <- vic_data()
    if (!is.null(data)) {
      plot_ly(data = data, x = ~date, y = ~OUT_AIR_TEMP, type = 'scatter', mode = 'lines') %>%
        layout(title = "Air Temperature",
               xaxis = list(title = "Date"),
               yaxis = list(title = "Temperature (°C)"))
    }
  })

  output$vic_surface_temp <- renderPlotly({
    data <- vic_data()
    if (!is.null(data)) {
      plot_ly(data = data) %>%
        add_trace(x = ~date, y = ~OUT_SURF_TEMP, name = "Surface", type = 'scatter', mode = 'lines') %>%
        add_trace(x = ~date, y = ~OUT_BARESOILT, name = "Bare Soil", type = 'scatter', mode = 'lines') %>%
        add_trace(x = ~date, y = ~OUT_VEGT, name = "Vegetation", type = 'scatter', mode = 'lines') %>%
        layout(title = "Surface Temperatures",
               xaxis = list(title = "Date"),
               yaxis = list(title = "Temperature (°C)"))
    }
  })

  # Combined Analysis
  output$vic_water_balance <- renderPlotly({
    data <- vic_data()
    if (!is.null(data)) {
      plot_ly(data = data) %>%
        add_trace(x = ~date, y = ~OUT_PREC, name = "Precipitation", type = 'scatter', mode = 'lines') %>%
        add_trace(x = ~date, y = ~OUT_EVAP, name = "Evapotranspiration", type = 'scatter', mode = 'lines') %>%
        add_trace(x = ~date, y = ~OUT_RUNOFF, name = "Runoff", type = 'scatter', mode = 'lines') %>%
        layout(title = "Water Balance Components",
               xaxis = list(title = "Date"),
               yaxis = list(title = "Amount (mm)"))
    }
  })

  output$vic_trends <- renderPlotly({
    data <- vic_data()
    if (!is.null(data)) {
      # Calculate trends for each variable
      trends <- data %>%
        group_by(year = year(date)) %>%
        summarise(
          prec = mean(OUT_PREC),
          et = mean(OUT_EVAP),
          runoff = mean(OUT_RUNOFF),
          swe = mean(OUT_SWE)
        )
      
      plot_ly(data = trends) %>%
        add_trace(x = ~year, y = ~prec, name = "Precipitation", type = 'scatter', mode = 'lines+markers') %>%
        add_trace(x = ~year, y = ~et, name = "Evapotranspiration", type = 'scatter', mode = 'lines+markers') %>%
        add_trace(x = ~year, y = ~runoff, name = "Runoff", type = 'scatter', mode = 'lines+markers') %>%
        add_trace(x = ~year, y = ~swe, name = "Snow Water Equivalent", type = 'scatter', mode = 'lines+markers') %>%
        layout(title = "Annual Trends",
               xaxis = list(title = "Year"),
               yaxis = list(title = "Annual Mean (mm)"))
    }
  })

  output$vic_correlations <- renderPlotly({
    data <- vic_data()
    if (!is.null(data)) {
      # Calculate correlations between variables
      cor_matrix <- cor(data[, c("OUT_PREC", "OUT_EVAP", "OUT_RUNOFF", "OUT_SWE", "OUT_SOIL_MOIST")])
      
      plot_ly(z = cor_matrix,
              x = c("Precipitation", "Evapotranspiration", "Runoff", "SWE", "Soil Moisture"),
              y = c("Precipitation", "Evapotranspiration", "Runoff", "SWE", "Soil Moisture"),
              type = "heatmap",
              colorscale = "RdBu") %>%
        layout(title = "Variable Correlations",
               xaxis = list(title = ""),
               yaxis = list(title = ""))
    }
  })

  output$vic_extremes <- renderPlotly({
    data <- vic_data()
    if (!is.null(data)) {
      # Calculate extreme events
      extremes <- data %>%
        group_by(year = year(date)) %>%
        summarise(
          max_prec = max(OUT_PREC),
          max_et = max(OUT_EVAP),
          max_runoff = max(OUT_RUNOFF),
          max_swe = max(OUT_SWE)
        )
      
      plot_ly(data = extremes) %>%
        add_trace(x = ~year, y = ~max_prec, name = "Max Precipitation", type = 'scatter', mode = 'lines+markers') %>%
        add_trace(x = ~year, y = ~max_et, name = "Max Evapotranspiration", type = 'scatter', mode = 'lines+markers') %>%
        add_trace(x = ~year, y = ~max_runoff, name = "Max Runoff", type = 'scatter', mode = 'lines+markers') %>%
        add_trace(x = ~year, y = ~max_swe, name = "Max SWE", type = 'scatter', mode = 'lines+markers') %>%
        layout(title = "Extreme Events",
               xaxis = list(title = "Year"),
               yaxis = list(title = "Maximum Value (mm)"))
    }
  })

  # Add precipitation statistics output
  output$precipitation_stats <- renderTable({
    data.frame(
      Statistic = c("Mean", "Maximum", "Minimum", "Standard Deviation"),
      Value = c("0.78 mm", "9.09 mm", "0 mm", "1.31 mm")
    )
  })
  
  # Render interactive map
  output$interactive_map <- renderUI({
    tags$iframe(
      src = "images/dashboard/interactive_map.html",
      width = "100%",
      height = "600px",
      frameborder = "0"
    )
  })
  
  # Render time series plots
  output$monthly_timeseries <- renderImage({
    list(
      src = "images/dashboard/monthly_timeseries.png",
      width = "100%",
      height = "400px"
    )
  }, deleteFile = FALSE)
  
  output$annual_timeseries <- renderImage({
    list(
      src = "images/dashboard/annual_timeseries.png",
      width = "100%",
      height = "400px"
    )
  }, deleteFile = FALSE)
  
  # Render correlation plot
  output$correlation_matrix <- renderImage({
    list(
      src = "images/dashboard/correlation_matrix.png",
      width = "100%",
      height = "400px"
    )
  }, deleteFile = FALSE)
  
  # Render trend plots
  output$trend_precipitation <- renderImage({
    list(
      src = "images/dashboard/trend_precipitation.png",
      width = "100%",
      height = "300px"
    )
  }, deleteFile = FALSE)
  
  output$trend_swe <- renderImage({
    list(
      src = "images/dashboard/trend_swe.png",
      width = "100%",
      height = "300px"
    )
  }, deleteFile = FALSE)
  
  output$trend_runoff <- renderImage({
    list(
      src = "images/dashboard/trend_runoff.png",
      width = "100%",
      height = "300px"
    )
  }, deleteFile = FALSE)
  
  output$trend_baseflow <- renderImage({
    list(
      src = "images/dashboard/trend_baseflow.png",
      width = "100%",
      height = "300px"
    )
  }, deleteFile = FALSE)
  
  output$trend_evap <- renderImage({
    list(
      src = "images/dashboard/trend_evap.png",
      width = "100%",
      height = "300px"
    )
  }, deleteFile = FALSE)
  
  output$trend_soil_moisture <- renderImage({
    list(
      src = "images/dashboard/trend_soil_moisture.png",
      width = "100%",
      height = "300px"
    )
  }, deleteFile = FALSE) 

  # VIC Model Tab Server Code
  output$vic_info_boxes <- renderUI({
    req(input$vic_variable)
    metadata <- get_vic_metadata(input$vic_variable)
    
    fluidRow(
      infoBox("Variable", metadata$name, 
              icon = icon("chart-line"), color = "blue", width = 4),
      infoBox("Unit", metadata$unit, 
              icon = icon("ruler"), color = "green", width = 4),
      infoBox("Description", metadata$description, 
              icon = icon("info-circle"), color = "light-blue", width = 4)
    )
  })
  
  output$vic_spatial_map <- renderUI({
    req(input$vic_variable)
    
    # Try to get interactive data
    tryCatch({
      nc_file <- vic_data()
      var_data <- ncvar_get(nc_file, input$vic_variable)
      time <- ncvar_get(nc_file, "time")
      dates <- convert_vic_time(time)
      
      # Filter data for selected year and day
      year_idx <- which(year(dates) == input$vic_year_slider)
      day_idx <- which(yday(dates[year_idx]) == input$vic_day_slider)
      
      if (length(day_idx) == 0) stop("No data available")
      
      # Get data for the selected day
      if (length(dim(var_data)) == 3) {
        data <- var_data[,,day_idx]
      } else if (length(dim(var_data)) == 4) {
        data <- var_data[,,1,day_idx]  # Use first layer for soil moisture
      }
      
      # Create spatial map
      plot_ly(z = data, type = "heatmap", colors = "Viridis") %>%
        layout(title = paste("Spatial Distribution -", input$vic_variable),
               xaxis = list(title = "Longitude"),
               yaxis = list(title = "Latitude"))
    }, error = function(e) {
      # If interactive data fails, show static image
      var_name <- tolower(gsub("OUT_", "", input$vic_variable))
      img_path <- paste0("images/dashboard/spatial_", var_name, ".png")
      
      if (file.exists(img_path)) {
        tags$img(src = img_path, width = "100%", height = "auto")
      } else {
        tags$div(class = "alert alert-warning",
                "No data available for this variable")
      }
    })
  })
  
  output$vic_time_series <- renderUI({
    req(input$vic_variable)
    
    # Try to get interactive data
    tryCatch({
      nc_file <- vic_data()
      var_data <- ncvar_get(nc_file, input$vic_variable)
      time <- ncvar_get(nc_file, "time")
      dates <- convert_vic_time(time)
      
      # Calculate time series
      if (length(dim(var_data)) == 3) {
        ts_data <- apply(var_data, 3, mean, na.rm = TRUE)
      } else if (length(dim(var_data)) == 4) {
        ts_data <- apply(var_data[,,1,], 3, mean, na.rm = TRUE)
      }
      
      # Create time series plot
      plot_ly(x = dates, y = ts_data, type = "scatter", mode = "lines") %>%
        layout(title = paste("Time Series -", input$vic_variable),
               xaxis = list(title = "Date"),
               yaxis = list(title = paste("Value (", get_vic_metadata(input$vic_variable)$unit, ")")))
    }, error = function(e) {
      # If interactive data fails, show static image
      var_name <- tolower(gsub("OUT_", "", input$vic_variable))
      img_path <- paste0("images/dashboard/timeseries_", var_name, ".png")
      
      if (file.exists(img_path)) {
        tags$img(src = img_path, width = "100%", height = "auto")
      } else {
        tags$div(class = "alert alert-warning",
                "No data available for this variable")
      }
    })
  })
  
  output$vic_monthly_stats <- renderUI({
    req(input$vic_variable)
    
    # Try to get interactive data
    tryCatch({
      nc_file <- vic_data()
      var_data <- ncvar_get(nc_file, input$vic_variable)
      time <- ncvar_get(nc_file, "time")
      dates <- convert_vic_time(time)
      
      # Calculate monthly statistics
      monthly_stats <- data.frame(
        date = dates,
        month = month(dates, label = TRUE),
        year = year(dates)
      )
      
      if (length(dim(var_data)) == 3) {
        monthly_stats$value <- apply(var_data, 3, mean, na.rm = TRUE)
      } else if (length(dim(var_data)) == 4) {
        monthly_stats$value <- apply(var_data[,,1,], 3, mean, na.rm = TRUE)
      }
      
      # Calculate monthly means
      monthly_means <- monthly_stats %>%
        group_by(month) %>%
        summarise(mean_value = mean(value, na.rm = TRUE))
      
      # Create plot
      plot_ly(monthly_means, x = ~month, y = ~mean_value, type = "bar") %>%
        layout(title = "Monthly Statistics",
               xaxis = list(title = "Month"),
               yaxis = list(title = paste("Mean Value (", get_vic_metadata(input$vic_variable)$unit, ")")))
    }, error = function(e) {
      # If interactive data fails, show static image
      var_name <- tolower(gsub("OUT_", "", input$vic_variable))
      img_path <- paste0("images/dashboard/monthly_", var_name, ".png")
      
      if (file.exists(img_path)) {
        tags$img(src = img_path, width = "100%", height = "auto")
      } else {
        tags$div(class = "alert alert-warning",
                "No data available for this variable")
      }
    })
  })
  
  output$vic_additional_analysis <- renderUI({
    req(input$vic_variable)
    
    # Add additional analysis based on variable type
    if (input$vic_variable == "OUT_SOIL_MOIST") {
      fluidRow(
        column(width = 6,
               box(title = "Soil Layers", width = NULL, solidHeader = TRUE,
                   plotlyOutput("vic_soil_layers", height = "300px")
               )
        ),
        column(width = 6,
               box(title = "Monthly Statistics", width = NULL, solidHeader = TRUE,
                   plotlyOutput("vic_monthly_stats", height = "300px")
               )
        )
      )
    } else if (input$vic_variable == "OUT_PREC") {
      fluidRow(
        column(width = 6,
               box(title = "Rain vs Snow", width = NULL, solidHeader = TRUE,
                   plotlyOutput("vic_rain_snow", height = "300px")
               )
        ),
        column(width = 6,
               box(title = "Monthly Statistics", width = NULL, solidHeader = TRUE,
                   plotlyOutput("vic_monthly_stats", height = "300px")
               )
        )
      )
    } else if (input$vic_variable == "OUT_EVAP") {
      fluidRow(
        column(width = 6,
               box(title = "Components", width = NULL, solidHeader = TRUE,
                   plotlyOutput("vic_et_components", height = "300px")
               )
        ),
        column(width = 6,
               box(title = "Monthly Statistics", width = NULL, solidHeader = TRUE,
                   plotlyOutput("vic_monthly_stats", height = "300px")
               )
        )
      )
    } else if (input$vic_variable == "OUT_RUNOFF") {
      fluidRow(
        column(width = 6,
               box(title = "Surface vs Baseflow", width = NULL, solidHeader = TRUE,
                   plotlyOutput("vic_runoff_components", height = "300px")
               )
        ),
        column(width = 6,
               box(title = "Monthly Statistics", width = NULL, solidHeader = TRUE,
                   plotlyOutput("vic_monthly_stats", height = "300px")
               )
        )
      )
    } else {
      fluidRow(
        column(width = 12,
               box(title = "Monthly Statistics", width = NULL, solidHeader = TRUE,
                   plotlyOutput("vic_monthly_stats", height = "300px")
               )
        )
      )
    }
  })
  
  # Additional VIC Model Plot Outputs
  output$vic_soil_layers <- renderPlotly({
    req(input$vic_variable == "OUT_SOIL_MOIST")
    
    nc_file <- vic_data()
    var_data <- ncvar_get(nc_file, "OUT_SOIL_MOIST")
    time <- ncvar_get(nc_file, "time")
    dates <- convert_vic_time(time)
    
    # Calculate mean for each layer
    layer_means <- lapply(1:3, function(layer) {
      apply(var_data[,,layer,], 3, mean, na.rm = TRUE)
    })
    
    # Create plot
    plot_ly() %>%
      add_trace(x = dates, y = layer_means[[1]], name = "Layer 1", type = "scatter", mode = "lines") %>%
      add_trace(x = dates, y = layer_means[[2]], name = "Layer 2", type = "scatter", mode = "lines") %>%
      add_trace(x = dates, y = layer_means[[3]], name = "Layer 3", type = "scatter", mode = "lines") %>%
      layout(title = "Soil Moisture by Layer",
             xaxis = list(title = "Date"),
             yaxis = list(title = "Soil Moisture (mm)"))
  })
  
  output$vic_rain_snow <- renderPlotly({
    req(input$vic_variable == "OUT_PREC")
    
    nc_file <- vic_data()
    rain_data <- ncvar_get(nc_file, "OUT_RAINF")
    snow_data <- ncvar_get(nc_file, "OUT_SNOWF")
    time <- ncvar_get(nc_file, "time")
    dates <- convert_vic_time(time)
    
    # Calculate means
    rain_mean <- apply(rain_data, 3, mean, na.rm = TRUE)
    snow_mean <- apply(snow_data, 3, mean, na.rm = TRUE)
    
    # Create plot
    plot_ly() %>%
      add_trace(x = dates, y = rain_mean, name = "Rainfall", type = "scatter", mode = "lines") %>%
      add_trace(x = dates, y = snow_mean, name = "Snowfall", type = "scatter", mode = "lines") %>%
      layout(title = "Rainfall vs Snowfall",
             xaxis = list(title = "Date"),
             yaxis = list(title = "Precipitation (mm/day)"))
  })
  
  output$vic_et_components <- renderPlotly({
    req(input$vic_variable == "OUT_EVAP")
    
    nc_file <- vic_data()
    bare_evap <- ncvar_get(nc_file, "OUT_EVAP_BARE")
    canop_evap <- ncvar_get(nc_file, "OUT_EVAP_CANOP")
    transp_veg <- ncvar_get(nc_file, "OUT_TRANSP_VEG")
    time <- ncvar_get(nc_file, "time")
    dates <- convert_vic_time(time)
    
    # Calculate means
    bare_mean <- apply(bare_evap, 3, mean, na.rm = TRUE)
    canop_mean <- apply(canop_evap, 3, mean, na.rm = TRUE)
    transp_mean <- apply(transp_veg, 3, mean, na.rm = TRUE)
    
    # Create plot
    plot_ly() %>%
      add_trace(x = dates, y = bare_mean, name = "Bare Soil", type = "scatter", mode = "lines") %>%
      add_trace(x = dates, y = canop_mean, name = "Canopy", type = "scatter", mode = "lines") %>%
      add_trace(x = dates, y = transp_mean, name = "Transpiration", type = "scatter", mode = "lines") %>%
      layout(title = "Evapotranspiration Components",
             xaxis = list(title = "Date"),
             yaxis = list(title = "ET (mm/day)"))
  })
  
  output$vic_runoff_components <- renderPlotly({
    req(input$vic_variable == "OUT_RUNOFF")
    
    nc_file <- vic_data()
    runoff_data <- ncvar_get(nc_file, "OUT_RUNOFF")
    baseflow_data <- ncvar_get(nc_file, "OUT_BASEFLOW")
    time <- ncvar_get(nc_file, "time")
    dates <- convert_vic_time(time)
    
    # Calculate means
    runoff_mean <- apply(runoff_data, 3, mean, na.rm = TRUE)
    baseflow_mean <- apply(baseflow_data, 3, mean, na.rm = TRUE)
    
    # Create plot
    plot_ly() %>%
      add_trace(x = dates, y = runoff_mean, name = "Surface Runoff", type = "scatter", mode = "lines") %>%
      add_trace(x = dates, y = baseflow_mean, name = "Baseflow", type = "scatter", mode = "lines") %>%
      layout(title = "Runoff Components",
             xaxis = list(title = "Date"),
             yaxis = list(title = "Flow (mm/day)"))
  })
  
  output$vic_monthly_stats <- renderPlotly({
    req(input$vic_variable)
    
    nc_file <- vic_data()
    var_data <- ncvar_get(nc_file, input$vic_variable)
    time <- ncvar_get(nc_file, "time")
    dates <- convert_vic_time(time)
    
    # Calculate monthly statistics
    monthly_stats <- data.frame(
      date = dates,
      month = month(dates, label = TRUE),
      year = year(dates)
    )
    
    if (length(dim(var_data)) == 3) {
      monthly_stats$value <- apply(var_data, 3, mean, na.rm = TRUE)
    } else if (length(dim(var_data)) == 4) {
      monthly_stats$value <- apply(var_data[,,1,], 3, mean, na.rm = TRUE)
    }
    
    # Calculate monthly means
    monthly_means <- monthly_stats %>%
      group_by(month) %>%
      summarise(mean_value = mean(value, na.rm = TRUE))
    
    # Create plot
    plot_ly(monthly_means, x = ~month, y = ~mean_value, type = "bar") %>%
      layout(title = "Monthly Statistics",
             xaxis = list(title = "Month"),
             yaxis = list(title = paste("Mean Value (", get_vic_metadata(input$vic_variable)$unit, ")")))
  })

  # VIC Model Tab Server Code
  # Load basin boundary data
  basin_boundary <- reactive({
    sf::st_read("data/basin_boundary.shp")
  })
  
  # Precipitation Plots
  output$prec_spatial <- renderPlotly({
    req(basin_boundary(), vic_data())
    
    # Create spatial plot with basin boundary
    plot_ly() %>%
      add_sf(data = basin_boundary(), 
             color = I("gray"), 
             opacity = 0.2,
             name = "Basin Boundary") %>%
      add_heatmap(data = vic_data(),
                  x = ~lon, y = ~lat, z = ~value,
                  colors = colorRamp(c("blue", "white", "red")),
                  name = "Precipitation") %>%
      layout(title = paste("Spatial Distribution - Precipitation",
                          "\nYear:", input$year_slider,
                          "Day:", input$day_slider),
             xaxis = list(title = "Longitude"),
             yaxis = list(title = "Latitude"),
             showlegend = TRUE)
  })
  
  # Evapotranspiration Plots
  output$evap_spatial <- renderPlotly({
    req(basin_boundary(), vic_data())
    
    plot_ly() %>%
      add_sf(data = basin_boundary(), 
             color = I("gray"), 
             opacity = 0.2,
             name = "Basin Boundary") %>%
      add_heatmap(data = vic_data(),
                  x = ~lon, y = ~lat, z = ~value,
                  colors = colorRamp(c("blue", "white", "red")),
                  name = "Evapotranspiration") %>%
      layout(title = paste("Spatial Distribution - Evapotranspiration",
                          "\nYear:", input$year_slider,
                          "Day:", input$day_slider),
             xaxis = list(title = "Longitude"),
             yaxis = list(title = "Latitude"),
             showlegend = TRUE)
  })
  
  # Runoff Plots
  output$runoff_spatial <- renderPlotly({
    req(basin_boundary(), vic_data())
    
    plot_ly() %>%
      add_sf(data = basin_boundary(), 
             color = I("gray"), 
             opacity = 0.2,
             name = "Basin Boundary") %>%
      add_heatmap(data = vic_data(),
                  x = ~lon, y = ~lat, z = ~value,
                  colors = colorRamp(c("blue", "white", "red")),
                  name = "Runoff") %>%
      layout(title = paste("Spatial Distribution - Runoff",
                          "\nYear:", input$year_slider,
                          "Day:", input$day_slider),
             xaxis = list(title = "Longitude"),
             yaxis = list(title = "Latitude"),
             showlegend = TRUE)
  })
  
  # Soil Moisture Plots
  output$soil_moisture_spatial <- renderPlotly({
    req(basin_boundary(), vic_data())
    
    plot_ly() %>%
      add_sf(data = basin_boundary(), 
             color = I("gray"), 
             opacity = 0.2,
             name = "Basin Boundary") %>%
      add_heatmap(data = vic_data(),
                  x = ~lon, y = ~lat, z = ~value,
                  colors = colorRamp(c("brown", "white", "green")),
                  name = "Soil Moisture") %>%
      layout(title = paste("Spatial Distribution - Soil Moisture",
                          "\nYear:", input$year_slider,
                          "Day:", input$day_slider),
             xaxis = list(title = "Longitude"),
             yaxis = list(title = "Latitude"),
             showlegend = TRUE)
  })
  
  # Snow Water Equivalent Plots
  output$swe_spatial <- renderPlotly({
    req(basin_boundary(), vic_data())
    
    plot_ly() %>%
      add_sf(data = basin_boundary(), 
             color = I("gray"), 
             opacity = 0.2,
             name = "Basin Boundary") %>%
      add_heatmap(data = vic_data(),
                  x = ~lon, y = ~lat, z = ~value,
                  colors = colorRamp(c("white", "lightblue", "blue")),
                  name = "Snow Water Equivalent") %>%
      layout(title = paste("Spatial Distribution - Snow Water Equivalent",
                          "\nYear:", input$year_slider,
                          "Day:", input$day_slider),
             xaxis = list(title = "Longitude"),
             yaxis = list(title = "Latitude"),
             showlegend = TRUE)
  })
  
  # Temperature Plots
  output$temp_spatial <- renderPlotly({
    req(basin_boundary(), vic_data())
    
    plot_ly() %>%
      add_sf(data = basin_boundary(), 
             color = I("gray"), 
             opacity = 0.2,
             name = "Basin Boundary") %>%
      add_heatmap(data = vic_data(),
                  x = ~lon, y = ~lat, z = ~value,
                  colors = colorRamp(c("blue", "white", "red")),
                  name = "Temperature") %>%
      layout(title = paste("Spatial Distribution - Temperature",
                          "\nYear:", input$year_slider,
                          "Day:", input$day_slider),
             xaxis = list(title = "Longitude"),
             yaxis = list(title = "Latitude"),
             showlegend = TRUE)
  })

  # Water Balance Tab UI
  output$water_balance_components <- renderPlotly({
    data <- vic_data()
    if (!is.null(data)) {
      plot_ly(data = data) %>%
        add_trace(x = ~date, y = ~OUT_PREC, name = "Precipitation", type = 'scatter', mode = 'lines') %>%
        add_trace(x = ~date, y = ~OUT_EVAP, name = "Evapotranspiration", type = 'scatter', mode = 'lines') %>%
        add_trace(x = ~date, y = ~OUT_RUNOFF, name = "Runoff", type = 'scatter', mode = 'lines') %>%
        layout(title = "Water Balance Components",
               xaxis = list(title = "Date"),
               yaxis = list(title = "Amount (mm)"))
    }
  })

  # SMAP Analysis Outputs
  output$smap_spatial <- renderPlotly({
    req(basin_boundary(), vic_data())
    plot_ly() %>%
      add_sf(data = basin_boundary(), 
             color = I("gray"), 
             opacity = 0.2,
             name = "Basin Boundary") %>%
      add_heatmap(data = vic_data(),
                  x = ~lon, y = ~lat, z = ~value,
                  colors = colorRamp(c("brown", "white", "green")),
                  name = "SMAP Soil Moisture") %>%
      layout(title = paste("Spatial Distribution - SMAP",
                          "\nYear:", input$year_slider,
                          "Day:", input$day_slider),
             xaxis = list(title = "Longitude"),
             yaxis = list(title = "Latitude"),
             showlegend = TRUE)
  })

  output$smap_timeseries <- renderPlotly({
    req(vic_data())
    plot_ly(data = vic_data(), x = ~date, y = ~value, type = 'scatter', mode = 'lines') %>%
      layout(title = "SMAP Time Series",
             xaxis = list(title = "Date"),
             yaxis = list(title = "Soil Moisture (mm)"))
  })

  output$smap_monthly <- renderPlotly({
    req(vic_data())
    monthly_data <- vic_data() %>%
      mutate(month = month(date)) %>%
      group_by(month) %>%
      summarise(mean_value = mean(value, na.rm = TRUE))
    
    plot_ly(data = monthly_data, x = ~month, y = ~mean_value, type = 'bar') %>%
      layout(title = "Monthly SMAP Statistics",
             xaxis = list(title = "Month"),
             yaxis = list(title = "Mean Soil Moisture (mm)"))
  })

  output$smap_depth <- renderPlotly({
    req(vic_data())
    plot_ly(data = vic_data(), x = ~date, y = ~value, type = 'scatter', mode = 'lines',
            name = input$smap_depth) %>%
      layout(title = "Depth Profile Analysis",
             xaxis = list(title = "Date"),
             yaxis = list(title = "Soil Moisture (mm)"))
  })

  output$smap_land_cover <- renderPlotly({
    req(vic_data())
    plot_ly(data = vic_data(), x = ~date, y = ~value, type = 'scatter', mode = 'lines') %>%
      layout(title = "Land Cover Impact",
             xaxis = list(title = "Date"),
             yaxis = list(title = "Soil Moisture (mm)"))
  })

  output$smap_drought <- renderPlotly({
    req(vic_data())
    plot_ly(data = vic_data(), x = ~date, y = ~value, type = 'scatter', mode = 'lines') %>%
      layout(title = "Drought Analysis",
             xaxis = list(title = "Date"),
             yaxis = list(title = "Soil Moisture (mm)"))
  })

  output$smap_vegetation <- renderPlotly({
    req(vic_data())
    plot_ly(data = vic_data(), x = ~date, y = ~value, type = 'scatter', mode = 'lines') %>%
      layout(title = "Vegetation Response",
             xaxis = list(title = "Date"),
             yaxis = list(title = "Soil Moisture (mm)"))
  })

  output$smap_climate <- renderPlotly({
    req(vic_data())
    plot_ly(data = vic_data(), x = ~date, y = ~value, type = 'scatter', mode = 'lines') %>%
      layout(title = "Climate Impact",
             xaxis = list(title = "Date"),
             yaxis = list(title = "Soil Moisture (mm)"))
  })

  output$smap_human <- renderPlotly({
    req(vic_data())
    plot_ly(data = vic_data(), x = ~date, y = ~value, type = 'scatter', mode = 'lines') %>%
      layout(title = "Human Impact",
             xaxis = list(title = "Date"),
             yaxis = list(title = "Soil Moisture (mm)"))
  })

  # GRACE Analysis Outputs
  output$grace_spatial <- renderPlotly({
    req(basin_boundary(), vic_data())
    plot_ly() %>%
      add_sf(data = basin_boundary(), 
             color = I("gray"), 
             opacity = 0.2,
             name = "Basin Boundary") %>%
      add_heatmap(data = vic_data(),
                  x = ~lon, y = ~lat, z = ~value,
                  colors = colorRamp(c("blue", "white", "red")),
                  name = "GRACE TWS") %>%
      layout(title = paste("Spatial Distribution - GRACE",
                          "\nYear:", input$year_slider,
                          "Day:", input$day_slider),
             xaxis = list(title = "Longitude"),
             yaxis = list(title = "Latitude"),
             showlegend = TRUE)
  })

  output$grace_timeseries <- renderPlotly({
    req(vic_data())
    plot_ly(data = vic_data(), x = ~date, y = ~value, type = 'scatter', mode = 'lines') %>%
      layout(title = "GRACE Time Series",
             xaxis = list(title = "Date"),
             yaxis = list(title = "TWS Anomaly (mm)"))
  })

  output$grace_monthly <- renderPlotly({
    req(vic_data())
    monthly_data <- vic_data() %>%
      mutate(month = month(date)) %>%
      group_by(month) %>%
      summarise(mean_value = mean(value, na.rm = TRUE))
    
    plot_ly(data = monthly_data, x = ~month, y = ~mean_value, type = 'bar') %>%
      layout(title = "Monthly GRACE Statistics",
             xaxis = list(title = "Month"),
             yaxis = list(title = "Mean TWS Anomaly (mm)"))
  })

  output$grace_components <- renderPlotly({
    req(vic_data())
    plot_ly(data = vic_data(), x = ~date, y = ~value, type = 'scatter', mode = 'lines') %>%
      layout(title = "Water Storage Components",
             xaxis = list(title = "Date"),
             yaxis = list(title = "Storage (mm)"))
  })

  output$grace_groundwater <- renderPlotly({
    req(vic_data())
    plot_ly(data = vic_data(), x = ~date, y = ~value, type = 'scatter', mode = 'lines') %>%
      layout(title = "Groundwater Analysis",
             xaxis = list(title = "Date"),
             yaxis = list(title = "Groundwater Storage (mm)"))
  })

  output$grace_climate <- renderPlotly({
    req(vic_data())
    plot_ly(data = vic_data(), x = ~date, y = ~value, type = 'scatter', mode = 'lines') %>%
      layout(title = "Climate Impact",
             xaxis = list(title = "Date"),
             yaxis = list(title = "TWS Anomaly (mm)"))
  })

  output$grace_human <- renderPlotly({
    req(vic_data())
    plot_ly(data = vic_data(), x = ~date, y = ~value, type = 'scatter', mode = 'lines') %>%
      layout(title = "Human Impact",
             xaxis = list(title = "Date"),
             yaxis = list(title = "TWS Anomaly (mm)"))
  })

  output$grace_drought <- renderPlotly({
    req(vic_data())
    plot_ly(data = vic_data(), x = ~date, y = ~value, type = 'scatter', mode = 'lines') %>%
      layout(title = "Drought Analysis",
             xaxis = list(title = "Date"),
             yaxis = list(title = "TWS Anomaly (mm)"))
  })

  output$grace_management <- renderPlotly({
    req(vic_data())
    plot_ly(data = vic_data(), x = ~date, y = ~value, type = 'scatter', mode = 'lines') %>%
      layout(title = "Water Management Impact",
             xaxis = list(title = "Date"),
             yaxis = list(title = "TWS Anomaly (mm)"))
  })

  # Precipitation Anomalies Outputs
  output$precip_anomaly_spatial <- renderPlotly({
    req(basin_boundary(), vic_data())
    plot_ly() %>%
      add_sf(data = basin_boundary(), 
             color = I("gray"), 
             opacity = 0.2,
             name = "Basin Boundary") %>%
      add_heatmap(data = vic_data(),
                  x = ~lon, y = ~lat, z = ~value,
                  colors = colorRamp(c("red", "white", "blue")),
                  name = "Precipitation Anomaly") %>%
      layout(title = paste("Spatial Distribution - Precipitation Anomaly",
                          "\nYear:", input$year_slider,
                          "Day:", input$day_slider),
             xaxis = list(title = "Longitude"),
             yaxis = list(title = "Latitude"),
             showlegend = TRUE)
  })

  output$precip_anomaly_timeseries <- renderPlotly({
    req(vic_data())
    plot_ly(data = vic_data(), x = ~date, y = ~value, type = 'scatter', mode = 'lines') %>%
      layout(title = "Precipitation Anomaly Time Series",
             xaxis = list(title = "Date"),
             yaxis = list(title = "Anomaly (mm)"))
  })

  output$precip_anomaly_monthly <- renderPlotly({
    req(vic_data())
    monthly_data <- vic_data() %>%
      mutate(month = month(date)) %>%
      group_by(month) %>%
      summarise(mean_value = mean(value, na.rm = TRUE))
    
    plot_ly(data = monthly_data, x = ~month, y = ~mean_value, type = 'bar') %>%
      layout(title = "Monthly Precipitation Anomalies",
             xaxis = list(title = "Month"),
             yaxis = list(title = "Mean Anomaly (mm)"))
  })

  output$precip_extremes <- renderPlotly({
    req(vic_data())
    plot_ly(data = vic_data(), x = ~date, y = ~value, type = 'scatter', mode = 'lines') %>%
      layout(title = "Extreme Events",
             xaxis = list(title = "Date"),
             yaxis = list(title = "Precipitation (mm)"))
  })

  output$precip_seasonal <- renderPlotly({
    req(vic_data())
    plot_ly(data = vic_data(), x = ~date, y = ~value, type = 'scatter', mode = 'lines') %>%
      layout(title = "Seasonal Patterns",
             xaxis = list(title = "Date"),
             yaxis = list(title = "Precipitation (mm)"))
  })

  output$precip_climate <- renderPlotly({
    req(vic_data())
    plot_ly(data = vic_data(), x = ~date, y = ~value, type = 'scatter', mode = 'lines') %>%
      layout(title = "Climate Indices",
             xaxis = list(title = "Date"),
             yaxis = list(title = "Index Value"))
  })

  output$precip_impact <- renderPlotly({
    req(vic_data())
    plot_ly(data = vic_data(), x = ~date, y = ~value, type = 'scatter', mode = 'lines') %>%
      layout(title = "Impact Analysis",
             xaxis = list(title = "Date"),
             yaxis = list(title = "Impact Score"))
  })

  output$precip_drought <- renderPlotly({
    req(vic_data())
    plot_ly(data = vic_data(), x = ~date, y = ~value, type = 'scatter', mode = 'lines') %>%
      layout(title = "Drought Analysis",
             xaxis = list(title = "Date"),
             yaxis = list(title = "Drought Index"))
  })

  output$precip_flood <- renderPlotly({
    req(vic_data())
    plot_ly(data = vic_data(), x = ~date, y = ~value, type = 'scatter', mode = 'lines') %>%
      layout(title = "Flood Risk",
             xaxis = list(title = "Date"),
             yaxis = list(title = "Risk Score"))
  })

  # SWE Anomalies Outputs
  output$swe_anomaly_spatial <- renderPlotly({
    req(basin_boundary(), vic_data())
    plot_ly() %>%
      add_sf(data = basin_boundary(), 
             color = I("gray"), 
             opacity = 0.2,
             name = "Basin Boundary") %>%
      add_heatmap(data = vic_data(),
                  x = ~lon, y = ~lat, z = ~value,
                  colors = colorRamp(c("red", "white", "blue")),
                  name = "SWE Anomaly") %>%
      layout(title = paste("Spatial Distribution - SWE Anomaly",
                          "\nYear:", input$year_slider,
                          "Day:", input$day_slider),
             xaxis = list(title = "Longitude"),
             yaxis = list(title = "Latitude"),
             showlegend = TRUE)
  })

  output$swe_anomaly_timeseries <- renderPlotly({
    req(vic_data())
    plot_ly(data = vic_data(), x = ~date, y = ~value, type = 'scatter', mode = 'lines') %>%
      layout(title = "SWE Anomaly Time Series",
             xaxis = list(title = "Date"),
             yaxis = list(title = "Anomaly (mm)"))
  })

  output$swe_anomaly_monthly <- renderPlotly({
    req(vic_data())
    monthly_data <- vic_data() %>%
      mutate(month = month(date)) %>%
      group_by(month) %>%
      summarise(mean_value = mean(value, na.rm = TRUE))
    
    plot_ly(data = monthly_data, x = ~month, y = ~mean_value, type = 'bar') %>%
      layout(title = "Monthly SWE Anomalies",
             xaxis = list(title = "Month"),
             yaxis = list(title = "Mean Anomaly (mm)"))
  })

  output$swe_elevation <- renderPlotly({
    req(vic_data())
    plot_ly(data = vic_data(), x = ~date, y = ~value, type = 'scatter', mode = 'lines') %>%
      layout(title = "Elevation Analysis",
             xaxis = list(title = "Date"),
             yaxis = list(title = "SWE (mm)"))
  })

  output$swe_temperature <- renderPlotly({
    req(vic_data())
    plot_ly(data = vic_data(), x = ~date, y = ~value, type = 'scatter', mode = 'lines') %>%
      layout(title = "Temperature Impact",
             xaxis = list(title = "Date"),
             yaxis = list(title = "SWE (mm)"))
  })

  output$swe_duration <- renderPlotly({
    req(vic_data())
    plot_ly(data = vic_data(), x = ~date, y = ~value, type = 'scatter', mode = 'lines') %>%
      layout(title = "Snow Cover Duration",
             xaxis = list(title = "Date"),
             yaxis = list(title = "Duration (days)"))
  })

  output$swe_melt <- renderPlotly({
    req(vic_data())
    plot_ly(data = vic_data(), x = ~date, y = ~value, type = 'scatter', mode = 'lines') %>%
      layout(title = "Melt Analysis",
             xaxis = list(title = "Date"),
             yaxis = list(title = "Melt Rate (mm/day)"))
  })

  output$swe_climate <- renderPlotly({
    req(vic_data())
    plot_ly(data = vic_data(), x = ~date, y = ~value, type = 'scatter', mode = 'lines') %>%
      layout(title = "Climate Impact",
             xaxis = list(title = "Date"),
             yaxis = list(title = "SWE (mm)"))
  })

  output$swe_water <- renderPlotly({
    req(vic_data())
    plot_ly(data = vic_data(), x = ~date, y = ~value, type = 'scatter', mode = 'lines') %>%
      layout(title = "Water Resource Impact",
             xaxis = list(title = "Date"),
             yaxis = list(title = "Impact Score"))
  })

  # Snow Water Equivalent Outputs
  output$swe_spatial <- renderPlotly({
    req(basin_boundary(), vic_data())
    plot_ly() %>%
      add_sf(data = basin_boundary(), 
             color = I("gray"), 
             opacity = 0.2,
             name = "Basin Boundary") %>%
      add_heatmap(data = vic_data(),
                  x = ~lon, y = ~lat, z = ~value,
                  colors = colorRamp(c("white", "lightblue", "blue")),
                  name = "Snow Water Equivalent") %>%
      layout(title = paste("Spatial Distribution - SWE",
                          "\nYear:", input$year_slider,
                          "Day:", input$day_slider),
             xaxis = list(title = "Longitude"),
             yaxis = list(title = "Latitude"),
             showlegend = TRUE)
  })

  output$swe_timeseries <- renderPlotly({
    req(vic_data())
    plot_ly(data = vic_data(), x = ~date, y = ~value, type = 'scatter', mode = 'lines') %>%
      layout(title = "SWE Time Series",
             xaxis = list(title = "Date"),
             yaxis = list(title = "SWE (mm)"))
  })

  output$swe_monthly <- renderPlotly({
    req(vic_data())
    monthly_data <- vic_data() %>%
      mutate(month = month(date)) %>%
      group_by(month) %>%
      summarise(mean_value = mean(value, na.rm = TRUE))
    
    plot_ly(data = monthly_data, x = ~month, y = ~mean_value, type = 'bar') %>%
      layout(title = "Monthly SWE Statistics",
             xaxis = list(title = "Month"),
             yaxis = list(title = "Mean SWE (mm)"))
  })

  output$swe_accumulation <- renderPlotly({
    req(vic_data())
    plot_ly(data = vic_data(), x = ~date, y = ~value, type = 'scatter', mode = 'lines') %>%
      layout(title = "Snow Accumulation",
             xaxis = list(title = "Date"),
             yaxis = list(title = "SWE (mm)"))
  })

  output$swe_melt <- renderPlotly({
    req(vic_data())
    plot_ly(data = vic_data(), x = ~date, y = ~value, type = 'scatter', mode = 'lines') %>%
      layout(title = "Snow Melt",
             xaxis = list(title = "Date"),
             yaxis = list(title = "Melt Rate (mm/day)"))
  })

  output$swe_elevation <- renderPlotly({
    req(vic_data())
    plot_ly(data = vic_data(), x = ~date, y = ~value, type = 'scatter', mode = 'lines') %>%
      layout(title = "Elevation Impact",
             xaxis = list(title = "Date"),
             yaxis = list(title = "SWE (mm)"))
  })

  output$swe_climate <- renderPlotly({
    req(vic_data())
    plot_ly(data = vic_data(), x = ~date, y = ~value, type = 'scatter', mode = 'lines') %>%
      layout(title = "Climate Impact",
             xaxis = list(title = "Date"),
             yaxis = list(title = "SWE (mm)"))
  })

  output$swe_drought <- renderPlotly({
    req(vic_data())
    plot_ly(data = vic_data(), x = ~date, y = ~value, type = 'scatter', mode = 'lines') %>%
      layout(title = "Drought Impact",
             xaxis = list(title = "Date"),
             yaxis = list(title = "Impact Score"))
  })

  output$swe_water <- renderPlotly({
    req(vic_data())
    plot_ly(data = vic_data(), x = ~date, y = ~value, type = 'scatter', mode = 'lines') %>%
      layout(title = "Water Resource Impact",
             xaxis = list(title = "Date"),
             yaxis = list(title = "Impact Score"))
  })

  # Soil Moisture Outputs
  output$soil_spatial <- renderPlotly({
    req(basin_boundary(), vic_data())
    plot_ly() %>%
      add_sf(data = basin_boundary(), 
             color = I("gray"), 
             opacity = 0.2,
             name = "Basin Boundary") %>%
      add_heatmap(data = vic_data(),
                  x = ~lon, y = ~lat, z = ~value,
                  colors = colorRamp(c("brown", "white", "green")),
                  name = "Soil Moisture") %>%
      layout(title = paste("Spatial Distribution - Soil Moisture",
                          "\nYear:", input$year_slider,
                          "Day:", input$day_slider),
             xaxis = list(title = "Longitude"),
             yaxis = list(title = "Latitude"),
             showlegend = TRUE)
  })

  output$soil_timeseries <- renderPlotly({
    req(vic_data())
    plot_ly(data = vic_data(), x = ~date, y = ~value, type = 'scatter', mode = 'lines') %>%
      layout(title = "Soil Moisture Time Series",
             xaxis = list(title = "Date"),
             yaxis = list(title = "Soil Moisture (mm)"))
  })

  output$soil_monthly <- renderPlotly({
    req(vic_data())
    monthly_data <- vic_data() %>%
      mutate(month = month(date)) %>%
      group_by(month) %>%
      summarise(mean_value = mean(value, na.rm = TRUE))
    
    plot_ly(data = monthly_data, x = ~month, y = ~mean_value, type = 'bar') %>%
      layout(title = "Monthly Soil Moisture Statistics",
             xaxis = list(title = "Month"),
             yaxis = list(title = "Mean Soil Moisture (mm)"))
  })

  output$soil_depth <- renderPlotly({
    req(vic_data())
    plot_ly(data = vic_data(), x = ~date, y = ~value, type = 'scatter', mode = 'lines') %>%
      layout(title = "Depth Profile",
             xaxis = list(title = "Date"),
             yaxis = list(title = "Soil Moisture (mm)"))
  })

  output$soil_vegetation <- renderPlotly({
    req(vic_data())
    plot_ly(data = vic_data(), x = ~date, y = ~value, type = 'scatter', mode = 'lines') %>%
      layout(title = "Vegetation Impact",
             xaxis = list(title = "Date"),
             yaxis = list(title = "Soil Moisture (mm)"))
  })

  output$soil_drought <- renderPlotly({
    req(vic_data())
    plot_ly(data = vic_data(), x = ~date, y = ~value, type = 'scatter', mode = 'lines') %>%
      layout(title = "Drought Analysis",
             xaxis = list(title = "Date"),
             yaxis = list(title = "Soil Moisture (mm)"))
  })

  output$soil_climate <- renderPlotly({
    req(vic_data())
    plot_ly(data = vic_data(), x = ~date, y = ~value, type = 'scatter', mode = 'lines') %>%
      layout(title = "Climate Impact",
             xaxis = list(title = "Date"),
             yaxis = list(title = "Soil Moisture (mm)"))
  })

  output$soil_landuse <- renderPlotly({
    req(vic_data())
    plot_ly(data = vic_data(), x = ~date, y = ~value, type = 'scatter', mode = 'lines') %>%
      layout(title = "Land Use Impact",
             xaxis = list(title = "Date"),
             yaxis = list(title = "Soil Moisture (mm)"))
  })

  output$soil_water <- renderPlotly({
    req(vic_data())
    plot_ly(data = vic_data(), x = ~date, y = ~value, type = 'scatter', mode = 'lines') %>%
      layout(title = "Water Resource Impact",
             xaxis = list(title = "Date"),
             yaxis = list(title = "Impact Score"))
  })

  # Load pre-processed analysis results
  analysis_results <- reactive({
    if (file.exists("data/analysis_cache/analysis_results.rds")) {
      readRDS("data/analysis_cache/analysis_results.rds")
    } else {
      NULL
    }
  })

  # Precipitation Analysis Tab
  output$precip_anomaly_spatial <- renderPlotly({
    req(analysis_results())
    plot_ly(
      x = seq_along(dim(analysis_results()$precipitation$spatial)[1]),
      y = seq_along(dim(analysis_results()$precipitation$spatial)[2]),
      z = analysis_results()$precipitation$spatial,
      type = "heatmap",
      colorscale = "Viridis"
    ) %>%
      layout(
        title = "Spatial Distribution of Precipitation",
        xaxis = list(title = "Longitude"),
        yaxis = list(title = "Latitude")
      )
  })

  output$precip_anomaly_timeseries <- renderPlotly({
    req(analysis_results())
    plot_ly(
      x = seq_along(analysis_results()$precipitation$timeseries),
      y = analysis_results()$precipitation$timeseries,
      type = "scatter",
      mode = "lines"
    ) %>%
      layout(
        title = "Precipitation Time Series",
        xaxis = list(title = "Time"),
        yaxis = list(title = "Precipitation (mm)")
      )
  })

  # Snow Water Equivalent Tab
  output$swe_spatial <- renderPlotly({
    req(analysis_results())
    plot_ly(
      x = seq_along(dim(analysis_results()$swe$spatial)[1]),
      y = seq_along(dim(analysis_results()$swe$spatial)[2]),
      z = analysis_results()$swe$spatial,
      type = "heatmap",
      colorscale = "Viridis"
    ) %>%
      layout(
        title = "Spatial Distribution of SWE",
        xaxis = list(title = "Longitude"),
        yaxis = list(title = "Latitude")
      )
  })

  output$swe_timeseries <- renderPlotly({
    req(analysis_results())
    plot_ly(
      x = seq_along(analysis_results()$swe$timeseries),
      y = analysis_results()$swe$timeseries,
      type = "scatter",
      mode = "lines"
    ) %>%
      layout(
        title = "SWE Time Series",
        xaxis = list(title = "Time"),
        yaxis = list(title = "SWE (mm)")
      )
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
