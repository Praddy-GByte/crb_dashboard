# Load required packages
library(ncdf4)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(plotly)
library(leaflet)
library(sf)
library(raster)
library(terra)
library(stars)
library(arrow)
library(jsonlite)

# Create output directories
dirs <- c("data/VIC_outputs", "data/SMAP_outputs", "data/GRACE_outputs", "images/dashboard")
for (dir in dirs) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
    cat("Created directory:", dir, "\n")
  }
}

# Function to create interactive map with all data layers
create_interactive_map <- function() {
  # Read basin boundary
  basin <- st_read("data/CRB_poly/basin_CRB_poly.shp", quiet = TRUE)
  
  # Read HUC10 watersheds
  huc10 <- st_read("data/huc10s/huc10.shp", quiet = TRUE)
  
  # Create base map
  m <- leaflet() %>%
    addTiles() %>%
    addPolygons(data = basin, 
                color = "#8C1D40", 
                weight = 2,
                fillOpacity = 0.1,
                group = "Basin Boundary") %>%
    addPolygons(data = huc10,
                color = "#FFC627",
                weight = 1,
                fillOpacity = 0.1,
                group = "HUC10 Watersheds")
  
  # Add VIC model outputs
  vic_vars <- c("OUT_PREC", "OUT_SWE", "OUT_RUNOFF", "OUT_BASEFLOW", 
                "OUT_EVAP", "OUT_SOIL_MOIST")
  
  for (var in vic_vars) {
    file_path <- paste0("data/VIC_outputs/", var, "_annual.tif")
    if (file.exists(file_path)) {
      r <- rast(file_path)
      m <- m %>%
        addRasterImage(r, 
                      colors = "Spectral",
                      opacity = 0.7,
                      group = paste("VIC", var))
    }
  }
  
  # Add SMAP data
  smap_vars <- c(
    "Geophysical_Data_sm_surface",
    "Geophysical_Data_sm_rootzone",
    "Geophysical_Data_sm_profile"
  )
  smap_names <- c("Surface", "Root Zone", "Profile")
  
  for (i in seq_along(smap_vars)) {
    nc <- nc_open("data/SPL4SMGP.007_9km_aid0001.nc")
    var_data <- ncvar_get(nc, smap_vars[i])
    # Average over time and day_period dimensions
    mean_data <- apply(var_data, c(2, 3), mean, na.rm = TRUE)
    # Create a raster
    r <- raster(t(mean_data), 
                xmn = min(ncvar_get(nc, "lon")), 
                xmx = max(ncvar_get(nc, "lon")),
                ymn = min(ncvar_get(nc, "lat")), 
                ymx = max(ncvar_get(nc, "lat")),
                crs = "+proj=longlat +datum=WGS84")
    nc_close(nc)
    
    # Save as GeoTIFF
    writeRaster(r, 
                filename = paste0("data/SMAP_outputs/smap_", tolower(smap_names[i]), "_2024.tif"),
                format = "GTiff",
                overwrite = TRUE)
  }
  
  # Update the map creation code
  for (i in seq_along(smap_names)) {
    file_path <- paste0("data/SMAP_outputs/smap_", tolower(smap_names[i]), "_2024.tif")
    if (file.exists(file_path)) {
      r <- rast(file_path)
      m <- m %>%
        addRasterImage(r,
                      colors = "Spectral",
                      opacity = 0.7,
                      group = paste("SMAP", smap_names[i]))
    }
  }
  
  # Process GRACE data
  grace_file <- "data/GRCTellus.JPL.200204_202401.GLO.RL06.1M.MSCNv03CRI.nc"
  if (file.exists(grace_file)) {
    nc <- nc_open(grace_file)
    # Get the latest time step's data
    time <- ncvar_get(nc, "time")
    lwe <- ncvar_get(nc, "lwe_thickness")
    # Get the last time step that's not missing
    last_valid <- max(which(!is.na(apply(lwe, 1, mean))))
    latest_data <- lwe[last_valid, , ]
    
    # Create a raster
    r <- raster(t(latest_data),
                xmn = min(ncvar_get(nc, "lon")),
                xmx = max(ncvar_get(nc, "lon")),
                ymn = min(ncvar_get(nc, "lat")),
                ymx = max(ncvar_get(nc, "lat")),
                crs = "+proj=longlat +datum=WGS84")
    
    # Save as GeoTIFF
    writeRaster(r,
                filename = "data/GRACE_outputs/GRACE_2024_annual.tif",
                format = "GTiff",
                overwrite = TRUE)
    nc_close(nc)
  }
  
  # Add layer control
  m <- m %>%
    addLayersControl(
      baseGroups = c("Basin Boundary", "HUC10 Watersheds"),
      overlayGroups = c(paste("VIC", vic_vars),
                       paste("SMAP", smap_names),
                       "GRACE TWS"),
      options = layersControlOptions(collapsed = FALSE)
    )
  
  return(m)
}

# Function to create time series plots
create_time_series_plots <- function() {
  # Read VIC data
  nc <- nc_open("data/VICOut2.nc")
  time <- ncvar_get(nc, "time")
  dates <- as.Date(time, origin = "0001-01-01")
  
  # Create data frame with all variables
  vic_data <- data.frame(
    date = dates,
    precipitation = apply(ncvar_get(nc, "OUT_PREC"), 3, mean, na.rm = TRUE),
    swe = apply(ncvar_get(nc, "OUT_SWE"), 3, mean, na.rm = TRUE),
    runoff = apply(ncvar_get(nc, "OUT_RUNOFF"), 3, mean, na.rm = TRUE),
    baseflow = apply(ncvar_get(nc, "OUT_BASEFLOW"), 3, mean, na.rm = TRUE),
    evap = apply(ncvar_get(nc, "OUT_EVAP"), 3, mean, na.rm = TRUE),
    soil_moisture = apply(ncvar_get(nc, "OUT_SOIL_MOIST"), 4, mean, na.rm = TRUE)
  )
  
  nc_close(nc)
  
  # Create monthly averages
  vic_monthly <- vic_data %>%
    mutate(year_month = floor_date(date, "month")) %>%
    group_by(year_month) %>%
    summarise(across(-date, mean, na.rm = TRUE))
  
  # Create annual averages
  vic_annual <- vic_data %>%
    mutate(year = year(date)) %>%
    group_by(year) %>%
    summarise(across(-date, mean, na.rm = TRUE))
  
  # Create plots
  plots <- list()
  
  # Monthly time series
  p_monthly <- ggplot(vic_monthly, aes(x = year_month)) +
    geom_line(aes(y = precipitation, color = "Precipitation")) +
    geom_line(aes(y = swe, color = "Snow Water Equivalent")) +
    geom_line(aes(y = runoff, color = "Runoff")) +
    geom_line(aes(y = baseflow, color = "Baseflow")) +
    geom_line(aes(y = evap, color = "Evapotranspiration")) +
    geom_line(aes(y = soil_moisture, color = "Soil Moisture")) +
    theme_minimal() +
    labs(title = "Monthly Averages",
         x = "Date",
         y = "Value (mm)",
         color = "Variable") +
    scale_color_manual(values = c(
      "Precipitation" = "#1f77b4",
      "Snow Water Equivalent" = "#2ca02c",
      "Runoff" = "#d62728",
      "Baseflow" = "#9467bd",
      "Evapotranspiration" = "#ff7f0e",
      "Soil Moisture" = "#8c564b"
    ))
  
  # Annual time series
  p_annual <- ggplot(vic_annual, aes(x = year)) +
    geom_line(aes(y = precipitation, color = "Precipitation")) +
    geom_line(aes(y = swe, color = "Snow Water Equivalent")) +
    geom_line(aes(y = runoff, color = "Runoff")) +
    geom_line(aes(y = baseflow, color = "Baseflow")) +
    geom_line(aes(y = evap, color = "Evapotranspiration")) +
    geom_line(aes(y = soil_moisture, color = "Soil Moisture")) +
    theme_minimal() +
    labs(title = "Annual Averages",
         x = "Year",
         y = "Value (mm)",
         color = "Variable") +
    scale_color_manual(values = c(
      "Precipitation" = "#1f77b4",
      "Snow Water Equivalent" = "#2ca02c",
      "Runoff" = "#d62728",
      "Baseflow" = "#9467bd",
      "Evapotranspiration" = "#ff7f0e",
      "Soil Moisture" = "#8c564b"
    ))
  
  return(list(monthly = p_monthly, annual = p_annual))
}

# Function to create correlation plots
create_correlation_plots <- function() {
  # Read VIC data
  nc <- nc_open("data/VICOut2.nc")
  time <- ncvar_get(nc, "time")
  dates <- as.Date(time, origin = "0001-01-01")
  
  # Create data frame with all variables
  vic_data <- data.frame(
    date = dates,
    precipitation = apply(ncvar_get(nc, "OUT_PREC"), 3, mean, na.rm = TRUE),
    swe = apply(ncvar_get(nc, "OUT_SWE"), 3, mean, na.rm = TRUE),
    runoff = apply(ncvar_get(nc, "OUT_RUNOFF"), 3, mean, na.rm = TRUE),
    baseflow = apply(ncvar_get(nc, "OUT_BASEFLOW"), 3, mean, na.rm = TRUE),
    evap = apply(ncvar_get(nc, "OUT_EVAP"), 3, mean, na.rm = TRUE),
    soil_moisture = apply(ncvar_get(nc, "OUT_SOIL_MOIST"), 4, mean, na.rm = TRUE)
  )
  
  nc_close(nc)
  
  # Calculate correlations
  cor_matrix <- cor(vic_data[, -1], use = "complete.obs")
  
  # Create correlation plot
  p_cor <- ggplot(data = reshape2::melt(cor_matrix), 
                 aes(x = Var1, y = Var2, fill = value)) +
    geom_tile() +
    scale_fill_gradient2(low = "#d62728", 
                        mid = "white", 
                        high = "#1f77b4",
                        midpoint = 0,
                        limits = c(-1, 1)) +
    theme_minimal() +
    labs(title = "Correlation Matrix",
         x = "Variable",
         y = "Variable",
         fill = "Correlation") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  return(p_cor)
}

# Function to create trend analysis plots
create_trend_plots <- function() {
  # Read VIC data
  nc <- nc_open("data/VICOut2.nc")
  time <- ncvar_get(nc, "time")
  dates <- as.Date(time, origin = "0001-01-01")
  
  # Create data frame with all variables
  vic_data <- data.frame(
    date = dates,
    precipitation = apply(ncvar_get(nc, "OUT_PREC"), 3, mean, na.rm = TRUE),
    swe = apply(ncvar_get(nc, "OUT_SWE"), 3, mean, na.rm = TRUE),
    runoff = apply(ncvar_get(nc, "OUT_RUNOFF"), 3, mean, na.rm = TRUE),
    baseflow = apply(ncvar_get(nc, "OUT_BASEFLOW"), 3, mean, na.rm = TRUE),
    evap = apply(ncvar_get(nc, "OUT_EVAP"), 3, mean, na.rm = TRUE),
    soil_moisture = apply(ncvar_get(nc, "OUT_SOIL_MOIST"), 4, mean, na.rm = TRUE)
  )
  
  nc_close(nc)
  
  # Calculate annual trends
  vic_annual <- vic_data %>%
    mutate(year = year(date)) %>%
    group_by(year) %>%
    summarise(across(-date, mean, na.rm = TRUE))
  
  # Create trend plots
  plots <- list()
  for (var in names(vic_annual)[-1]) {
    p <- ggplot(vic_annual, aes(x = year, y = !!sym(var))) +
      geom_point() +
      geom_smooth(method = "lm", se = TRUE) +
      theme_minimal() +
      labs(title = paste("Trend Analysis:", var),
           x = "Year",
           y = "Value (mm)")
    plots[[var]] <- p
  }
  
  return(plots)
}

# Main function to generate all visualizations
generate_all_visualizations <- function() {
  # Create output directory
  dir.create("images/dashboard", showWarnings = FALSE, recursive = TRUE)
  
  # Generate interactive map
  m <- create_interactive_map()
  htmlwidgets::saveWidget(m, "images/dashboard/interactive_map.html", selfcontained = TRUE)
  
  # Generate time series plots
  ts_plots <- create_time_series_plots()
  ggsave("images/dashboard/monthly_timeseries.png", ts_plots$monthly, 
         width = 12, height = 6, dpi = 300)
  ggsave("images/dashboard/annual_timeseries.png", ts_plots$annual, 
         width = 12, height = 6, dpi = 300)
  
  # Generate correlation plot
  p_cor <- create_correlation_plots()
  ggsave("images/dashboard/correlation_matrix.png", p_cor, 
         width = 10, height = 8, dpi = 300)
  
  # Generate trend plots
  trend_plots <- create_trend_plots()
  for (var in names(trend_plots)) {
    ggsave(paste0("images/dashboard/trend_", var, ".png"), trend_plots[[var]], 
           width = 10, height = 6, dpi = 300)
  }
}

# Execute the main function
generate_all_visualizations() 