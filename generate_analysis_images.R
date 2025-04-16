# Load required libraries
library(ncdf4)
library(ggplot2)
library(dplyr)
library(lubridate)
library(sf)
library(raster)
library(terra)
library(gridExtra)
library(scales)
library(viridis)
library(plotly)

# Function to process and plot SMAP data
process_smap_analysis <- function(smap_file, output_dir) {
  nc <- nc_open(smap_file)
  time <- ncvar_get(nc, "time")
  soil_moisture_surface <- ncvar_get(nc, "Geophysical_Data_sm_surface")
  soil_moisture_rootzone <- ncvar_get(nc, "Geophysical_Data_sm_rootzone")
  
  # Calculate means across all dimensions except time
  # First, calculate mean across day_period (dim 1)
  surface_means_day <- apply(soil_moisture_surface, c(2,3,4), mean, na.rm = TRUE)
  rootzone_means_day <- apply(soil_moisture_rootzone, c(2,3,4), mean, na.rm = TRUE)
  
  # Then, calculate mean across lat and lon (now dims 1 and 2)
  surface_means <- apply(surface_means_day, 3, mean, na.rm = TRUE)
  rootzone_means <- apply(rootzone_means_day, 3, mean, na.rm = TRUE)
  
  dates <- as.Date(time, origin = "2000-01-01")
  
  # Create time series plot for surface soil moisture
  p1 <- ggplot(data.frame(date = dates, sm = surface_means), aes(x = date, y = sm)) +
    geom_line(color = "#2c7fb8") +
    theme_minimal() +
    labs(title = "SMAP Surface Soil Moisture Time Series",
         x = "Date", y = "Surface Soil Moisture (mm)") +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y")
  
  # Create monthly average plot for surface soil moisture
  monthly_avg <- data.frame(date = dates, sm = surface_means) %>%
    mutate(month = month(date), year = year(date)) %>%
    group_by(month) %>%
    summarise(avg_sm = mean(sm, na.rm = TRUE))
  
  p2 <- ggplot(monthly_avg, aes(x = month, y = avg_sm)) +
    geom_line(color = "#2c7fb8") +
    geom_point(color = "#2c7fb8") +
    theme_minimal() +
    labs(title = "SMAP Monthly Average Surface Soil Moisture",
         x = "Month", y = "Average Surface Soil Moisture (mm)") +
    scale_x_continuous(breaks = 1:12, labels = month.abb)
  
  # Create time series plot for root zone soil moisture
  p3 <- ggplot(data.frame(date = dates, sm = rootzone_means), aes(x = date, y = sm)) +
    geom_line(color = "#e41a1c") +
    theme_minimal() +
    labs(title = "SMAP Root Zone Soil Moisture Time Series",
         x = "Date", y = "Root Zone Soil Moisture (mm)") +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y")
  
  # Create monthly average plot for root zone soil moisture
  monthly_avg_rz <- data.frame(date = dates, sm = rootzone_means) %>%
    mutate(month = month(date), year = year(date)) %>%
    group_by(month) %>%
    summarise(avg_sm = mean(sm, na.rm = TRUE))
  
  p4 <- ggplot(monthly_avg_rz, aes(x = month, y = avg_sm)) +
    geom_line(color = "#e41a1c") +
    geom_point(color = "#e41a1c") +
    theme_minimal() +
    labs(title = "SMAP Monthly Average Root Zone Soil Moisture",
         x = "Month", y = "Average Root Zone Soil Moisture (mm)") +
    scale_x_continuous(breaks = 1:12, labels = month.abb)
  
  nc_close(nc)
  
  # Save plots
  ggsave(file.path(output_dir, "smap_surface_time_series.png"), p1, width = 12, height = 6, dpi = 300)
  ggsave(file.path(output_dir, "smap_surface_monthly_avg.png"), p2, width = 12, height = 6, dpi = 300)
  ggsave(file.path(output_dir, "smap_rootzone_time_series.png"), p3, width = 12, height = 6, dpi = 300)
  ggsave(file.path(output_dir, "smap_rootzone_monthly_avg.png"), p4, width = 12, height = 6, dpi = 300)
}

# Function to process and plot GRACE data
process_grace_analysis <- function(grace_file, output_dir) {
  nc <- nc_open(grace_file)
  print("GRACE variables:")
  print(names(nc$var))
  time <- ncvar_get(nc, "time")
  # Try different possible variable names for TWS
  tryCatch({
    tws <- ncvar_get(nc, "lwe_thickness")
  }, error = function(e) {
    tryCatch({
      tws <- ncvar_get(nc, "TWS")
    }, error = function(e) {
      tryCatch({
        tws <- ncvar_get(nc, "GRACE_TWS")
      }, error = function(e) {
        stop("Could not find TWS variable in GRACE file")
      })
    })
  })
  
  # Calculate spatial mean for each time step
  if (length(dim(tws)) == 3) {
    tws_means <- apply(tws, 3, mean, na.rm = TRUE)
  } else {
    tws_means <- apply(tws, 1, mean, na.rm = TRUE)
  }
  
  dates <- as.Date(time, origin = "2002-01-01")
  
  # Create time series plot
  p1 <- ggplot(data.frame(date = dates, tws = tws_means), aes(x = date, y = tws)) +
    geom_line(color = "#e41a1c") +
    theme_minimal() +
    labs(title = "GRACE TWS Time Series",
         x = "Date", y = "Terrestrial Water Storage (mm)") +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y")
  
  # Create seasonal decomposition
  seasonal_data <- data.frame(date = dates, tws = tws_means) %>%
    mutate(month = month(date)) %>%
    group_by(month) %>%
    summarise(avg_tws = mean(tws, na.rm = TRUE))
  
  p2 <- ggplot(seasonal_data, aes(x = month, y = avg_tws)) +
    geom_line(color = "#e41a1c") +
    geom_point(color = "#e41a1c") +
    theme_minimal() +
    labs(title = "GRACE Monthly Average TWS",
         x = "Month", y = "Average TWS (mm)") +
    scale_x_continuous(breaks = 1:12, labels = month.abb)
  
  nc_close(nc)
  
  # Save plots
  ggsave(file.path(output_dir, "grace_time_series.png"), p1, width = 12, height = 6, dpi = 300)
  ggsave(file.path(output_dir, "grace_monthly_avg.png"), p2, width = 12, height = 6, dpi = 300)
}

# Function to read and process VIC data
process_vic_data <- function() {
  # Read VIC output
  nc <- nc_open("data/VICOut2.nc")
  
  # Get dimensions
  lon <- ncvar_get(nc, "lon")
  lat <- ncvar_get(nc, "lat")
  time <- ncvar_get(nc, "time")
  
  # Convert time to dates
  dates <- as.Date(time, origin = "0001-01-01")
  
  # Extract key variables
  variables <- list(
    "Precipitation" = ncvar_get(nc, "OUT_PREC"),
    "Snow Water Equivalent" = ncvar_get(nc, "OUT_SWE"),
    "Runoff" = ncvar_get(nc, "OUT_RUNOFF"),
    "Baseflow" = ncvar_get(nc, "OUT_BASEFLOW"),
    "Evapotranspiration" = ncvar_get(nc, "OUT_EVAP")
  )
  
  # Extract soil moisture for each layer
  soil_moisture <- ncvar_get(nc, "OUT_SOIL_MOIST")
  for (i in 1:3) {
    variables[[paste0("Soil Moisture Layer ", i)]] <- soil_moisture[,,i,]
  }
  
  nc_close(nc)
  
  return(list(
    variables = variables,
    dates = dates,
    lon = lon,
    lat = lat
  ))
}

# Function to create spatial plots
create_spatial_plot <- function(data, lon, lat, title, color_scale = "viridis") {
  # Create a data frame with coordinates and values
  if (length(dim(data)) > 2) {
    # If data has more than 2 dimensions, calculate mean across time
    data <- apply(data, c(1,2), mean, na.rm = TRUE)
  }
  
  # Create coordinate grid
  coords <- expand.grid(lon = lon, lat = lat)
  coords$value <- as.vector(data)
  
  # Remove NA values
  coords <- coords[!is.na(coords$value),]
  
  ggplot(coords, aes(x = lon, y = lat, fill = value)) +
    geom_raster() +
    scale_fill_viridis_c(option = color_scale) +
    labs(title = title, x = "Longitude", y = "Latitude") +
    theme_minimal()
}

# Function to create time series plots
create_time_series <- function(data, dates, title) {
  df <- data.frame(
    Date = dates,
    Value = data
  )
  
  ggplot(df, aes(x = Date, y = Value)) +
    geom_line() +
    labs(title = title) +
    theme_minimal()
}

# Function to create monthly statistics
create_monthly_stats <- function(data, dates, title) {
  df <- data.frame(
    Date = dates,
    Value = data,
    Month = month(dates, label = TRUE)
  )
  
  monthly_stats <- df %>%
    group_by(Month) %>%
    summarise(
      Mean = mean(Value, na.rm = TRUE),
      SD = sd(Value, na.rm = TRUE)
    )
  
  ggplot(monthly_stats, aes(x = Month, y = Mean)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = 0.2) +
    labs(title = title) +
    theme_minimal()
}

# Function to process basin shapefiles and create maps
process_basin_analysis <- function(basin_dir, output_dir) {
  # Read basin shapefiles
  basin_files <- list.files(basin_dir, pattern = "\\.shp$", full.names = TRUE)
  
  for (file in basin_files) {
    basin <- st_read(file, quiet = TRUE)
    basin_name <- tools::file_path_sans_ext(basename(file))
    
    # Create basic basin map
    p <- ggplot() +
      geom_sf(data = basin, fill = "#377eb8", color = "black") +
      theme_minimal() +
      labs(title = paste("Basin:", basin_name))
    
    # Save plot
    ggsave(file.path(output_dir, paste0("basin_", basin_name, ".png")), 
           p, width = 10, height = 8, dpi = 300)
  }
}

# Main function to generate all analysis images
generate_analysis_images <- function() {
  # Process VIC data
  vic_data <- process_vic_data()
  
  # Create output directory if it doesn't exist
  output_dir <- "images/analysis_results"
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Generate plots for each variable
  for (var_name in names(vic_data$variables)) {
    # Spatial distribution
    spatial_mean <- apply(vic_data$variables[[var_name]], c(1,2), mean, na.rm = TRUE)
    p <- create_spatial_plot(spatial_mean, vic_data$lon, vic_data$lat, 
                           paste("Spatial Distribution of", var_name))
    ggsave(file.path(output_dir, paste0(gsub(" ", "_", tolower(var_name)), "_spatial.png")), p)
    
    # Time series
    time_series <- apply(vic_data$variables[[var_name]], 3, mean, na.rm = TRUE)
    p <- create_time_series(time_series, vic_data$dates, paste("Time Series of", var_name))
    ggsave(file.path(output_dir, paste0(gsub(" ", "_", tolower(var_name)), "_timeseries.png")), p)
    
    # Monthly statistics
    p <- create_monthly_stats(time_series, vic_data$dates, paste("Monthly Statistics of", var_name))
    ggsave(file.path(output_dir, paste0(gsub(" ", "_", tolower(var_name)), "_monthly.png")), p)
  }
  
  # Temperature analysis
  temp_data <- nc_open("data/CRB_PRISM_Calibrated.2024-01-01.nc")
  print("Temperature variables:")
  print(names(temp_data$var))
  
  # Read temperature data
  temp <- ncvar_get(temp_data, "OUT_AIR_TEMP")
  temp_lon <- ncvar_get(temp_data, "lon")
  temp_lat <- ncvar_get(temp_data, "lat")
  temp_time <- ncvar_get(temp_data, "time")
  temp_dates <- as.Date(temp_time, origin = "0001-01-01")
  nc_close(temp_data)
  
  # Temperature statistics
  temp_stats <- data.frame(
    Mean = mean(temp, na.rm = TRUE),
    SD = sd(temp, na.rm = TRUE),
    Min = min(temp, na.rm = TRUE),
    Max = max(temp, na.rm = TRUE)
  )
  
  # Save temperature statistics
  write.csv(temp_stats, file.path(output_dir, "temperature_statistics.csv"))
  
  # Temperature spatial distribution
  p <- create_spatial_plot(temp, temp_lon, temp_lat, "Spatial Distribution of Air Temperature")
  ggsave(file.path(output_dir, "temperature_spatial.png"), p)
  
  # Temperature time series
  temp_series <- apply(temp, 3, mean, na.rm = TRUE)
  p <- create_time_series(temp_series, temp_dates, "Air Temperature Time Series")
  ggsave(file.path(output_dir, "temperature_timeseries.png"), p)
  
  # Monthly temperature statistics
  p <- create_monthly_stats(temp_series, temp_dates, "Monthly Air Temperature Statistics")
  ggsave(file.path(output_dir, "temperature_monthly.png"), p)
  
  # Diurnal variation
  if (length(dim(temp)) > 3) {
    # Calculate mean diurnal cycle if hourly data is available
    hours <- 1:24
    diurnal_mean <- apply(temp, c(1,2,4), mean, na.rm = TRUE)
    diurnal_series <- apply(diurnal_mean, 3, mean, na.rm = TRUE)
    
    p <- ggplot(data.frame(Hour = hours, Temperature = diurnal_series), 
                aes(x = Hour, y = Temperature)) +
      geom_line() +
      labs(title = "Mean Diurnal Temperature Variation") +
      theme_minimal()
    ggsave(file.path(output_dir, "temperature_diurnal.png"), p)
  }
  
  # Elevation gradient analysis
  if (file.exists("data/elevation.tif")) {
    elevation_data <- raster("data/elevation.tif")
    p <- ggplot() +
      geom_raster(data = as.data.frame(elevation_data, xy = TRUE), 
                 aes(x = x, y = y, fill = layer)) +
      scale_fill_viridis_c(option = "magma") +
      labs(title = "Elevation Gradient") +
      theme_minimal()
    ggsave(file.path(output_dir, "elevation_gradient.png"), p)
  }
  
  print("Analysis images generated successfully!")
}

# Run the analysis
generate_analysis_images() 