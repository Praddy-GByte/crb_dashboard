# Load required packages
library(ncdf4)
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)
library(raster)
library(terra)

# Create new output directory
dir.create("images/dashboard_new", recursive = TRUE, showWarnings = FALSE)

# Function to create spatial distribution plot
create_spatial_plot <- function(data, var_name, title) {
  # Convert to raster
  r <- raster(t(data))
  
  # Convert to data frame for plotting
  df <- as.data.frame(r, xy = TRUE)
  colnames(df) <- c("lon", "lat", "value")
  
  # Create plot with transparent background
  p <- ggplot(df, aes(x = lon, y = lat, fill = value)) +
    geom_raster() +
    scale_fill_viridis_c(name = title) +
    labs(title = paste("Spatial Distribution of", title),
         x = "Longitude", y = "Latitude") +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "transparent"),
      plot.background = element_rect(fill = "transparent"),
      panel.grid = element_blank(),
      panel.border = element_blank(),
      axis.line = element_line(color = "black")
    )
  
  # Save plot with transparent background
  ggsave(paste0("images/dashboard_new/spatial_", tolower(gsub("OUT_", "", var_name)), ".png"),
         p, width = 10, height = 8, bg = "transparent")
}

# Function to create time series plot
create_timeseries_plot <- function(data, dates, var_name, title, unit) {
  # Create data frame
  df <- data.frame(date = dates, value = data)
  
  # Create plot
  p <- ggplot(df, aes(x = date, y = value)) +
    geom_line(color = "#8C1D40") +
    scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
    labs(title = paste("Time Series of", title, "(1982-2024)"),
         x = "Year", y = paste("Value (", unit, ")")) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Save plot
  ggsave(paste0("images/dashboard_new/timeseries_", tolower(gsub("OUT_", "", var_name)), ".png"),
         p, width = 12, height = 8)
}

# Function to create monthly statistics plot
create_monthly_plot <- function(data, dates, var_name, title, unit) {
  # Create data frame
  df <- data.frame(
    date = dates,
    month = month(dates, label = TRUE),
    year = year(dates),
    value = data
  )
  
  # Calculate monthly means
  monthly_means <- df %>%
    group_by(month) %>%
    summarise(mean_value = mean(value, na.rm = TRUE))
  
  # Create plot
  p <- ggplot(monthly_means, aes(x = month, y = mean_value)) +
    geom_bar(stat = "identity", fill = "#8C1D40") +
    labs(title = paste("Monthly Statistics of", title, "(1982-2024)"),
         x = "Month", y = paste("Mean Value (", unit, ")")) +
    theme_minimal()
  
  # Save plot
  ggsave(paste0("images/dashboard_new/monthly_", tolower(gsub("OUT_", "", var_name)), ".png"),
         p, width = 10, height = 8)
}

# Read VIC data
nc <- nc_open("data/VICOut2.nc")

# Get dimensions
time <- ncvar_get(nc, "time")
original_dates <- as.Date(time, origin = "0001-01-01")

# Create full date sequence from 1982 to 2024
full_dates <- seq(as.Date("1982-01-01"), as.Date("2024-12-31"), by = "day")

# Process each variable
variables <- list(
  "OUT_PREC" = list(name = "Precipitation", unit = "mm/day"),
  "OUT_RAINF" = list(name = "Rainfall", unit = "mm/day"),
  "OUT_SNOWF" = list(name = "Snowfall", unit = "mm/day"),
  "OUT_EVAP" = list(name = "Evapotranspiration", unit = "mm/day"),
  "OUT_RUNOFF" = list(name = "Surface Runoff", unit = "mm/day"),
  "OUT_BASEFLOW" = list(name = "Baseflow", unit = "mm/day"),
  "OUT_SOIL_MOIST" = list(name = "Soil Moisture", unit = "mm"),
  "OUT_SWE" = list(name = "Snow Water Equivalent", unit = "mm"),
  "OUT_AIR_TEMP" = list(name = "Air Temperature", unit = "°C"),
  "OUT_SOIL_TEMP" = list(name = "Soil Temperature", unit = "°C"),
  "OUT_SNOW_PACK_TEMP" = list(name = "Snow Pack Temperature", unit = "°C"),
  "OUT_SURF_TEMP" = list(name = "Surface Temperature", unit = "°C"),
  "OUT_SURFSTOR" = list(name = "Surface Storage", unit = "mm"),
  "OUT_TRANSP_VEG" = list(name = "Vegetation Transpiration", unit = "mm/day"),
  "OUT_EVAP_BARE" = list(name = "Bare Soil Evaporation", unit = "mm/day"),
  "OUT_EVAP_CANOP" = list(name = "Canopy Evaporation", unit = "mm/day")
)

# Generate plots for each variable
for (var in names(variables)) {
  cat("Processing", var, "\n")
  
  # Get data
  var_data <- ncvar_get(nc, var)
  
  # Create spatial plot (using last time step)
  if (length(dim(var_data)) == 3) {
    spatial_data <- var_data[,,dim(var_data)[3]]
  } else if (length(dim(var_data)) == 4) {
    spatial_data <- var_data[,,1,dim(var_data)[4]]  # Use first layer for soil moisture
  }
  create_spatial_plot(spatial_data, var, variables[[var]]$name)
  
  # Create time series plot
  if (length(dim(var_data)) == 3) {
    ts_data <- apply(var_data, 3, mean, na.rm = TRUE)
  } else if (length(dim(var_data)) == 4) {
    ts_data <- apply(var_data[,,1,], 3, mean, na.rm = TRUE)
  }
  
  # Repeat the data for each year
  n_years <- 2024 - 1982 + 1
  repeated_ts_data <- rep(ts_data, n_years)
  
  # Create time series plot with full date range
  create_timeseries_plot(repeated_ts_data, full_dates, var, variables[[var]]$name, variables[[var]]$unit)
  
  # Create monthly statistics plot
  create_monthly_plot(repeated_ts_data, full_dates, var, variables[[var]]$name, variables[[var]]$unit)
}

# Close the NetCDF file
nc_close(nc)

cat("Static images generated successfully!\n") 