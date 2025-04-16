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
library(trend)
library(corrplot)
library(ggpubr)

# Function to process water balance components
process_water_balance <- function(vic_file, output_dir) {
  nc <- nc_open(vic_file)
  
  # Get all water balance components
  time <- ncvar_get(nc, "time")
  precip <- ncvar_get(nc, "OUT_PREC")
  evap <- ncvar_get(nc, "OUT_EVAP")
  runoff <- ncvar_get(nc, "OUT_RUNOFF")
  baseflow <- ncvar_get(nc, "OUT_BASEFLOW")
  
  # Calculate spatial means
  precip_mean <- apply(precip, 3, mean, na.rm = TRUE)
  evap_mean <- apply(evap, 3, mean, na.rm = TRUE)
  runoff_mean <- apply(runoff, 3, mean, na.rm = TRUE)
  baseflow_mean <- apply(baseflow, 3, mean, na.rm = TRUE)
  
  dates <- as.Date(time, origin = "0001-01-01")
  
  # Create water balance data frame
  wb_data <- data.frame(
    date = dates,
    precipitation = precip_mean,
    evapotranspiration = evap_mean,
    runoff = runoff_mean,
    baseflow = baseflow_mean
  )
  
  # Create monthly water balance plot
  monthly_wb <- wb_data %>%
    mutate(month = month(date)) %>%
    group_by(month) %>%
    summarise(
      avg_precip = mean(precipitation, na.rm = TRUE),
      avg_evap = mean(evapotranspiration, na.rm = TRUE),
      avg_runoff = mean(runoff, na.rm = TRUE),
      avg_baseflow = mean(baseflow, na.rm = TRUE)
    )
  
  p1 <- ggplot(monthly_wb, aes(x = month)) +
    geom_line(aes(y = avg_precip, color = "Precipitation")) +
    geom_line(aes(y = avg_evap, color = "Evapotranspiration")) +
    geom_line(aes(y = avg_runoff, color = "Runoff")) +
    geom_line(aes(y = avg_baseflow, color = "Baseflow")) +
    theme_minimal() +
    labs(title = "Monthly Water Balance Components",
         x = "Month", y = "Value (mm)") +
    scale_x_continuous(breaks = 1:12, labels = month.abb) +
    scale_color_manual(values = c("Precipitation" = "#377eb8",
                                 "Evapotranspiration" = "#4daf4a",
                                 "Runoff" = "#e41a1c",
                                 "Baseflow" = "#984ea3")) +
    theme(legend.position = "bottom")
  
  # Create annual water balance plot
  annual_wb <- wb_data %>%
    mutate(year = year(date)) %>%
    group_by(year) %>%
    summarise(
      total_precip = sum(precipitation, na.rm = TRUE),
      total_evap = sum(evapotranspiration, na.rm = TRUE),
      total_runoff = sum(runoff, na.rm = TRUE),
      total_baseflow = sum(baseflow, na.rm = TRUE)
    )
  
  p2 <- ggplot(annual_wb, aes(x = year)) +
    geom_line(aes(y = total_precip, color = "Precipitation")) +
    geom_line(aes(y = total_evap, color = "Evapotranspiration")) +
    geom_line(aes(y = total_runoff, color = "Runoff")) +
    geom_line(aes(y = total_baseflow, color = "Baseflow")) +
    theme_minimal() +
    labs(title = "Annual Water Balance Components",
         x = "Year", y = "Value (mm)") +
    scale_color_manual(values = c("Precipitation" = "#377eb8",
                                 "Evapotranspiration" = "#4daf4a",
                                 "Runoff" = "#e41a1c",
                                 "Baseflow" = "#984ea3")) +
    theme(legend.position = "bottom")
  
  nc_close(nc)
  
  # Save plots
  ggsave(file.path(output_dir, "water_balance_monthly.png"), p1, width = 12, height = 6, dpi = 300)
  ggsave(file.path(output_dir, "water_balance_annual.png"), p2, width = 12, height = 6, dpi = 300)
}

# Function to perform trend analysis
process_trend_analysis <- function(vic_file, output_dir) {
  nc <- nc_open(vic_file)
  
  # Get variables for trend analysis
  time <- ncvar_get(nc, "time")
  soil_moisture <- ncvar_get(nc, "OUT_SOIL_MOIST")
  swe <- ncvar_get(nc, "OUT_SWE")
  precip <- ncvar_get(nc, "OUT_PREC")
  evap <- ncvar_get(nc, "OUT_EVAP")
  
  # Calculate spatial means
  sm_mean <- apply(soil_moisture, 4, mean, na.rm = TRUE)
  swe_mean <- apply(swe, 3, mean, na.rm = TRUE)
  precip_mean <- apply(precip, 3, mean, na.rm = TRUE)
  evap_mean <- apply(evap, 3, mean, na.rm = TRUE)
  
  dates <- as.Date(time, origin = "0001-01-01")
  
  # Create trend data frame
  trend_data <- data.frame(
    date = dates,
    soil_moisture = sm_mean,
    swe = swe_mean,
    precipitation = precip_mean,
    evapotranspiration = evap_mean
  )
  
  # Calculate annual means
  annual_trends <- trend_data %>%
    mutate(year = year(date)) %>%
    group_by(year) %>%
    summarise(
      avg_sm = mean(soil_moisture, na.rm = TRUE),
      avg_swe = mean(swe, na.rm = TRUE),
      avg_precip = mean(precipitation, na.rm = TRUE),
      avg_evap = mean(evapotranspiration, na.rm = TRUE)
    )
  
  # Create trend plots with linear regression
  p1 <- ggplot(annual_trends, aes(x = year, y = avg_sm)) +
    geom_point() +
    geom_smooth(method = "lm", color = "#377eb8") +
    theme_minimal() +
    labs(title = "Soil Moisture Trend",
         x = "Year", y = "Average Soil Moisture (mm)")
  
  p2 <- ggplot(annual_trends, aes(x = year, y = avg_swe)) +
    geom_point() +
    geom_smooth(method = "lm", color = "#4daf4a") +
    theme_minimal() +
    labs(title = "Snow Water Equivalent Trend",
         x = "Year", y = "Average SWE (mm)")
  
  p3 <- ggplot(annual_trends, aes(x = year, y = avg_precip)) +
    geom_point() +
    geom_smooth(method = "lm", color = "#e41a1c") +
    theme_minimal() +
    labs(title = "Precipitation Trend",
         x = "Year", y = "Average Precipitation (mm)")
  
  p4 <- ggplot(annual_trends, aes(x = year, y = avg_evap)) +
    geom_point() +
    geom_smooth(method = "lm", color = "#984ea3") +
    theme_minimal() +
    labs(title = "Evapotranspiration Trend",
         x = "Year", y = "Average Evapotranspiration (mm)")
  
  nc_close(nc)
  
  # Save plots
  ggsave(file.path(output_dir, "trend_soil_moisture.png"), p1, width = 12, height = 6, dpi = 300)
  ggsave(file.path(output_dir, "trend_swe.png"), p2, width = 12, height = 6, dpi = 300)
  ggsave(file.path(output_dir, "trend_precipitation.png"), p3, width = 12, height = 6, dpi = 300)
  ggsave(file.path(output_dir, "trend_evapotranspiration.png"), p4, width = 12, height = 6, dpi = 300)
}

# Function to perform correlation analysis
process_correlation_analysis <- function(vic_file, output_dir) {
  nc <- nc_open(vic_file)
  
  # Get variables for correlation analysis
  time <- ncvar_get(nc, "time")
  soil_moisture <- ncvar_get(nc, "OUT_SOIL_MOIST")
  swe <- ncvar_get(nc, "OUT_SWE")
  precip <- ncvar_get(nc, "OUT_PREC")
  evap <- ncvar_get(nc, "OUT_EVAP")
  temp <- ncvar_get(nc, "OUT_AIR_TEMP")
  
  # Calculate spatial means
  sm_mean <- apply(soil_moisture, 4, mean, na.rm = TRUE)
  swe_mean <- apply(swe, 3, mean, na.rm = TRUE)
  precip_mean <- apply(precip, 3, mean, na.rm = TRUE)
  evap_mean <- apply(evap, 3, mean, na.rm = TRUE)
  temp_mean <- apply(temp, 3, mean, na.rm = TRUE)
  
  dates <- as.Date(time, origin = "0001-01-01")
  
  # Create correlation data frame
  corr_data <- data.frame(
    date = dates,
    soil_moisture = sm_mean,
    swe = swe_mean,
    precipitation = precip_mean,
    evapotranspiration = evap_mean,
    temperature = temp_mean
  )
  
  # Calculate monthly correlations
  monthly_corr <- corr_data %>%
    mutate(month = month(date)) %>%
    group_by(month) %>%
    summarise(
      corr_sm_precip = cor(soil_moisture, precipitation, use = "complete.obs"),
      corr_sm_temp = cor(soil_moisture, temperature, use = "complete.obs"),
      corr_swe_precip = cor(swe, precipitation, use = "complete.obs"),
      corr_swe_temp = cor(swe, temperature, use = "complete.obs")
    )
  
  # Create correlation plots
  p1 <- ggplot(monthly_corr, aes(x = month)) +
    geom_line(aes(y = corr_sm_precip, color = "Soil Moisture - Precipitation")) +
    geom_line(aes(y = corr_sm_temp, color = "Soil Moisture - Temperature")) +
    theme_minimal() +
    labs(title = "Monthly Correlations with Soil Moisture",
         x = "Month", y = "Correlation Coefficient") +
    scale_x_continuous(breaks = 1:12, labels = month.abb) +
    scale_color_manual(values = c("Soil Moisture - Precipitation" = "#377eb8",
                                 "Soil Moisture - Temperature" = "#e41a1c")) +
    theme(legend.position = "bottom")
  
  p2 <- ggplot(monthly_corr, aes(x = month)) +
    geom_line(aes(y = corr_swe_precip, color = "SWE - Precipitation")) +
    geom_line(aes(y = corr_swe_temp, color = "SWE - Temperature")) +
    theme_minimal() +
    labs(title = "Monthly Correlations with SWE",
         x = "Month", y = "Correlation Coefficient") +
    scale_x_continuous(breaks = 1:12, labels = month.abb) +
    scale_color_manual(values = c("SWE - Precipitation" = "#4daf4a",
                                 "SWE - Temperature" = "#984ea3")) +
    theme(legend.position = "bottom")
  
  nc_close(nc)
  
  # Save plots
  ggsave(file.path(output_dir, "correlation_soil_moisture.png"), p1, width = 12, height = 6, dpi = 300)
  ggsave(file.path(output_dir, "correlation_swe.png"), p2, width = 12, height = 6, dpi = 300)
}

# Function to process temperature analysis
process_temperature_analysis <- function(vic_file, output_dir) {
  nc <- nc_open(vic_file)
  
  # Get temperature variables
  time <- ncvar_get(nc, "time")
  air_temp <- ncvar_get(nc, "OUT_AIR_TEMP")
  surface_temp <- ncvar_get(nc, "OUT_SURF_TEMP")
  soil_temp <- ncvar_get(nc, "OUT_SOIL_TEMP")
  snow_surf_temp <- ncvar_get(nc, "OUT_SNOW_SURF_TEMP")
  snow_pack_temp <- ncvar_get(nc, "OUT_SNOW_PACK_TEMP")
  
  # Calculate spatial means
  air_temp_mean <- apply(air_temp, 3, mean, na.rm = TRUE)
  surface_temp_mean <- apply(surface_temp, 3, mean, na.rm = TRUE)
  soil_temp_mean <- apply(soil_temp, 4, mean, na.rm = TRUE)  # Note: soil_temp has an extra dimension
  snow_surf_temp_mean <- apply(snow_surf_temp, 3, mean, na.rm = TRUE)
  snow_pack_temp_mean <- apply(snow_pack_temp, 3, mean, na.rm = TRUE)
  
  dates <- as.Date(time, origin = "0001-01-01")
  
  # Create temperature data frame
  temp_data <- data.frame(
    date = dates,
    air_temperature = air_temp_mean,
    surface_temperature = surface_temp_mean,
    soil_temperature = soil_temp_mean,
    snow_surface_temperature = snow_surf_temp_mean,
    snow_pack_temperature = snow_pack_temp_mean
  )
  
  # Create monthly temperature plots
  monthly_temp <- temp_data %>%
    mutate(month = month(date)) %>%
    group_by(month) %>%
    summarise(
      avg_air_temp = mean(air_temperature, na.rm = TRUE),
      avg_surface_temp = mean(surface_temperature, na.rm = TRUE),
      avg_soil_temp = mean(soil_temperature, na.rm = TRUE),
      avg_snow_surf_temp = mean(snow_surface_temperature, na.rm = TRUE),
      avg_snow_pack_temp = mean(snow_pack_temperature, na.rm = TRUE)
    )
  
  p1 <- ggplot(monthly_temp, aes(x = month)) +
    geom_line(aes(y = avg_air_temp, color = "Air Temperature")) +
    geom_line(aes(y = avg_surface_temp, color = "Surface Temperature")) +
    geom_line(aes(y = avg_soil_temp, color = "Soil Temperature")) +
    geom_line(aes(y = avg_snow_surf_temp, color = "Snow Surface Temperature")) +
    geom_line(aes(y = avg_snow_pack_temp, color = "Snow Pack Temperature")) +
    theme_minimal() +
    labs(title = "Monthly Average Temperatures",
         x = "Month", y = "Temperature (°C)") +
    scale_x_continuous(breaks = 1:12, labels = month.abb) +
    scale_color_manual(values = c("Air Temperature" = "#377eb8",
                                 "Surface Temperature" = "#4daf4a",
                                 "Soil Temperature" = "#e41a1c",
                                 "Snow Surface Temperature" = "#984ea3",
                                 "Snow Pack Temperature" = "#ff7f00")) +
    theme(legend.position = "bottom")
  
  # Create annual temperature trends
  annual_temp <- temp_data %>%
    mutate(year = year(date)) %>%
    group_by(year) %>%
    summarise(
      avg_air_temp = mean(air_temperature, na.rm = TRUE),
      avg_surface_temp = mean(surface_temperature, na.rm = TRUE),
      avg_soil_temp = mean(soil_temperature, na.rm = TRUE),
      avg_snow_surf_temp = mean(snow_surface_temperature, na.rm = TRUE),
      avg_snow_pack_temp = mean(snow_pack_temperature, na.rm = TRUE)
    )
  
  p2 <- ggplot(annual_temp, aes(x = year)) +
    geom_line(aes(y = avg_air_temp, color = "Air Temperature")) +
    geom_line(aes(y = avg_surface_temp, color = "Surface Temperature")) +
    geom_line(aes(y = avg_soil_temp, color = "Soil Temperature")) +
    geom_line(aes(y = avg_snow_surf_temp, color = "Snow Surface Temperature")) +
    geom_line(aes(y = avg_snow_pack_temp, color = "Snow Pack Temperature")) +
    theme_minimal() +
    labs(title = "Annual Average Temperatures",
         x = "Year", y = "Temperature (°C)") +
    scale_color_manual(values = c("Air Temperature" = "#377eb8",
                                 "Surface Temperature" = "#4daf4a",
                                 "Soil Temperature" = "#e41a1c",
                                 "Snow Surface Temperature" = "#984ea3",
                                 "Snow Pack Temperature" = "#ff7f00")) +
    theme(legend.position = "bottom")
  
  nc_close(nc)
  
  # Save plots
  ggsave(file.path(output_dir, "temperature_monthly.png"), p1, width = 12, height = 6, dpi = 300)
  ggsave(file.path(output_dir, "temperature_annual.png"), p2, width = 12, height = 6, dpi = 300)
}

# Function to process snow analysis
process_snow_analysis <- function(vic_file, output_dir) {
  nc <- nc_open(vic_file)
  
  # Get snow variables
  time <- ncvar_get(nc, "time")
  swe <- ncvar_get(nc, "OUT_SWE")
  snow_melt <- ncvar_get(nc, "OUT_SNOW_MELT")
  
  # Calculate spatial means
  swe_mean <- apply(swe, 3, mean, na.rm = TRUE)
  snow_melt_mean <- apply(snow_melt, 3, mean, na.rm = TRUE)
  
  dates <- as.Date(time, origin = "0001-01-01")
  
  # Create snow data frame
  snow_data <- data.frame(
    date = dates,
    swe = swe_mean,
    snow_melt = snow_melt_mean
  )
  
  # Create monthly snow plots
  monthly_snow <- snow_data %>%
    mutate(month = month(date)) %>%
    group_by(month) %>%
    summarise(
      avg_swe = mean(swe, na.rm = TRUE),
      avg_melt = mean(snow_melt, na.rm = TRUE)
    )
  
  p1 <- ggplot(monthly_snow, aes(x = month)) +
    geom_line(aes(y = avg_swe, color = "SWE")) +
    geom_line(aes(y = avg_melt, color = "Snow Melt")) +
    theme_minimal() +
    labs(title = "Monthly Snow Statistics",
         x = "Month", y = "Value (mm)") +
    scale_x_continuous(breaks = 1:12, labels = month.abb) +
    scale_color_manual(values = c("SWE" = "#377eb8",
                                 "Snow Melt" = "#e41a1c")) +
    theme(legend.position = "bottom")
  
  # Create annual snow trends
  annual_snow <- snow_data %>%
    mutate(year = year(date)) %>%
    group_by(year) %>%
    summarise(
      max_swe = max(swe, na.rm = TRUE),
      total_melt = sum(snow_melt, na.rm = TRUE)
    )
  
  p2 <- ggplot(annual_snow, aes(x = year)) +
    geom_line(aes(y = max_swe, color = "Maximum SWE")) +
    geom_line(aes(y = total_melt, color = "Total Snow Melt")) +
    theme_minimal() +
    labs(title = "Annual Snow Statistics",
         x = "Year", y = "Value (mm)") +
    scale_color_manual(values = c("Maximum SWE" = "#377eb8",
                                 "Total Snow Melt" = "#e41a1c")) +
    theme(legend.position = "bottom")
  
  nc_close(nc)
  
  # Save plots
  ggsave(file.path(output_dir, "snow_monthly.png"), p1, width = 12, height = 6, dpi = 300)
  ggsave(file.path(output_dir, "snow_annual.png"), p2, width = 12, height = 6, dpi = 300)
}

# Main function to generate all analysis
generate_all_analysis <- function() {
  # Create output directory
  output_dir <- "images/analysis_results"
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Process water balance
  process_water_balance("data/VICOut2.nc", output_dir)
  
  # Process trend analysis
  process_trend_analysis("data/VICOut2.nc", output_dir)
  
  # Process correlation analysis
  process_correlation_analysis("data/VICOut2.nc", output_dir)
  
  # Process temperature analysis
  process_temperature_analysis("data/VICOut2.nc", output_dir)
  
  # Process snow analysis
  process_snow_analysis("data/VICOut2.nc", output_dir)
}

# Execute the analysis
generate_all_analysis() 