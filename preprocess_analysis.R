# Load required libraries
library(ncdf4)
library(raster)
library(ggplot2)
library(dplyr)
library(tidyr)
library(plotly)
library(sf)
library(lubridate)
library(jsonlite)
library(zoo)  # For rolling statistics

# Create output directories
dirs <- c("data/analysis_results", "data/analysis_cache")
for (dir in dirs) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }
}

# Function to process and cache VIC data analysis
process_vic_analysis <- function() {
  message("Processing VIC data analysis...")
  
  # Read VIC output
  nc <- nc_open("data/VICOut2.nc")
  
  # Get dimensions
  lon <- ncvar_get(nc, "lon")
  lat <- ncvar_get(nc, "lat")
  time <- ncvar_get(nc, "time")
  dates <- as.Date(time, origin = "0001-01-01")
  
  # Extract variables
  variables <- list(
    prec = ncvar_get(nc, "OUT_PREC"),
    swe = ncvar_get(nc, "OUT_SWE"),
    runoff = ncvar_get(nc, "OUT_RUNOFF"),
    baseflow = ncvar_get(nc, "OUT_BASEFLOW"),
    evap = ncvar_get(nc, "OUT_EVAP"),
    soil_moist = ncvar_get(nc, "OUT_SOIL_MOIST")
  )
  
  nc_close(nc)
  
  # Process and store analysis results
  analysis_results <- list()
  
  # 1. Precipitation Analysis
  message("Processing precipitation analysis...")
  analysis_results$precipitation <- list(
    # Statistics
    statistics = list(
      mean = mean(variables$prec, na.rm = TRUE),
      max = max(variables$prec, na.rm = TRUE),
      min = min(variables$prec, na.rm = TRUE),
      std = sd(variables$prec, na.rm = TRUE),
      quantiles = quantile(variables$prec, probs = c(0.1, 0.25, 0.5, 0.75, 0.9), na.rm = TRUE)
    ),
    
    # Spatial Distribution
    spatial = list(
      mean = apply(variables$prec, c(1,2), mean, na.rm = TRUE),
      max = apply(variables$prec, c(1,2), max, na.rm = TRUE),
      min = apply(variables$prec, c(1,2), min, na.rm = TRUE),
      std = apply(variables$prec, c(1,2), sd, na.rm = TRUE)
    ),
    
    # Time Series
    timeseries = list(
      daily = apply(variables$prec, 3, mean, na.rm = TRUE),
      rolling_mean = rollmean(apply(variables$prec, 3, mean, na.rm = TRUE), k = 30, fill = NA),
      anomalies = apply(variables$prec, 3, mean, na.rm = TRUE) - mean(apply(variables$prec, 3, mean, na.rm = TRUE), na.rm = TRUE)
    ),
    
    # Monthly Statistics
    monthly = list(
      mean = tapply(apply(variables$prec, 3, mean, na.rm = TRUE), 
                   format(dates, "%Y-%m"), mean, na.rm = TRUE),
      max = tapply(apply(variables$prec, 3, mean, na.rm = TRUE),
                  format(dates, "%Y-%m"), max, na.rm = TRUE),
      min = tapply(apply(variables$prec, 3, mean, na.rm = TRUE),
                  format(dates, "%Y-%m"), min, na.rm = TRUE),
      std = tapply(apply(variables$prec, 3, mean, na.rm = TRUE),
                  format(dates, "%Y-%m"), sd, na.rm = TRUE)
    ),
    
    # Annual Trends
    annual = list(
      mean = tapply(apply(variables$prec, 3, mean, na.rm = TRUE),
                   format(dates, "%Y"), mean, na.rm = TRUE),
      trend = lm(apply(variables$prec, 3, mean, na.rm = TRUE) ~ seq_along(dates))$coefficients[2]
    ),
    
    # Seasonal Patterns
    seasonal = list(
      mean = tapply(apply(variables$prec, 3, mean, na.rm = TRUE),
                   quarter(dates), mean, na.rm = TRUE),
      std = tapply(apply(variables$prec, 3, mean, na.rm = TRUE),
                  quarter(dates), sd, na.rm = TRUE)
    ),
    
    # Extreme Events Analysis
    extremes = list(
      threshold = quantile(apply(variables$prec, 3, mean, na.rm = TRUE), 0.95, na.rm = TRUE),
      events = sum(apply(variables$prec, 3, mean, na.rm = TRUE) > 
                   quantile(apply(variables$prec, 3, mean, na.rm = TRUE), 0.95, na.rm = TRUE), 
                 na.rm = TRUE),
      duration = rle(apply(variables$prec, 3, mean, na.rm = TRUE) > 
                     quantile(apply(variables$prec, 3, mean, na.rm = TRUE), 0.95, na.rm = TRUE))$lengths
    ),
    
    # Spatial Correlation
    spatial_correlation = cor(t(apply(variables$prec, c(1,2), mean, na.rm = TRUE)), 
                            use = "pairwise.complete.obs")
  )
  
  # 2. Snow Water Equivalent Analysis
  message("Processing snow water equivalent analysis...")
  analysis_results$swe <- list(
    # Statistics
    statistics = list(
      mean = mean(variables$swe, na.rm = TRUE),
      max = max(variables$swe, na.rm = TRUE),
      min = min(variables$swe, na.rm = TRUE),
      std = sd(variables$swe, na.rm = TRUE),
      quantiles = quantile(variables$swe, probs = c(0.1, 0.25, 0.5, 0.75, 0.9), na.rm = TRUE)
    ),
    
    # Spatial Distribution
    spatial = list(
      mean = apply(variables$swe, c(1,2), mean, na.rm = TRUE),
      max = apply(variables$swe, c(1,2), max, na.rm = TRUE),
      min = apply(variables$swe, c(1,2), min, na.rm = TRUE),
      std = apply(variables$swe, c(1,2), sd, na.rm = TRUE)
    ),
    
    # Time Series
    timeseries = list(
      daily = apply(variables$swe, 3, mean, na.rm = TRUE),
      rolling_mean = rollmean(apply(variables$swe, 3, mean, na.rm = TRUE), k = 30, fill = NA),
      anomalies = apply(variables$swe, 3, mean, na.rm = TRUE) - mean(apply(variables$swe, 3, mean, na.rm = TRUE), na.rm = TRUE)
    ),
    
    # Monthly Statistics
    monthly = list(
      mean = tapply(apply(variables$swe, 3, mean, na.rm = TRUE),
                   format(dates, "%Y-%m"), mean, na.rm = TRUE),
      max = tapply(apply(variables$swe, 3, mean, na.rm = TRUE),
                  format(dates, "%Y-%m"), max, na.rm = TRUE),
      min = tapply(apply(variables$swe, 3, mean, na.rm = TRUE),
                  format(dates, "%Y-%m"), min, na.rm = TRUE),
      std = tapply(apply(variables$swe, 3, mean, na.rm = TRUE),
                  format(dates, "%Y-%m"), sd, na.rm = TRUE)
    ),
    
    # Annual Trends
    annual = list(
      mean = tapply(apply(variables$swe, 3, mean, na.rm = TRUE),
                   format(dates, "%Y"), mean, na.rm = TRUE),
      trend = lm(apply(variables$swe, 3, mean, na.rm = TRUE) ~ seq_along(dates))$coefficients[2]
    ),
    
    # Seasonal Patterns
    seasonal = list(
      mean = tapply(apply(variables$swe, 3, mean, na.rm = TRUE),
                   quarter(dates), mean, na.rm = TRUE),
      std = tapply(apply(variables$swe, 3, mean, na.rm = TRUE),
                  quarter(dates), sd, na.rm = TRUE)
    ),
    
    # Elevation Analysis
    elevation = list(
      correlation = cor(apply(variables$swe, c(1,2), mean, na.rm = TRUE),
                       apply(variables$swe, c(1,2), max, na.rm = TRUE),
                       use = "pairwise.complete.obs")
    ),
    
    # Temperature Impact
    temperature_impact = list(
      correlation = cor(apply(variables$swe, 3, mean, na.rm = TRUE),
                       apply(variables$evap, 3, mean, na.rm = TRUE),
                       use = "pairwise.complete.obs")
    )
  )
  
  # 3. Water Balance Analysis
  message("Processing water balance analysis...")
  analysis_results$water_balance <- list(
    components = list(
      precipitation = apply(variables$prec, 3, mean, na.rm = TRUE),
      evapotranspiration = apply(variables$evap, 3, mean, na.rm = TRUE),
      runoff = apply(variables$runoff, 3, mean, na.rm = TRUE),
      baseflow = apply(variables$baseflow, 3, mean, na.rm = TRUE)
    ),
    monthly = list(
      prec = tapply(apply(variables$prec, 3, mean, na.rm = TRUE),
                   format(dates, "%Y-%m"), mean, na.rm = TRUE),
      evap = tapply(apply(variables$evap, 3, mean, na.rm = TRUE),
                   format(dates, "%Y-%m"), mean, na.rm = TRUE),
      runoff = tapply(apply(variables$runoff, 3, mean, na.rm = TRUE),
                     format(dates, "%Y-%m"), mean, na.rm = TRUE),
      baseflow = tapply(apply(variables$baseflow, 3, mean, na.rm = TRUE),
                       format(dates, "%Y-%m"), mean, na.rm = TRUE)
    )
  )
  
  # 4. Soil Moisture Analysis
  message("Processing soil moisture analysis...")
  analysis_results$soil_moisture <- list(
    spatial = apply(variables$soil_moist, c(1,2,3), mean, na.rm = TRUE),
    timeseries = apply(variables$soil_moist, 4, mean, na.rm = TRUE),
    monthly = tapply(apply(variables$soil_moist, 4, mean, na.rm = TRUE),
                    format(dates, "%Y-%m"), mean, na.rm = TRUE)
  )
  
  # Save analysis results
  saveRDS(analysis_results, "data/analysis_cache/analysis_results.rds")
  
  # Create and save visualizations
  message("Creating and saving visualizations...")
  create_visualizations(analysis_results)
  
  return(analysis_results)
}

# Function to create and save visualizations
create_visualizations <- function(analysis_results) {
  # Create visualization directory
  vis_dir <- "data/analysis_results/visualizations"
  if (!dir.exists(vis_dir)) {
    dir.create(vis_dir, recursive = TRUE)
  }
  
  # 1. Precipitation Visualizations
  message("Creating precipitation visualizations...")
  
  # Spatial Distribution
  p_prec_spatial <- ggplot(data.frame(
    lon = rep(seq_along(dim(analysis_results$precipitation$spatial$mean)[1]), 
              each = dim(analysis_results$precipitation$spatial$mean)[2]),
    lat = rep(seq_along(dim(analysis_results$precipitation$spatial$mean)[2]), 
              times = dim(analysis_results$precipitation$spatial$mean)[1]),
    value = as.vector(analysis_results$precipitation$spatial$mean)
  ), aes(x = lon, y = lat, fill = value)) +
    geom_raster() +
    scale_fill_viridis_c() +
    labs(title = "Spatial Distribution of Precipitation")
  
  ggsave(file.path(vis_dir, "precipitation_spatial.png"), p_prec_spatial)
  
  # Time Series
  p_prec_timeseries <- ggplot(data.frame(
    date = seq_along(analysis_results$precipitation$timeseries$daily),
    value = analysis_results$precipitation$timeseries$daily
  ), aes(x = date, y = value)) +
    geom_line() +
    labs(title = "Precipitation Time Series")
  
  ggsave(file.path(vis_dir, "precipitation_timeseries.png"), p_prec_timeseries)
  
  # 2. SWE Visualizations
  message("Creating SWE visualizations...")
  
  # Spatial Distribution
  p_swe_spatial <- ggplot(data.frame(
    lon = rep(seq_along(dim(analysis_results$swe$spatial$mean)[1]), 
              each = dim(analysis_results$swe$spatial$mean)[2]),
    lat = rep(seq_along(dim(analysis_results$swe$spatial$mean)[2]), 
              times = dim(analysis_results$swe$spatial$mean)[1]),
    value = as.vector(analysis_results$swe$spatial$mean)
  ), aes(x = lon, y = lat, fill = value)) +
    geom_raster() +
    scale_fill_viridis_c() +
    labs(title = "Spatial Distribution of SWE")
  
  ggsave(file.path(vis_dir, "swe_spatial.png"), p_swe_spatial)
  
  # Time Series
  p_swe_timeseries <- ggplot(data.frame(
    date = seq_along(analysis_results$swe$timeseries$daily),
    value = analysis_results$swe$timeseries$daily
  ), aes(x = date, y = value)) +
    geom_line() +
    labs(title = "SWE Time Series")
  
  ggsave(file.path(vis_dir, "swe_timeseries.png"), p_swe_timeseries)
}

# Main function to run all preprocessing
main <- function() {
  message("Starting analysis preprocessing...")
  results <- process_vic_analysis()
  message("Analysis preprocessing completed successfully!")
  return(results)
}

# Run the preprocessing
main() 