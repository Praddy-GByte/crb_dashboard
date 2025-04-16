# Load required libraries
library(ncdf4)
library(raster)
library(ggplot2)
library(dplyr)
library(tidyr)
library(viridis)

# Function to analyze PRISM data
analyze_prism_data <- function() {
  # Open the PRISM NetCDF file
  nc <- nc_open("data/CRB_PRISM_Calibrated.2024-01-01.nc")
  
  # Print file information
  cat("PRISM Data File Information:\n")
  cat("---------------------------\n")
  cat("Variables available:\n")
  print(names(nc$var))
  
  # Get dimensions
  lon <- ncvar_get(nc, "lon")
  lat <- ncvar_get(nc, "lat")
  time <- ncvar_get(nc, "time")
  
  cat("\nDimensions:\n")
  cat("Longitude range:", range(lon), "\n")
  cat("Latitude range:", range(lat), "\n")
  cat("Time steps:", length(time), "\n")
  
  # Get precipitation data
  precip <- ncvar_get(nc, "OUT_PREC")
  
  # Calculate spatial mean precipitation
  spatial_mean <- apply(precip, c(1,2), mean, na.rm=TRUE)
  
  # Convert to raster
  precip_raster <- raster(t(spatial_mean), 
                         xmn=min(lon), xmx=max(lon),
                         ymn=min(lat), ymx=max(lat))
  
  # Create output directory
  dir.create("images/prism_analysis", recursive = TRUE, showWarnings = FALSE)
  
  # Save spatial distribution plot
  png("images/prism_analysis/spatial_distribution.png", width=800, height=600)
  plot(precip_raster, main="Spatial Distribution of PRISM Precipitation",
       col=viridis(100), axes=FALSE, box=FALSE)
  dev.off()
  
  # Calculate time series (average over space)
  time_series <- apply(precip, 3, mean, na.rm=TRUE)
  dates <- as.Date(time, origin="0001-01-01")
  
  # Create time series data frame
  ts_data <- data.frame(
    date = dates,
    precipitation = time_series
  )
  
  # Save time series plot
  png("images/prism_analysis/time_series.png", width=800, height=400)
  plot(ts_data$date, ts_data$precipitation, type="l",
       xlab="Date", ylab="Precipitation (mm)",
       main="Time Series of PRISM Precipitation")
  dev.off()
  
  # Calculate monthly statistics
  monthly_stats <- ts_data %>%
    mutate(month = format(date, "%Y-%m")) %>%
    group_by(month) %>%
    summarise(
      mean_precip = mean(precipitation, na.rm=TRUE),
      max_precip = max(precipitation, na.rm=TRUE),
      min_precip = min(precipitation, na.rm=TRUE)
    )
  
  # Save monthly statistics
  write.csv(monthly_stats, "images/prism_analysis/monthly_statistics.csv", row.names=FALSE)
  
  # Close the NetCDF file
  nc_close(nc)
  
  return(list(
    spatial_mean = spatial_mean,
    time_series = ts_data,
    monthly_stats = monthly_stats
  ))
}

# Run the analysis
results <- analyze_prism_data() 