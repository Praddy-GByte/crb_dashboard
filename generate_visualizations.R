# Load required libraries
library(ncdf4)
library(raster)
library(sf)
library(ggplot2)
library(dplyr)
library(tidyr)
library(viridis)
library(gridExtra)
library(lubridate)

# Set working directory
setwd("/Users/praddy5/Desktop/Dashboard")

# Create necessary directories
for (dir in c("images", "data/processed")) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }
}

# Function to read and process VIC data
process_vic_data <- function() {
  # Read VIC output
  vic_file <- "data/VICOut2.nc"
  vic_nc <- nc_open(vic_file)
  
  # Print dimensions
  print("VIC dimensions:")
  print(vic_nc$dim)
  
  # Extract snow data
  snowf <- ncvar_get(vic_nc, "OUT_SNOWF")  # Dimensions: lon, lat, time
  time <- ncvar_get(vic_nc, "time")
  lat <- ncvar_get(vic_nc, "lat")
  lon <- ncvar_get(vic_nc, "lon")
  
  # Print array dimensions
  print("Snowfall array dimensions:")
  print(dim(snowf))
  
  # Create annual mean snowfall (average over time)
  snowf_annual <- apply(snowf, c(1,2), mean, na.rm=TRUE)  # Result: lon, lat
  
  # Convert to raster
  snowf_raster <- raster(t(snowf_annual), 
                      xmn=min(lon), xmx=max(lon),
                      ymn=min(lat), ymx=max(lat))
  
  # Save SWE analysis plot
  png("images/swe_analysis.png", width=800, height=600)
  plot(snowf_raster, main="Annual Mean Snowfall",
       col=viridis(100), axes=FALSE, box=FALSE)
  dev.off()
  
  # Process time series data (average over lat, lon for each time step)
  # Reshape array to handle dimensions correctly - permute to make time first
  snowf_permuted <- aperm(snowf, c(3,1,2))  # Now dimensions are: time, lon, lat
  snowf_reshaped <- array(snowf_permuted, dim=c(dim(snowf)[3], prod(dim(snowf)[1:2])))
  snowf_mean <- apply(snowf_reshaped, 1, mean, na.rm=TRUE)
  dates <- as.Date("0001-01-01") + time
  
  # Save processed data
  vic_processed <- data.frame(
    time = dates,
    snowf_mean = snowf_mean
  )
  saveRDS(vic_processed, "data/processed/vic_processed.rds")
  
  nc_close(vic_nc)
  
  return(vic_processed)
}

# Function to process SMAP data
process_smap_data <- function() {
  # Read SMAP data
  smap_file <- "data/SPL4SMGP.007_9km_aid0001.nc"
  smap_nc <- nc_open(smap_file)
  
  # Print dimensions
  print("SMAP dimensions:")
  print(smap_nc$dim)
  
  # Extract soil moisture data
  sm <- ncvar_get(smap_nc, "Geophysical_Data_sm_rootzone")  # Dimensions: day_period, lon, lat, time
  time <- ncvar_get(smap_nc, "time")
  lat <- ncvar_get(smap_nc, "lat")
  lon <- ncvar_get(smap_nc, "lon")
  
  # Print array dimensions
  print("Soil moisture array dimensions:")
  print(dim(sm))
  
  # First average over day_period
  sm_daily <- apply(sm, c(2,3,4), mean, na.rm=TRUE)  # Result: lon, lat, time
  
  # Create mean soil moisture (average over time)
  sm_mean_spatial <- apply(sm_daily, c(1,2), mean, na.rm=TRUE)  # Result: lon, lat
  
  # Convert to raster
  sm_raster <- raster(t(sm_mean_spatial),
                     xmn=min(lon), xmx=max(lon),
                     ymn=min(lat), ymx=max(lat))
  
  # Save soil moisture plot
  png("images/soil_moisture.png", width=800, height=600)
  plot(sm_raster, main="Mean Root Zone Soil Moisture",
       col=viridis(100), axes=FALSE, box=FALSE)
  dev.off()
  
  # Process time series data (average over lat, lon for each time step)
  # Reshape array to handle dimensions correctly
  sm_daily_reshaped <- array(sm_daily, dim=c(prod(dim(sm_daily)[1:2]), dim(sm_daily)[3]))
  sm_mean <- apply(sm_daily_reshaped, 2, mean, na.rm=TRUE)
  dates <- as.Date("2000-01-01") + time
  
  # Save processed data
  smap_processed <- data.frame(
    time = dates,
    sm_mean = sm_mean
  )
  saveRDS(smap_processed, "data/processed/smap_processed.rds")
  
  nc_close(smap_nc)
  
  return(smap_processed)
}

# Function to process GRACE data
process_grace_data <- function() {
  # Read GRACE data
  grace_file <- "data/GRCTellus.JPL.200204_202401.GLO.RL06.1M.MSCNv03CRI.nc"
  grace_nc <- nc_open(grace_file)
  
  # Print dimensions
  print("GRACE dimensions:")
  print(grace_nc$dim)
  
  # Extract water storage data (in cm)
  tws <- ncvar_get(grace_nc, "lwe_thickness")  # Dimensions: lon, lat, time
  time <- ncvar_get(grace_nc, "time")
  lat <- ncvar_get(grace_nc, "lat")
  lon <- ncvar_get(grace_nc, "lon")
  
  # Print array dimensions
  print("Water storage array dimensions:")
  print(dim(tws))
  
  # Create mean water storage (average over time)
  tws_mean_spatial <- apply(tws, c(1,2), mean, na.rm=TRUE)  # Result: lon, lat
  
  # Convert to raster
  tws_raster <- raster(t(tws_mean_spatial),
                      xmn=min(lon), xmx=max(lon),
                      ymn=min(lat), ymx=max(lat))
  
  # Save water storage plot
  png("images/water_storage.png", width=800, height=600)
  plot(tws_raster, main="Mean Terrestrial Water Storage",
       col=viridis(100), axes=FALSE, box=FALSE)
  dev.off()
  
  # Process time series data (average over lat, lon for each time step)
  # Reshape array to handle dimensions correctly - average over lon and lat for each time
  tws_mean <- apply(tws, 3, mean, na.rm=TRUE)  # Average over lon and lat for each time step
  dates <- as.Date("2002-01-01") + time
  
  # Save processed data
  grace_processed <- data.frame(
    time = dates,
    tws_mean = tws_mean
  )
  saveRDS(grace_processed, "data/processed/grace_processed.rds")
  
  nc_close(grace_nc)
  
  return(grace_processed)
}

# Function to create historical analysis plot
create_historical_analysis <- function(vic_data, smap_data, grace_data) {
  # Create time series plot
  png("images/historical_analysis.png", width=800, height=600)
  par(mfrow=c(2,2))
  
  # Plot VIC snowfall trend
  plot(vic_data$time, vic_data$snowf_mean,
       type="l", col="blue", main="Snowfall Trend",
       xlab="Year", ylab="Snowfall (mm)")
  
  # Plot SMAP soil moisture trend
  plot(smap_data$time, smap_data$sm_mean,
       type="l", col="brown", main="Soil Moisture Trend",
       xlab="Year", ylab="Soil Moisture (m³/m³)")
  
  # Plot GRACE water storage trend
  plot(grace_data$time, grace_data$tws_mean,
       type="l", col="darkgreen", main="Water Storage Trend",
       xlab="Year", ylab="Water Storage (cm)")
  
  # Plot combined anomalies
  plot(vic_data$time, scale(vic_data$snowf_mean),
       type="l", col="blue", main="Standardized Anomalies",
       xlab="Year", ylab="Standardized Value")
  lines(smap_data$time, scale(smap_data$sm_mean),
        col="brown")
  lines(grace_data$time, scale(grace_data$tws_mean),
        col="darkgreen")
  legend("topright", legend=c("Snowfall", "Soil Moisture", "Water Storage"),
         col=c("blue", "brown", "darkgreen"), lty=1)
  
  dev.off()
}

# Generate all visualizations
print("Processing VIC data...")
vic_data <- process_vic_data()

print("Processing SMAP data...")
smap_data <- process_smap_data()

print("Processing GRACE data...")
grace_data <- process_grace_data()

print("Creating historical analysis...")
create_historical_analysis(vic_data, smap_data, grace_data)

print("Visualizations generated successfully!") 