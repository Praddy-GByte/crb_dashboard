# Load required packages
library(sf)
library(dplyr)
library(tidyr)
library(ncdf4)
library(ggplot2)
library(plotly)
library(leaflet)
library(units)
library(zoo)
library(lubridate)
library(terra)
library(stars)
library(arrow)
library(jsonlite)

# Create directories if they don't exist
dirs <- c("data/VIC_outputs", "data/VIC_json", "data/SMAP_outputs", 
          "data/GRACE_outputs", "data/static_images", "cache")

for (dir in dirs) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
    cat("Created directory:", dir, "\n")
  } else {
    cat("Directory already exists:", dir, "\n")
  }
}

# Helper function to save RDS files
save_rds <- function(data, filename) {
  saveRDS(data, file.path("cache", filename))
}

# Function to load RDS objects
load_rds <- function(filename) {
  if (file.exists(filename)) {
    readRDS(filename)
  } else {
    NULL
  }
}

# Function to check if a point is within the basin
filter_to_basin <- function(points_sf, basin_boundary) {
  st_intersection(points_sf, basin_boundary)
}

# Process and cache basin data
process_basin_data <- function() {
  message("Processing basin data...")
  
  # Read basin shapefile
  basin <- st_read("data/CRB_poly/basin_CRB_poly.shp", quiet = TRUE)
  
  # Read HUC10 watersheds
  huc10 <- st_read("data/huc10s/huc10.shp", quiet = TRUE)
  
  # Save processed data
  saveRDS(basin, "cache/basin.rds")
  saveRDS(huc10, "cache/huc10.rds")
  
  return(list(basin = basin, huc10 = huc10))
}

# Process and cache SNOTEL data
process_snotel_data <- function() {
  message("Processing SNOTEL data...")
  
  # Read SNOTEL data
  snotel_files <- list.files("data/snotel", pattern = ".csv$", full.names = TRUE)
  
  snotel_data <- lapply(snotel_files, function(file) {
    read.csv(file, stringsAsFactors = FALSE)
  })
  
  # Save processed data
  saveRDS(snotel_data, "cache/snotel.rds")
  
  return(snotel_data)
}

# Function to process VIC NetCDF data
process_vic_data <- function() {
  message("Processing VIC data...")
  
  # Read VIC NetCDF file
  vic_file <- "data/VICOut2.nc"
  if (!file.exists(vic_file)) {
    stop("VIC NetCDF file not found")
  }
  
  nc <- nc_open(vic_file)
  
  # Get dimensions
  time <- ncvar_get(nc, "time")
  lat <- ncvar_get(nc, "lat")
  lon <- ncvar_get(nc, "lon")
  
  # Process each variable
  variables <- names(nc$var)
  vic_data <- list()
  
  for (var in variables) {
    message(sprintf("Processing variable: %s", var))
    
    # Read data using terra for efficiency
    r <- rast(vic_file, var)
    
    # Calculate statistics
    vic_data[[var]] <- list(
      annual_mean = mean(r, na.rm = TRUE),
      monthly_means = tapp(r, rep(1:12, length.out = nlyr(r)), mean, na.rm = TRUE),
      seasonal_means = tapp(r, rep(1:4, each = 3, length.out = nlyr(r)), mean, na.rm = TRUE)
    )
    
    # Save as GeoTIFF
    writeRaster(vic_data[[var]]$annual_mean,
                filename = sprintf("data/VIC_outputs/%s_annual.tif", var),
                overwrite = TRUE)
  }
  
  nc_close(nc)
  
  # Save metadata
  metadata <- list(
    variables = variables,
    extent = list(
      lon = range(lon),
      lat = range(lat)
    ),
    time_range = range(as.Date(time, origin = "1900-01-01"))
  )
  
  write_json(metadata, "data/VIC_json/metadata.json", pretty = TRUE)
  
  # Cache the processed data
  saveRDS(vic_data, "cache/vic_processed.rds")
  
  return(vic_data)
}

# Function to process SMAP data
process_smap_data <- function() {
  cat("Processing SMAP data...\n")
  
  # Read SMAP NetCDF file
  smap_file <- "data/SPL4SMGP.007_9km_aid0001.nc"
  nc <- nc_open(smap_file)
  
  # Extract dimensions
  time <- ncvar_get(nc, "time")
  lat <- ncvar_get(nc, "lat")
  lon <- ncvar_get(nc, "lon")
  
  # Process each soil moisture layer
  layers <- c(
    "surface" = "Geophysical_Data_sm_surface",
    "rootzone" = "Geophysical_Data_sm_rootzone",
    "profile" = "Geophysical_Data_sm_profile"
  )
  
  for (layer_name in names(layers)) {
    var_name <- layers[layer_name]
    cat("Processing", layer_name, "soil moisture...\n")
    
    # Read the data
    sm_data <- ncvar_get(nc, var_name)
    
    # Convert dates
    dates <- as.Date(time, origin = "2000-01-01")
    years <- format(dates, "%Y")
    unique_years <- unique(years)
    
    # Create an empty array for the daily means
    daily_means <- array(NA, dim = c(dim(sm_data)[1], dim(sm_data)[2], dim(sm_data)[4]))
    
    # Calculate daily means across the third dimension (typically diurnal variations)
    for (i in 1:dim(sm_data)[4]) {  # For each time step
      daily_means[,,i] <- apply(sm_data[,,1:dim(sm_data)[3],i], c(1,2), mean, na.rm = TRUE)
    }
    
    # Process annual means
    for (year in unique_years) {
      year_idx <- which(years == year)
      if (length(year_idx) > 0) {
        annual_mean <- apply(daily_means[,,year_idx], c(1,2), mean, na.rm = TRUE)
        
        # Create terra SpatRaster
        r <- rast(t(annual_mean))
        ext(r) <- ext(c(min(lon), max(lon), min(lat), max(lat)))
        crs(r) <- "EPSG:4326"
        
        writeRaster(r, 
                   filename = file.path("data/SMAP_outputs", 
                                      paste0("smap_", layer_name, "_", year, ".tif")),
                   overwrite = TRUE)
      }
    }
    
    # Save the complete time series as parquet
    df <- data.frame(
      date = rep(dates, each = length(lon) * length(lat)),
      lat = rep(rep(lat, each = length(lon)), length(dates)),
      lon = rep(rep(lon, length(lat)), length(dates)),
      soil_moisture = as.vector(aperm(daily_means, c(2,1,3)))
    )
    
    write_parquet(df, 
                 sink = file.path("data/SMAP_outputs", 
                                paste0("smap_", layer_name, "_timeseries.parquet")))
  }
  
  nc_close(nc)
  cat("SMAP data processing complete\n")
  return(TRUE)
}

# Function to process GRACE data
process_grace_data <- function() {
  # Check if processed data already exists
  if (file.exists("data/processed/grace_processed.rds")) {
    return(readRDS("data/processed/grace_processed.rds"))
  }
  
  # Load GRACE data
  grace_file <- "data/GRCTellus.JPL.200204_202401.GLO.RL06.1M.MSCNv03CRI.nc"
  if (!file.exists(grace_file)) {
    stop("GRACE data file not found")
  }
  
  # Read GRACE data
  grace_data <- nc_open(grace_file)
  
  # Extract water storage data
  tws <- ncvar_get(grace_data, "lwe_thickness")
  time <- ncvar_get(grace_data, "time")
  lon <- ncvar_get(grace_data, "lon")
  lat <- ncvar_get(grace_data, "lat")
  
  # Close the NetCDF file
  nc_close(grace_data)
  
  # Calculate mean water storage for each time step
  tws_mean <- apply(tws, 3, mean, na.rm = TRUE)
  
  # Convert time to dates
  dates <- as.Date(time, origin = "2002-01-01")
  
  # Create data frame
  grace_df <- data.frame(
    time = dates,
    tws_mean = tws_mean
  )
  
  # Save processed data
  saveRDS(grace_df, "data/processed/grace_processed.rds")
  
  return(grace_df)
}

# Main processing function
process_all_data <- function(force = FALSE) {
  message("Starting data processing...")
  
  # Check if cached data exists and force is FALSE
  if (!force && all(file.exists(c("cache/vic_processed.rds",
                                 "cache/smap_processed.rds",
                                 "cache/grace_processed.rds",
                                 "cache/basin.rds",
                                 "cache/huc10.rds",
                                 "cache/snotel.rds")))) {
    message("Using cached data...")
    return(TRUE)
  }
  
  # Process all data sources
  tryCatch({
    process_vic_data()
    process_smap_data()
    process_grace_data()
    process_basin_data()
    process_snotel_data()
    message("Data processing completed successfully")
    return(TRUE)
  }, error = function(e) {
    message("Error processing data: ", e$message)
    return(FALSE)
  })
}

# Run data processing
process_all_data(force = TRUE)

# Create metadata file for VIC variables
vic_metadata <- list(
  variables = list(
    OUT_RAD_TEMP = list(
      name = "Radiation Temperature",
      description = "Surface radiation temperature",
      units = "K"
    ),
    OUT_SNOW_SURF_TEMP = list(
      name = "Snow Surface Temperature",
      description = "Temperature at snow surface",
      units = "K"
    ),
    OUT_SNOW_PACK_TEMP = list(
      name = "Snow Pack Temperature",
      description = "Temperature of the snow pack",
      units = "K"
    ),
    OUT_LATENT = list(
      name = "Latent Heat",
      description = "Latent heat flux from surface",
      units = "W/m^2"
    ),
    OUT_TRANSP_VEG = list(
      name = "Vegetation Transpiration",
      description = "Transpiration from vegetation",
      units = "mm"
    ),
    OUT_EVAP_BARE = list(
      name = "Bare Soil Evaporation",
      description = "Evaporation from bare soil",
      units = "mm"
    ),
    OUT_PREC = list(
      name = "Precipitation",
      description = "Total precipitation",
      units = "mm"
    ),
    OUT_RAINF = list(
      name = "Rainfall",
      description = "Rainfall amount",
      units = "mm"
    ),
    OUT_EVAP = list(
      name = "Evaporation",
      description = "Total evaporation",
      units = "mm"
    ),
    OUT_RUNOFF = list(
      name = "Runoff",
      description = "Surface runoff",
      units = "mm"
    ),
    OUT_BASEFLOW = list(
      name = "Baseflow",
      description = "Subsurface runoff",
      units = "mm"
    ),
    OUT_SWE = list(
      name = "Snow Water Equivalent",
      description = "Snow water equivalent",
      units = "mm"
    ),
    OUT_SOIL_MOIST = list(
      name = "Soil Moisture",
      description = "Soil moisture content",
      units = "mm",
      layers = 3
    )
  )
)

# Save metadata
write_json(vic_metadata, "data/VIC_json/metadata.json", pretty = TRUE) 