# Load required packages
library(ncdf4)
library(raster)
library(dplyr)
library(tidyr)

# Function to create optimized VIC data
optimize_vic_data <- function(input_file, output_dir) {
  # Create output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Open the input file
  nc <- nc_open(input_file)
  
  # Get dimensions
  nlat <- nc$dim$lat$len
  nlon <- nc$dim$lon$len
  ntime <- nc$dim$time$len
  nlayer <- nc$dim$nlayer$len
  
  # Create dimensions for new file
  londim <- ncdim_def("lon", "degrees_east", nc$dim$lon$vals)
  latdim <- ncdim_def("lat", "degrees_north", nc$dim$lat$vals)
  timedim <- ncdim_def("time", "days since 0001-01-01", nc$dim$time$vals)
  layerdim <- ncdim_def("nlayer", "", 1:nlayer)
  
  # Variables to process with their dimensions
  vars_3d <- c("OUT_PREC", "OUT_RAINF", "OUT_EVAP", "OUT_RUNOFF", 
               "OUT_BASEFLOW", "OUT_SWE")
  
  vars_4d <- c("OUT_SOIL_MOIST")
  
  # Create output file name
  output_file <- file.path(output_dir, paste0("CRB_PRISM_Calibrated.", 
                                             format(as.Date(ncvar_get(nc, "time")[1], origin = "0001-01-01"), "%Y-%m-%d"),
                                             "_optimized.nc"))
  
  # Define variables for new file
  var_defs <- list()
  
  # Add 3D variables
  for (var in vars_3d) {
    var_defs[[var]] <- ncvar_def(var, "float", list(londim, latdim, timedim),
                                missval = 9.96921e+36)
  }
  
  # Add 4D variables
  for (var in vars_4d) {
    var_defs[[var]] <- ncvar_def(var, "float", list(londim, latdim, layerdim, timedim),
                                missval = 9.96921e+36)
  }
  
  # Create new netCDF file
  ncout <- nc_create(output_file, var_defs)
  
  # Process 3D variables
  for (var in vars_3d) {
    cat("Processing", var, "\n")
    # Process data in chunks
    chunk_size <- 30
    n_chunks <- ceiling(ntime / chunk_size)
    
    for (i in 1:n_chunks) {
      start <- (i-1) * chunk_size + 1
      end <- min(i * chunk_size, ntime)
      
      # Read chunk
      chunk <- ncvar_get(nc, var, 
                        start = c(1, 1, start), 
                        count = c(nlon, nlat, end - start + 1))
      
      # Write chunk to output file
      ncvar_put(ncout, var, chunk,
                start = c(1, 1, start),
                count = c(nlon, nlat, end - start + 1))
      
      # Clear memory
      rm(chunk)
      gc()
    }
  }
  
  # Process 4D variables
  for (var in vars_4d) {
    cat("Processing", var, "\n")
    # Process data in chunks
    chunk_size <- 30
    n_chunks <- ceiling(ntime / chunk_size)
    
    for (i in 1:n_chunks) {
      start <- (i-1) * chunk_size + 1
      end <- min(i * chunk_size, ntime)
      
      # Read chunk
      chunk <- ncvar_get(nc, var,
                        start = c(1, 1, 1, start),
                        count = c(nlon, nlat, nlayer, end - start + 1))
      
      # Write chunk to output file
      ncvar_put(ncout, var, chunk,
                start = c(1, 1, 1, start),
                count = c(nlon, nlat, nlayer, end - start + 1))
      
      # Clear memory
      rm(chunk)
      gc()
    }
  }
  
  # Close files
  nc_close(ncout)
  nc_close(nc)
}

# Function to create monthly aggregated data
create_monthly_aggregates <- function(input_file, output_dir) {
  nc <- nc_open(input_file)
  
  # Get time dimension
  time <- ncvar_get(nc, "time")
  dates <- as.Date(time, origin = "0001-01-01")
  months <- format(dates, "%Y-%m")
  
  # Process each variable
  vars <- c("OUT_PREC", "OUT_RAINF", "OUT_EVAP", "OUT_RUNOFF", 
            "OUT_BASEFLOW", "OUT_SOIL_MOIST", "OUT_SWE")
  
  for (var in vars) {
    # Create monthly means
    monthly_means <- array(NA, dim = c(nc$dim$lon$len, nc$dim$lat$len, length(unique(months))))
    
    for (i in seq_along(unique(months))) {
      month_idx <- which(months == unique(months)[i])
      data <- ncvar_get(nc, var, start = c(1, 1, min(month_idx)),
                       count = c(nc$dim$lon$len, nc$dim$lat$len, length(month_idx)))
      monthly_means[,,i] <- apply(data, c(1, 2), mean, na.rm = TRUE)
    }
    
    # Save monthly data
    output_file <- file.path(output_dir, paste0(var, "_monthly.nc"))
    ncout <- nc_create(output_file, list(
      ncvar_def(var, "float", list(
        nc$dim$lon,
        nc$dim$lat,
        ncvar_def("time", "integer", list(ncdim_def("time", "", 1:length(unique(months)))))
      ))
    ))
    ncvar_put(ncout, var, monthly_means)
    nc_close(ncout)
  }
  
  nc_close(nc)
}

# Function to create spatial aggregates
create_spatial_aggregates <- function(input_file, output_dir) {
  nc <- nc_open(input_file)
  
  # Process each variable
  vars <- c("OUT_PREC", "OUT_RAINF", "OUT_EVAP", "OUT_RUNOFF", 
            "OUT_BASEFLOW", "OUT_SOIL_MOIST", "OUT_SWE")
  
  for (var in vars) {
    # Create spatial means
    spatial_means <- array(NA, dim = c(nc$dim$time$len))
    
    # Process in chunks
    chunk_size <- 30
    n_chunks <- ceiling(nc$dim$time$len / chunk_size)
    
    for (i in 1:n_chunks) {
      start <- (i-1) * chunk_size + 1
      end <- min(i * chunk_size, nc$dim$time$len)
      
      data <- ncvar_get(nc, var, start = c(1, 1, start),
                       count = c(nc$dim$lon$len, nc$dim$lat$len, end - start + 1))
      spatial_means[start:end] <- apply(data, 3, mean, na.rm = TRUE)
    }
    
    # Save spatial means
    output_file <- file.path(output_dir, paste0(var, "_spatial_means.nc"))
    ncout <- nc_create(output_file, list(
      ncvar_def(var, "float", list(
        ncvar_def("time", "integer", list(ncdim_def("time", "", 1:nc$dim$time$len)))
      ))
    ))
    ncvar_put(ncout, var, spatial_means)
    nc_close(ncout)
  }
  
  nc_close(nc)
}

# Main execution
input_dir <- "data/VIC_outputs"
output_dir <- "data/VIC_outputs_optimized"

# Process all VIC output files
vic_files <- list.files(input_dir, pattern = "CRB_PRISM_Calibrated.*\\.nc$", full.names = TRUE)

for (file in vic_files) {
  cat("Processing", basename(file), "\n")
  optimize_vic_data(file, output_dir)
} 