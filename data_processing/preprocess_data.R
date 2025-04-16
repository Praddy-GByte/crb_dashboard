# Data preprocessing and caching functions

# Function to preprocess and cache VIC data
preprocess_vic_data <- function(year_range = 1982:2024) {
  cache_dir <- "data/VIC_cache"
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }
  
  # Process data year by year
  for (year in year_range) {
    cache_file <- file.path(cache_dir, paste0("vic_", year, ".rds"))
    
    # Skip if already processed
    if (file.exists(cache_file)) {
      next
    }
    
    tryCatch({
      # Load raw data for the year
      raw_data <- load_vic_raw_data(year)
      
      # Process the data
      processed_data <- process_vic_data(raw_data)
      
      # Save to cache
      saveRDS(processed_data, cache_file)
      
      # Clean up
      rm(raw_data, processed_data)
      gc()
    }, error = function(e) {
      warning(paste("Error processing VIC data for year", year, ":", e$message))
    })
  }
}

# Function to preprocess and cache SMAP data
preprocess_smap_data <- function() {
  cache_dir <- "data/SMAP_cache"
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }
  
  cache_file <- file.path(cache_dir, "smap_processed.rds")
  
  # Skip if already processed
  if (file.exists(cache_file)) {
    return(TRUE)
  }
  
  tryCatch({
    # Load and process SMAP data
    raw_data <- load_smap_raw_data()
    processed_data <- process_smap_data(raw_data)
    
    # Save to cache
    saveRDS(processed_data, cache_file)
    
    # Clean up
    rm(raw_data, processed_data)
    gc()
    
    return(TRUE)
  }, error = function(e) {
    warning(paste("Error processing SMAP data:", e$message))
    return(FALSE)
  })
}

# Function to preprocess and cache GRACE data
preprocess_grace_data <- function() {
  cache_dir <- "data/GRACE_cache"
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }
  
  cache_file <- file.path(cache_dir, "grace_processed.rds")
  
  # Skip if already processed
  if (file.exists(cache_file)) {
    return(TRUE)
  }
  
  tryCatch({
    # Load and process GRACE data
    raw_data <- load_grace_raw_data()
    processed_data <- process_grace_data(raw_data)
    
    # Save to cache
    saveRDS(processed_data, cache_file)
    
    # Clean up
    rm(raw_data, processed_data)
    gc()
    
    return(TRUE)
  }, error = function(e) {
    warning(paste("Error processing GRACE data:", e$message))
    return(FALSE)
  })
}

# Function to check if preprocessing is needed
check_preprocessing_status <- function() {
  status <- list(
    vic = file.exists("data/VIC_cache/vic_2024.rds"),
    smap = file.exists("data/SMAP_cache/smap_processed.rds"),
    grace = file.exists("data/GRACE_cache/grace_processed.rds")
  )
  return(status)
}

# Function to get available years for VIC data
get_available_vic_years <- function() {
  cache_dir <- "data/VIC_cache"
  if (!dir.exists(cache_dir)) {
    return(numeric(0))
  }
  
  files <- list.files(cache_dir, pattern = "vic_\\d{4}\\.rds$")
  years <- as.numeric(gsub("vic_(\\d{4})\\.rds", "\\1", files))
  return(sort(years))
} 