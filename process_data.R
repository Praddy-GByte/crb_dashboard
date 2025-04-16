# Function to load processed data
load_processed_data <- function(data_type) {
  processed_path <- file.path("data", paste0(data_type, "_processed"))
  if (!dir.exists(processed_path)) {
    warning(paste("Processed data directory not found for", data_type))
    return(NULL)
  }
  
  # Load data based on type
  if (data_type == "vic") {
    # Load VIC data
    vic_file <- file.path(processed_path, "vic_processed.RData")
    if (file.exists(vic_file)) {
      load(vic_file)
      return(list(data = vic_data, dates = vic_dates))
    }
  } else if (data_type == "smap") {
    # Load SMAP data
    smap_file <- file.path(processed_path, "smap_processed.RData")
    if (file.exists(smap_file)) {
      load(smap_file)
      return(list(
        surface_means = smap_surface_means,
        rootzone_means = smap_rootzone_means,
        dates = smap_dates
      ))
    }
  } else if (data_type == "grace") {
    # Load GRACE data
    grace_file <- file.path(processed_path, "grace_processed.RData")
    if (file.exists(grace_file)) {
      load(grace_file)
      return(list(
        tws = grace_tws,
        uncertainty = grace_uncertainty,
        dates = grace_dates
      ))
    }
  } else if (data_type == "snotel") {
    # Load SNOTEL data
    snotel_file <- file.path(processed_path, "snotel_processed.RData")
    if (file.exists(snotel_file)) {
      load(snotel_file)
      return(snotel_data)
    }
  }
  
  warning(paste("Processed data file not found for", data_type))
  return(NULL)
}

# Function to check if data exists
check_processed_data <- function(data_type) {
  processed_path <- file.path("data", paste0(data_type, "_processed"))
  if (!dir.exists(processed_path)) {
    return(FALSE)
  }
  
  if (data_type == "vic") {
    return(file.exists(file.path(processed_path, "vic_processed.RData")))
  } else if (data_type == "smap") {
    return(file.exists(file.path(processed_path, "smap_processed.RData")))
  } else if (data_type == "grace") {
    return(file.exists(file.path(processed_path, "grace_processed.RData")))
  } else if (data_type == "snotel") {
    return(file.exists(file.path(processed_path, "snotel_processed.RData")))
  }
  
  return(FALSE)
} 