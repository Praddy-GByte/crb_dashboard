# Configuration file for the dashboard

# Create a new environment for configuration
config_env <- new.env()

# Data paths
config_env$config <- list(
  # Directory paths
  data_dir = "data",
  processed_dir = "data/processed",
  cache_dir = "data/cache",
  images_dir = "images",
  
  # Cache directories
  vic_cache_dir = "data/VIC_cache",
  smap_cache_dir = "data/SMAP_cache",
  grace_cache_dir = "data/GRACE_cache",
  
  # Data file paths
  vic_data_path = "data/VIC_outputs",
  smap_data_path = "data/SMAP",
  grace_data_path = "data/GRACE",
  
  # Processing settings
  vic_years = 1982:2024,
  default_year = 2024,
  
  # Memory settings
  max_request_size = 1000*1024^2,  # 1GB
  
  # UI settings
  plot_height = "600px",
  map_height = "500px",
  
  # Color settings
  colors = list(
    asu_maroon = "#8C1D40",
    asu_gold = "#FFC627",
    asu_light_gold = "#FFD700",
    asu_dark_maroon = "#5C0025"
  ),
  
  # Map settings
  map_center = c(39.0, -105.0),
  map_zoom = 6,
  
  # Data processing settings
  chunk_size = 1000,  # Number of records to process at once
  max_workers = 4     # Maximum number of parallel workers
)

# Function to get configuration value
get_config <- function(key) {
  if (key %in% names(config_env$config)) {
    return(config_env$config[[key]])
  } else {
    warning(paste("Configuration key", key, "not found"))
    return(NULL)
  }
}

# Function to set configuration value
set_config <- function(key, value) {
  config_env$config[[key]] <- value
}

# Function to load configuration from file
load_config <- function(config_file = "config.yaml") {
  if (file.exists(config_file)) {
    config_env$config <- yaml::read_yaml(config_file)
  }
}

# Function to save configuration to file
save_config <- function(config_file = "config.yaml") {
  yaml::write_yaml(config_env$config, config_file)
} 