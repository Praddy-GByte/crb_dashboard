# Load required packages
library(ncdf4)
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)
library(plotly)
library(raster)
library(terra)
library(sf)
library(maps)
library(mapdata)

# Create output directory
dir.create("vic_analysis", showWarnings = FALSE)

# Read VIC output
nc <- nc_open("data/VICOut2.nc")

# Get dimensions
lon <- ncvar_get(nc, "lon")
lat <- ncvar_get(nc, "lat")
time <- ncvar_get(nc, "time")

# Convert time to dates
dates <- as.Date(time, origin = "0001-01-01")

# Extract key variables
variables <- list(
  "Precipitation" = ncvar_get(nc, "OUT_PREC"),
  "Snow Water Equivalent" = ncvar_get(nc, "OUT_SWE"),
  "Runoff" = ncvar_get(nc, "OUT_RUNOFF"),
  "Baseflow" = ncvar_get(nc, "OUT_BASEFLOW"),
  "Evapotranspiration" = ncvar_get(nc, "OUT_EVAP")
)

# Extract soil moisture for each layer (3 layers)
soil_moisture <- ncvar_get(nc, "OUT_SOIL_MOIST")
for (i in 1:3) {
  variables[[paste0("Soil Moisture Layer ", i)]] <- soil_moisture[,,i,]
}

# Close the NetCDF file
nc_close(nc)

# Function to create spatial mean time series
create_time_series <- function(data, dates, name) {
  # Calculate spatial mean
  if (length(dim(data)) == 3) {
    spatial_mean <- apply(data, 3, mean, na.rm = TRUE)
  } else {
    spatial_mean <- apply(data, 4, mean, na.rm = TRUE)
  }
  
  # Create data frame
  df <- data.frame(
    Date = dates,
    Value = spatial_mean,
    Variable = name
  )
  
  return(df)
}

# Create time series for all variables
time_series_data <- lapply(names(variables), function(name) {
  create_time_series(variables[[name]], dates, name)
}) %>% bind_rows()

# Create time series plot
p_time_series <- ggplot(time_series_data, aes(x = Date, y = Value, color = Variable)) +
  geom_line() +
  labs(title = "VIC Model Output Time Series",
       x = "Date",
       y = "Value (mm)") +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("vic_analysis/time_series.png", p_time_series, width = 12, height = 8)

# Calculate monthly statistics
monthly_stats <- time_series_data %>%
  mutate(Month = month(Date, label = TRUE)) %>%
  group_by(Variable, Month) %>%
  summarise(
    Mean = mean(Value, na.rm = TRUE),
    Min = min(Value, na.rm = TRUE),
    Max = max(Value, na.rm = TRUE)
  )

# Create monthly boxplot
p_monthly <- ggplot(monthly_stats, aes(x = Month, y = Mean, color = Variable)) +
  geom_line() +
  geom_point() +
  labs(title = "Monthly Average VIC Outputs",
       x = "Month",
       y = "Value (mm)") +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("vic_analysis/monthly_stats.png", p_monthly, width = 12, height = 8)

# Create spatial maps for the last time step
create_spatial_map <- function(data, name) {
  # Get the last time step
  if (length(dim(data)) == 3) {
    last_step <- data[,,dim(data)[3]]
  } else {
    last_step <- data[,,1,dim(data)[4]]  # Use first soil layer
  }
  
  # Create raster
  r <- raster(t(last_step),
              xmn = min(lon), xmx = max(lon),
              ymn = min(lat), ymx = max(lat))
  
  # Convert to data frame for plotting
  df <- as.data.frame(r, xy = TRUE)
  colnames(df) <- c("lon", "lat", "value")
  
  # Get map data
  states <- map_data("state")
  
  # Create plot
  p <- ggplot() +
    geom_raster(data = df, aes(x = lon, y = lat, fill = value)) +
    geom_polygon(data = states, aes(x = long, y = lat, group = group),
                 fill = NA, color = "black", size = 0.1) +
    scale_fill_viridis_c(name = name) +
    coord_fixed(1.3) +
    labs(title = paste("Spatial Distribution of", name),
         x = "Longitude", y = "Latitude") +
    theme_minimal()
  
  ggsave(paste0("vic_analysis/", tolower(gsub(" ", "_", name)), "_map.png"), p, width = 10, height = 8)
}

# Create spatial maps for key variables
lapply(names(variables), function(name) {
  create_spatial_map(variables[[name]], name)
})

# Print summary statistics
cat("\nSummary Statistics:\n")
for (name in names(variables)) {
  data <- variables[[name]]
  if (length(dim(data)) == 3) {
    cat("\n", name, ":\n")
    cat("  Mean:", mean(data, na.rm = TRUE), "\n")
    cat("  Min:", min(data, na.rm = TRUE), "\n")
    cat("  Max:", max(data, na.rm = TRUE), "\n")
    cat("  SD:", sd(data, na.rm = TRUE), "\n")
  } else {
    for (i in 1:dim(data)[3]) {
      cat("\n", name, "Layer", i, ":\n")
      layer_data <- data[,,i,]
      cat("  Mean:", mean(layer_data, na.rm = TRUE), "\n")
      cat("  Min:", min(layer_data, na.rm = TRUE), "\n")
      cat("  Max:", max(layer_data, na.rm = TRUE), "\n")
      cat("  SD:", sd(layer_data, na.rm = TRUE), "\n")
    }
  }
} 