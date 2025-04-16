# Load required packages
library(ncdf4)
library(raster)
library(ggplot2)
library(dplyr)
library(lubridate)
library(plotly)
library(tidyr)
library(sp)
library(maps)
library(mapdata)

# Define colorful theme
theme_colorful <- function() {
  theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold", color = "#8C1D40"),
      axis.title = element_text(size = 12, color = "#8C1D40"),
      axis.text = element_text(size = 10, color = "#8C1D40"),
      panel.grid.major = element_line(color = "gray90"),
      panel.grid.minor = element_blank(),
      legend.title = element_text(size = 12, color = "#8C1D40"),
      legend.text = element_text(size = 10, color = "#8C1D40"),
      legend.position = "bottom",
      plot.background = element_rect(fill = "white"),
      panel.background = element_rect(fill = "white")
    )
}

# Define ASU colors
asu_colors <- c(
  "#8C1D40",  # ASU Maroon
  "#FFC627",  # ASU Gold
  "#00A3E0",  # ASU Blue
  "#78BE20",  # ASU Green
  "#FF7F32",  # ASU Orange
  "#747474"   # ASU Gray
)

# Function to add year info annotation
add_year_info <- function(plot, years) {
  year_range <- paste(min(years), "-", max(years))
  plot + 
    annotate("text", 
             x = Inf, y = -Inf, 
             label = paste("Data Period:", year_range),
             hjust = 1, vjust = -1,
             color = "#333333",
             size = 3)
}

# Read VIC output
nc_file <- nc_open("data/VICOut2.nc")

# Print file structure
print("NetCDF File Structure:")
print(nc_file)

# Get dimensions
lon <- ncvar_get(nc_file, "lon")
lat <- ncvar_get(nc_file, "lat")
time <- ncvar_get(nc_file, "time")

print("Dimensions:")
print(paste("Longitude:", length(lon)))
print(paste("Latitude:", length(lat)))
print(paste("Time:", length(time)))

# Convert time to dates (assuming time is in days since 1982-01-01)
dates <- as.Date(time, origin = "1982-01-01")
years <- year(dates)

# Extract all variables
variables <- list(
  # Precipitation
  prec = ncvar_get(nc_file, "OUT_PREC"),
  rain = ncvar_get(nc_file, "OUT_RAINF"),
  snow = ncvar_get(nc_file, "OUT_SNOWF"),
  
  # Evapotranspiration
  evap = ncvar_get(nc_file, "OUT_EVAP"),
  evap_bare = ncvar_get(nc_file, "OUT_EVAP_BARE"),
  evap_canop = ncvar_get(nc_file, "OUT_EVAP_CANOP"),
  transp_veg = ncvar_get(nc_file, "OUT_TRANSP_VEG"),
  
  # Runoff
  runoff = ncvar_get(nc_file, "OUT_RUNOFF"),
  baseflow = ncvar_get(nc_file, "OUT_BASEFLOW"),
  
  # Soil Moisture
  soil_moist = ncvar_get(nc_file, "OUT_SOIL_MOIST"),
  
  # Snow
  swe = ncvar_get(nc_file, "OUT_SWE"),
  snow_melt = ncvar_get(nc_file, "OUT_SNOW_MELT"),
  
  # Temperature
  air_temp = ncvar_get(nc_file, "OUT_AIR_TEMP"),
  surf_temp = ncvar_get(nc_file, "OUT_SURF_TEMP"),
  soil_temp = ncvar_get(nc_file, "OUT_SOIL_TEMP"),
  snow_temp = ncvar_get(nc_file, "OUT_SNOW_PACK_TEMP")
)

# Print variable dimensions
print("Variable Dimensions:")
for (var_name in names(variables)) {
  print(paste(var_name, ":", paste(dim(variables[[var_name]]), collapse = " x ")))
}

# Close the NetCDF file
nc_close(nc_file)

# Function to create spatial plot with map borders
create_spatial_plot <- function(data, title, color_scale = "viridis") {
  # Calculate spatial mean
  spatial_mean <- apply(data, c(1, 2), mean, na.rm = TRUE)
  
  # Create raster
  r <- raster(spatial_mean)
  extent(r) <- c(min(lon), max(lon), min(lat), max(lat))
  
  # Convert to data frame for ggplot
  df <- as.data.frame(r, xy = TRUE)
  colnames(df) <- c("x", "y", "value")
  
  # Get map data
  map_data <- map_data("state")
  
  # Create plot with map borders
  p <- ggplot() +
    geom_raster(data = df, aes(x = x, y = y, fill = value)) +
    geom_polygon(data = map_data, aes(x = long, y = lat, group = group),
                 fill = NA, color = "black", size = 0.2) +
    scale_fill_viridis_c(option = "inferno") +
    coord_fixed(ratio = 1, xlim = c(min(lon), max(lon)), ylim = c(min(lat), max(lat))) +
    labs(title = title, x = "Longitude", y = "Latitude", fill = "Value") +
    theme_colorful() +
    theme(panel.border = element_rect(color = "black", fill = NA, size = 1))
  
  return(add_year_info(p, years))
}

# Function to create time series plot with improved design
create_time_series <- function(data, title, var_name, folder) {
  # Calculate daily mean across spatial dimensions
  if (length(dim(data)) == 4) {
    daily_mean <- apply(data, 4, function(x) mean(x, na.rm = TRUE))
  } else {
    daily_mean <- apply(data, 3, function(x) mean(x, na.rm = TRUE))
  }
  
  # Create data frame with years
  df <- data.frame(Year = years, Value = daily_mean)
  
  # Create plot with ASU colors and improved design
  p <- ggplot(df, aes(x = Year, y = Value)) +
    geom_area(fill = asu_colors[1], alpha = 0.3) +
    geom_line(color = asu_colors[1], size = 1) +
    geom_point(color = asu_colors[2], size = 2) +
    labs(title = title,
         x = "",
         y = paste(var_name, "(mm)")) +
    theme_colorful() +
    scale_x_continuous(breaks = seq(min(years), max(years), by = 2))
  
  ggsave(paste0("images/", folder, "/time_series.png"), p, width = 12, height = 6)
}

# Function to create monthly statistics plot with colorful bars
create_monthly_stats <- function(data, title, var_name, folder) {
  # Calculate daily mean across spatial dimensions
  if (length(dim(data)) == 4) {
    daily_mean <- apply(data, 4, function(x) mean(x, na.rm = TRUE))
  } else {
    daily_mean <- apply(data, 3, function(x) mean(x, na.rm = TRUE))
  }
  
  # Create data frame with years and months
  df <- data.frame(Year = years, Month = month(dates), Value = daily_mean)
  
  # Calculate monthly statistics
  monthly_stats <- df %>%
    group_by(Month) %>%
    summarise(
      Mean = mean(Value, na.rm = TRUE),
      SD = sd(Value, na.rm = TRUE),
      Min = min(Value, na.rm = TRUE),
      Max = max(Value, na.rm = TRUE)
    )
  
  # Create plot with colorful bars
  p <- ggplot(monthly_stats, aes(x = Month)) +
    geom_bar(aes(y = Mean), stat = "identity", fill = asu_colors[1], alpha = 0.7) +
    geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), 
                  width = 0.2, color = asu_colors[2], size = 1) +
    geom_point(aes(y = Min), color = asu_colors[3], size = 3) +
    geom_point(aes(y = Max), color = asu_colors[4], size = 3) +
    labs(title = title,
         x = "Month",
         y = paste(var_name, "(mm)")) +
    scale_x_continuous(breaks = 1:12, labels = month.abb) +
    theme_colorful()
  
  ggsave(paste0("images/", folder, "/monthly_stats.png"), p, width = 12, height = 6)
}

# 1. Precipitation Analysis
create_spatial_plot(variables$prec, "Spatial Distribution of Precipitation")
create_time_series(variables$prec, "Precipitation Time Series", "Precipitation", "precipitation_analysis")
create_monthly_stats(variables$prec, "Monthly Precipitation Statistics", "Precipitation", "precipitation_analysis")

# Rain vs Snow plot
rain_mean <- apply(variables$rain, 3, mean, na.rm = TRUE)
snow_mean <- apply(variables$snow, 3, mean, na.rm = TRUE)
df_rain_snow <- data.frame(Year = years, Rainfall = rain_mean, Snowfall = snow_mean)

p_rain_snow <- ggplot(df_rain_snow, aes(x = Year)) +
  geom_area(aes(y = Rainfall, fill = "Rainfall"), alpha = 0.3) +
  geom_area(aes(y = Snowfall, fill = "Snowfall"), alpha = 0.3) +
  geom_line(aes(y = Rainfall, color = "Rainfall"), size = 1) +
  geom_line(aes(y = Snowfall, color = "Snowfall"), size = 1) +
  scale_color_manual(values = c("Rainfall" = asu_colors[1], 
                               "Snowfall" = asu_colors[2])) +
  scale_fill_manual(values = c("Rainfall" = asu_colors[1], 
                              "Snowfall" = asu_colors[2])) +
  labs(title = "Rainfall vs Snowfall",
       x = "",
       y = "Amount (mm)",
       color = "Type",
       fill = "Type") +
  theme_colorful() +
  scale_x_continuous(breaks = seq(min(years), max(years), by = 2))

p_rain_snow <- add_year_info(p_rain_snow, years)
ggsave("images/precipitation_analysis/rain_vs_snow.png", p_rain_snow, width = 12, height = 6)

# 2. Evapotranspiration Analysis
create_spatial_plot(variables$evap, "Spatial Distribution of Evapotranspiration")
create_time_series(variables$evap, "Evapotranspiration Time Series", "ET", "evapotranspiration_analysis")
create_monthly_stats(variables$evap, "Monthly Evapotranspiration Statistics", "ET", "evapotranspiration_analysis")

# ET Components plot
et_components <- data.frame(
  Year = years,
  Bare = apply(variables$evap_bare, 3, mean, na.rm = TRUE),
  Canopy = apply(variables$evap_canop, 3, mean, na.rm = TRUE),
  Transpiration = apply(variables$transp_veg, 3, mean, na.rm = TRUE)
)

p_et_components <- ggplot(et_components, aes(x = Year)) +
  geom_line(aes(y = Bare, color = "Bare Soil"), size = 1) +
  geom_line(aes(y = Canopy, color = "Canopy"), size = 1) +
  geom_line(aes(y = Transpiration, color = "Transpiration"), size = 1) +
  scale_color_manual(values = c("Bare Soil" = asu_colors[1], 
                               "Canopy" = asu_colors[2], 
                               "Transpiration" = asu_colors[3])) +
  labs(title = "Evapotranspiration Components",
       x = "Year",
       y = "ET (mm)",
       color = "Component") +
  theme_colorful() +
  scale_x_continuous(breaks = seq(min(years), max(years), by = 2))

p_et_components <- add_year_info(p_et_components, years)
ggsave("images/evapotranspiration_analysis/components.png", p_et_components, width = 12, height = 6)

# 3. Runoff Analysis
create_spatial_plot(variables$runoff, "Spatial Distribution of Runoff")
create_time_series(variables$runoff, "Runoff Time Series", "Runoff", "runoff_analysis")
create_monthly_stats(variables$runoff, "Monthly Runoff Statistics", "Runoff", "runoff_analysis")

# Surface vs Baseflow plot
runoff_mean <- apply(variables$runoff, 3, mean, na.rm = TRUE)
baseflow_mean <- apply(variables$baseflow, 3, mean, na.rm = TRUE)
df_runoff <- data.frame(Year = years, Surface = runoff_mean, Baseflow = baseflow_mean)

p_runoff <- ggplot(df_runoff, aes(x = Year)) +
  geom_line(aes(y = Surface, color = "Surface Runoff"), size = 1) +
  geom_line(aes(y = Baseflow, color = "Baseflow"), size = 1) +
  scale_color_manual(values = c("Surface Runoff" = asu_colors[1], 
                               "Baseflow" = asu_colors[2])) +
  labs(title = "Surface Runoff vs Baseflow",
       x = "Year",
       y = "Flow (mm)",
       color = "Type") +
  theme_colorful() +
  scale_x_continuous(breaks = seq(min(years), max(years), by = 2))

p_runoff <- add_year_info(p_runoff, years)
ggsave("images/runoff_analysis/surface_vs_baseflow.png", p_runoff, width = 12, height = 6)

# 4. Soil Moisture Analysis
create_spatial_plot(variables$soil_moist[,,1,], "Spatial Distribution of Soil Moisture (Layer 1)")
create_time_series(variables$soil_moist[,,1,], "Surface Soil Moisture Time Series", "Surface Soil Moisture", "soil_moisture_analysis")
create_monthly_stats(variables$soil_moist[,,1,], "Monthly Soil Moisture Statistics (Layer 1)", "Soil Moisture", "soil_moisture_analysis")

# Root Zone Soil Moisture (Layer 2)
create_time_series(variables$soil_moist[,,2,], "Root Zone Soil Moisture Time Series", "Root Zone Soil Moisture", "soil_moisture_analysis")

# Soil Layers plot
soil_layers <- data.frame(
  Year = years,
  Layer1 = apply(variables$soil_moist[,,1,], 3, mean, na.rm = TRUE),
  Layer2 = apply(variables$soil_moist[,,2,], 3, mean, na.rm = TRUE),
  Layer3 = apply(variables$soil_moist[,,3,], 3, mean, na.rm = TRUE)
)

p_soil_layers <- ggplot(soil_layers, aes(x = Year)) +
  geom_line(aes(y = Layer1, color = "Layer 1"), size = 1) +
  geom_line(aes(y = Layer2, color = "Layer 2"), size = 1) +
  geom_line(aes(y = Layer3, color = "Layer 3"), size = 1) +
  scale_color_manual(values = c("Layer 1" = asu_colors[1], 
                               "Layer 2" = asu_colors[2], 
                               "Layer 3" = asu_colors[3])) +
  labs(title = "Soil Moisture by Layer",
       x = "Year",
       y = "Soil Moisture (mm)",
       color = "Layer") +
  theme_colorful() +
  scale_x_continuous(breaks = seq(min(years), max(years), by = 2))

p_soil_layers <- add_year_info(p_soil_layers, years)
ggsave("images/soil_moisture_analysis/soil_layers.png", p_soil_layers, width = 12, height = 6)

# 5. Snow Water Equivalent Analysis
create_spatial_plot(variables$swe, "Spatial Distribution of Snow Water Equivalent")
create_time_series(variables$swe, "Snow Water Equivalent Time Series", "SWE", "snow_water_analysis")
create_monthly_stats(variables$swe, "Monthly Snow Water Equivalent Statistics", "SWE", "snow_water_analysis")

# Snow Melt plot
swe_mean <- apply(variables$swe, 3, mean, na.rm = TRUE)
melt_mean <- apply(variables$snow_melt, 3, mean, na.rm = TRUE)
df_snow <- data.frame(Year = years, SWE = swe_mean, Melt = melt_mean)

p_snow_melt <- ggplot(df_snow, aes(x = Year)) +
  geom_line(aes(y = SWE, color = "SWE"), size = 1) +
  geom_line(aes(y = Melt, color = "Melt"), size = 1) +
  scale_color_manual(values = c("SWE" = asu_colors[1], 
                               "Melt" = asu_colors[2])) +
  labs(title = "Snow Water Equivalent and Melt",
       x = "Year",
       y = "Amount (mm)",
       color = "Variable") +
  theme_colorful() +
  scale_x_continuous(breaks = seq(min(years), max(years), by = 2))

p_snow_melt <- add_year_info(p_snow_melt, years)
ggsave("images/snow_water_analysis/snow_melt.png", p_snow_melt, width = 12, height = 6)

# 6. Temperature Analysis
# Air Temperature
create_spatial_plot(variables$air_temp, "Spatial Distribution of Air Temperature")
create_time_series(variables$air_temp, "Air Temperature Time Series", "Temperature (°C)", "temperature_analysis")
create_monthly_stats(variables$air_temp, "Monthly Air Temperature Statistics", "Temperature (°C)", "temperature_analysis")

# Surface Temperature
create_spatial_plot(variables$surf_temp, "Spatial Distribution of Surface Temperature")
create_time_series(variables$surf_temp, "Surface Temperature Time Series", "Temperature (°C)", "temperature_analysis")
create_monthly_stats(variables$surf_temp, "Monthly Surface Temperature Statistics", "Temperature (°C)", "temperature_analysis")

# Soil Temperature
create_spatial_plot(variables$soil_temp, "Spatial Distribution of Soil Temperature")
create_time_series(variables$soil_temp, "Soil Temperature Time Series", "Temperature (°C)", "temperature_analysis")
create_monthly_stats(variables$soil_temp, "Monthly Soil Temperature Statistics", "Temperature (°C)", "temperature_analysis")

# Snow Temperature
create_spatial_plot(variables$snow_temp, "Spatial Distribution of Snow Temperature")
create_time_series(variables$snow_temp, "Snow Temperature Time Series", "Temperature (°C)", "temperature_analysis")
create_monthly_stats(variables$snow_temp, "Monthly Snow Temperature Statistics", "Temperature (°C)", "temperature_analysis")

# 7. Combined Analysis
# Water Balance
water_balance <- data.frame(
  Year = years,
  Precipitation = apply(variables$prec, 3, mean, na.rm = TRUE),
  Evapotranspiration = apply(variables$evap, 3, mean, na.rm = TRUE),
  Runoff = apply(variables$runoff, 3, mean, na.rm = TRUE)
)

p_water_balance <- ggplot(water_balance, aes(x = Year)) +
  geom_line(aes(y = Precipitation, color = "Precipitation"), size = 1) +
  geom_line(aes(y = Evapotranspiration, color = "Evapotranspiration"), size = 1) +
  geom_line(aes(y = Runoff, color = "Runoff"), size = 1) +
  scale_color_manual(values = c("Precipitation" = asu_colors[1], 
                               "Evapotranspiration" = asu_colors[2], 
                               "Runoff" = asu_colors[3])) +
  labs(title = "Water Balance Components",
       x = "Year",
       y = "Amount (mm)",
       color = "Component") +
  theme_colorful() +
  scale_x_continuous(breaks = seq(min(years), max(years), by = 2))

p_water_balance <- add_year_info(p_water_balance, years)
ggsave("images/combined_analysis/water_balance.png", p_water_balance, width = 12, height = 6)

# Trend Analysis
annual_means <- water_balance %>%
  group_by(Year) %>%
  summarise(
    Precipitation = mean(Precipitation),
    Evapotranspiration = mean(Evapotranspiration),
    Runoff = mean(Runoff)
  )

p_trends <- ggplot(annual_means, aes(x = Year)) +
  geom_line(aes(y = Precipitation, color = "Precipitation"), size = 1) +
  geom_line(aes(y = Evapotranspiration, color = "Evapotranspiration"), size = 1) +
  geom_line(aes(y = Runoff, color = "Runoff"), size = 1) +
  scale_color_manual(values = c("Precipitation" = asu_colors[1], 
                               "Evapotranspiration" = asu_colors[2], 
                               "Runoff" = asu_colors[3])) +
  labs(title = "Annual Trends",
       x = "Year",
       y = "Annual Mean (mm)",
       color = "Component") +
  theme_colorful() +
  scale_x_continuous(breaks = seq(min(years), max(years), by = 2))

p_trends <- add_year_info(p_trends, years)
ggsave("images/combined_analysis/trends.png", p_trends, width = 12, height = 6)

# Correlation Analysis
correlation_data <- water_balance %>%
  select(-Year) %>%
  cor()

correlation_df <- data.frame(
  Variable1 = rep(colnames(correlation_data), each = ncol(correlation_data)),
  Variable2 = rep(colnames(correlation_data), times = nrow(correlation_data)),
  Correlation = as.vector(correlation_data)
)

p_correlation <- ggplot(correlation_df, aes(x = Variable1, y = Variable2, fill = Correlation)) +
  geom_tile() +
  scale_fill_gradient2(low = asu_colors[1], 
                      mid = "white", 
                      high = asu_colors[2], 
                      midpoint = 0) +
  labs(title = "Correlation Matrix",
       x = "Variable",
       y = "Variable",
       fill = "Correlation") +
  theme_colorful() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p_correlation <- add_year_info(p_correlation, years)
ggsave("images/combined_analysis/correlations.png", p_correlation, width = 10, height = 8)

# Extreme Events
extremes <- water_balance %>%
  group_by(Year) %>%
  summarise(
    max_prec = max(Precipitation),
    max_et = max(Evapotranspiration),
    max_runoff = max(Runoff)
  )

p_extremes <- ggplot(extremes, aes(x = Year)) +
  geom_line(aes(y = max_prec, color = "Max Precipitation"), size = 1) +
  geom_line(aes(y = max_et, color = "Max ET"), size = 1) +
  geom_line(aes(y = max_runoff, color = "Max Runoff"), size = 1) +
  scale_color_manual(values = c("Max Precipitation" = asu_colors[1], 
                               "Max ET" = asu_colors[2], 
                               "Max Runoff" = asu_colors[3])) +
  labs(title = "Extreme Events",
       x = "Year",
       y = "Maximum Value (mm)",
       color = "Event") +
  theme_colorful() +
  scale_x_continuous(breaks = seq(min(years), max(years), by = 2))

p_extremes <- add_year_info(p_extremes, years)
ggsave("images/combined_analysis/extreme_events.png", p_extremes, width = 12, height = 6) 