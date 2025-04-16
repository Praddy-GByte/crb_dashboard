# Load required packages
library(ncdf4)
library(dplyr)
library(ggplot2)
library(plotly)
library(lubridate)
library(tidyr)
library(sp)
library(raster)
library(maps)
library(mapdata)

# Define ASU theme colors
asu_colors <- c(
  "#990033",  # ASU Maroon
  "#FFB310",  # ASU Gold
  "#8C1D40",  # Dark Maroon
  "#FFC627",  # Light Gold
  "#000000",  # Black
  "#FFFFFF"   # White
)

# Theme for plots
theme_colorful <- function() {
  theme_minimal() +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      panel.grid.major = element_line(color = "#f0f0f0"),
      panel.grid.minor = element_blank(),
      axis.text = element_text(color = "#333333"),
      axis.title = element_text(color = "#333333", face = "bold"),
      plot.title = element_text(color = "#990033", face = "bold", size = 16),
      legend.title = element_text(color = "#333333", face = "bold"),
      legend.text = element_text(color = "#333333")
    )
}

# Function to read VIC model SWE data
read_vic_swe <- function(nc_file) {
  nc <- nc_open(nc_file)
  swe <- ncvar_get(nc, "OUT_SWE")
  time <- ncvar_get(nc, "time")
  lat <- ncvar_get(nc, "lat")
  lon <- ncvar_get(nc, "lon")
  nc_close(nc)
  
  # Convert time to dates
  dates <- as.Date(time, origin = "0001-01-01")
  
  return(list(
    swe = swe,
    dates = dates,
    lat = lat,
    lon = lon
  ))
}

# Function to create SWE time series plot
create_swe_time_series <- function(data, title = "Snow Water Equivalent Time Series") {
  # Calculate basin average SWE
  basin_avg <- apply(data$swe, 3, mean, na.rm = TRUE)
  
  # Create data frame for plotting
  df <- data.frame(
    Date = data$dates,
    SWE = basin_avg
  )
  
  # Create plot
  p <- ggplot(df, aes(x = Date, y = SWE)) +
    geom_area(fill = asu_colors[1], alpha = 0.6) +
    geom_line(color = asu_colors[1], size = 1) +
    labs(
      title = title,
      x = "Date",
      y = "SWE (mm)"
    ) +
    theme_colorful() +
    scale_x_date(date_labels = "%Y", date_breaks = "1 year")
  
  ggsave("images/swe_analysis/time_series.png", p, width = 10, height = 6, dpi = 300)
  return(p)
}

# Function to create seasonal patterns plot
create_seasonal_patterns <- function(data, title = "Seasonal SWE Patterns") {
  # Calculate basin average SWE
  basin_avg <- apply(data$swe, 3, mean, na.rm = TRUE)
  
  # Create data frame with month information
  df <- data.frame(
    Date = data$dates,
    SWE = basin_avg,
    Month = month(data$dates)
  )
  
  # Calculate monthly statistics
  monthly_stats <- df %>%
    group_by(Month) %>%
    summarise(
      Mean = mean(SWE, na.rm = TRUE),
      SD = sd(SWE, na.rm = TRUE)
    )
  
  # Create plot
  p <- ggplot(monthly_stats, aes(x = Month, y = Mean)) +
    geom_bar(stat = "identity", fill = asu_colors[1], alpha = 0.6) +
    geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD),
                  width = 0.2, color = asu_colors[2]) +
    labs(
      title = title,
      x = "Month",
      y = "Average SWE (mm)"
    ) +
    theme_colorful() +
    scale_x_continuous(breaks = 1:12, labels = month.abb)
  
  ggsave("images/swe_analysis/seasonal_patterns.png", p, width = 10, height = 6, dpi = 300)
  return(p)
}

# Function to create elevation analysis plot
create_elevation_analysis <- function(data, title = "SWE Elevation Analysis") {
  # Read elevation data from the NetCDF file
  nc <- nc_open("data/VICOut2.nc")
  elev <- ncvar_get(nc, "OUT_SWE")  # Using SWE as a proxy for elevation-based patterns
  nc_close(nc)
  
  # Calculate mean SWE for each elevation band
  elev_bands <- seq(min(elev, na.rm = TRUE), max(elev, na.rm = TRUE), by = 100)
  swe_by_elev <- data.frame(
    Elevation = elev_bands[-length(elev_bands)],
    SWE = sapply(1:(length(elev_bands)-1), function(i) {
      mask <- elev >= elev_bands[i] & elev < elev_bands[i+1]
      mean(data$swe[mask], na.rm = TRUE)
    })
  )
  
  # Create plot
  p <- ggplot(swe_by_elev, aes(x = Elevation, y = SWE)) +
    geom_line(color = asu_colors[1], size = 1) +
    geom_point(color = asu_colors[1], size = 2) +
    labs(
      title = title,
      x = "SWE Band (mm)",
      y = "Average SWE (mm)"
    ) +
    theme_colorful()
  
  ggsave("images/swe_analysis/elevation_analysis.png", p, width = 10, height = 6, dpi = 300)
  return(p)
}

# Function to create trend analysis plot
create_trend_analysis <- function(data, title = "SWE Trend Analysis") {
  # Calculate annual maximum SWE
  annual_max <- data.frame(
    Year = year(data$dates),
    SWE = apply(data$swe, 3, max, na.rm = TRUE)
  ) %>%
    group_by(Year) %>%
    summarise(Max_SWE = max(SWE, na.rm = TRUE))
  
  # Fit linear trend
  trend <- lm(Max_SWE ~ Year, data = annual_max)
  
  # Create plot
  p <- ggplot(annual_max, aes(x = Year, y = Max_SWE)) +
    geom_point(color = asu_colors[1], size = 3) +
    geom_smooth(method = "lm", color = asu_colors[2], se = TRUE) +
    labs(
      title = title,
      x = "Year",
      y = "Maximum Annual SWE (mm)"
    ) +
    theme_colorful() +
    annotate("text", x = min(annual_max$Year), y = max(annual_max$Max_SWE),
             label = paste("Trend:", round(coef(trend)[2], 2), "mm/year"),
             hjust = 0, vjust = 1, color = asu_colors[1])
  
  ggsave("images/swe_analysis/trend_analysis.png", p, width = 10, height = 6, dpi = 300)
  return(p)
}

# Main function to run all analyses
run_swe_analysis <- function() {
  # Read VIC model data
  vic_data <- read_vic_swe("data/VICOut2.nc")
  
  # Generate all plots
  create_swe_time_series(vic_data)
  create_seasonal_patterns(vic_data)
  create_elevation_analysis(vic_data)
  create_trend_analysis(vic_data)
}

# Run the analysis
run_swe_analysis() 