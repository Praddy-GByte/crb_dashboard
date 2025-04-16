# Load required libraries
library(ncdf4)
library(ggplot2)
library(dplyr)
library(lubridate)
library(gridExtra)
library(scales)

# Function to read and process VIC data
process_vic_data <- function(file_path) {
  nc <- nc_open(file_path)
  time <- ncvar_get(nc, "time")
  soil_moisture <- ncvar_get(nc, "SOIL_MOISTURE")
  swe <- ncvar_get(nc, "SWE")
  nc_close(nc)
  
  # Convert time to dates (assuming time is in days since 1982-01-01)
  dates <- as.Date(time, origin = "1982-01-01")
  
  # Filter data to 1982-2024 range
  data <- data.frame(
    date = dates,
    soil_moisture = soil_moisture,
    swe = swe
  )
  
  return(data %>% filter(date >= as.Date("1982-01-01") & date <= as.Date("2024-12-31")))
}

# Function to read and process SMAP data
process_smap_data <- function(file_path) {
  nc <- nc_open(file_path)
  time <- ncvar_get(nc, "time")
  soil_moisture <- ncvar_get(nc, "soil_moisture")
  nc_close(nc)
  
  # SMAP data starts from 2015
  dates <- as.Date(time, origin = "2015-01-01")
  
  data <- data.frame(
    date = dates,
    smap_soil_moisture = soil_moisture
  )
  
  return(data %>% filter(date <= as.Date("2024-12-31")))
}

# Function to read and process GRACE data
process_grace_data <- function(file_path) {
  nc <- nc_open(file_path)
  time <- ncvar_get(nc, "time")
  tws <- ncvar_get(nc, "tws")
  nc_close(nc)
  
  # GRACE data starts from 2002
  dates <- as.Date(time, origin = "2002-01-01")
  
  data <- data.frame(
    date = dates,
    grace_tws = tws
  )
  
  return(data %>% filter(date <= as.Date("2024-12-31")))
}

# Function to read and process precipitation data
process_precip_data <- function(file_path) {
  nc <- nc_open(file_path)
  time <- ncvar_get(nc, "time")
  precip <- ncvar_get(nc, "precipitation")
  nc_close(nc)
  
  dates <- as.Date(time, origin = "1982-01-01")
  
  data <- data.frame(
    date = dates,
    precipitation = precip
  )
  
  return(data %>% filter(date >= as.Date("1982-01-01") & date <= as.Date("2024-12-31")))
}

# Function to create time series plot
create_time_series_plot <- function(data, variable_name, y_label, color, start_year, end_year) {
  ggplot(data, aes(x = date, y = !!sym(variable_name))) +
    geom_line(color = color, size = 1) +
    theme_minimal() +
    labs(
      title = paste("Time Series:", variable_name),
      x = "Date",
      y = y_label
    ) +
    scale_x_date(
      breaks = seq(as.Date(paste0(start_year, "-01-01")), 
                  as.Date(paste0(end_year, "-12-31")), 
                  by = "2 years"),
      labels = date_format("%Y"),
      limits = c(as.Date(paste0(start_year, "-01-01")), 
                as.Date(paste0(end_year, "-12-31")))
    ) +
    theme(
      plot.title = element_text(hjust = 0.5),
      panel.grid.major = element_line(color = "gray80"),
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1)
    ) +
    annotate(
      "text",
      x = as.Date(paste0(end_year, "-01-01")),
      y = max(data[[variable_name]], na.rm = TRUE),
      label = paste0(start_year, "-", end_year),
      hjust = 1,
      vjust = 1,
      size = 4
    )
}

# Main function to generate all plots
generate_all_plots <- function() {
  # Read and process data
  vic_data <- process_vic_data("data/VICOut2.nc")
  smap_data <- process_smap_data("data/SPL4SMGP.007_9km_aid0001.nc")
  grace_data <- process_grace_data("data/GRCTellus.JPL.200204_202401.GLO.RL06.1M.MSCNv03CRI.nc")
  precip_data <- process_precip_data("data/CRB_PRISM_Calibrated.2024-01-01.nc")
  
  # Create plots with correct time ranges
  plots <- list(
    create_time_series_plot(vic_data, "soil_moisture", "Soil Moisture (mm)", "#1f77b4", 1982, 2024),
    create_time_series_plot(vic_data, "swe", "Snow Water Equivalent (mm)", "#2ca02c", 1982, 2024),
    create_time_series_plot(smap_data, "smap_soil_moisture", "SMAP Soil Moisture (mm)", "#ff7f0e", 2015, 2024),
    create_time_series_plot(grace_data, "grace_tws", "GRACE TWS (mm)", "#d62728", 2002, 2024),
    create_time_series_plot(precip_data, "precipitation", "Precipitation (mm)", "#9467bd", 1982, 2024)
  )
  
  # Save plots
  dir.create("images", showWarnings = FALSE)
  
  # Save individual plots with proper names
  plot_names <- c("vic_soil_moisture", "vic_swe", "smap_soil_moisture", "grace_tws", "precipitation")
  
  for (i in seq_along(plots)) {
    ggsave(
      paste0("images/", plot_names[i], "_time_series.png"),
      plots[[i]],
      width = 12,
      height = 6,
      dpi = 300
    )
  }
  
  # Create a combined plot with proper layout
  combined_plot <- grid.arrange(
    grobs = plots,
    ncol = 2,
    top = "Time Series Analysis (1982-2024)",
    bottom = "Note: SMAP data starts from 2015, GRACE data starts from 2002"
  )
  
  ggsave(
    "images/combined_time_series.png",
    combined_plot,
    width = 20,
    height = 15,
    dpi = 300
  )
}

# Execute the main function
generate_all_plots() 