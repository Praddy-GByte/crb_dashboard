# Load required libraries
library(ncdf4)
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)
library(scales)

# Function to process VIC data and create combined time series
generate_combined_timeseries <- function() {
  # Read VIC data
  nc <- nc_open("data/VICOut2.nc")
  
  # Get time variable
  time <- ncvar_get(nc, "time")
  dates <- as.Date(time, origin = "0001-01-01")
  
  # Get all relevant variables
  soil_moisture <- apply(ncvar_get(nc, "OUT_SOIL_MOIST"), 4, mean, na.rm = TRUE)
  swe <- apply(ncvar_get(nc, "OUT_SWE"), 3, mean, na.rm = TRUE)
  precip <- apply(ncvar_get(nc, "OUT_PREC"), 3, mean, na.rm = TRUE)
  evap <- apply(ncvar_get(nc, "OUT_EVAP"), 3, mean, na.rm = TRUE)
  runoff <- apply(ncvar_get(nc, "OUT_RUNOFF"), 3, mean, na.rm = TRUE)
  baseflow <- apply(ncvar_get(nc, "OUT_BASEFLOW"), 3, mean, na.rm = TRUE)
  air_temp <- apply(ncvar_get(nc, "OUT_AIR_TEMP"), 3, mean, na.rm = TRUE)
  
  nc_close(nc)
  
  # Create data frame with all variables
  vic_data <- data.frame(
    date = dates,
    soil_moisture = soil_moisture,
    swe = swe,
    precipitation = precip,
    evapotranspiration = evap,
    runoff = runoff,
    baseflow = baseflow,
    air_temperature = air_temp
  )
  
  # Convert to long format for plotting
  vic_data_long <- vic_data %>%
    pivot_longer(cols = -date,
                names_to = "variable",
                values_to = "value")
  
  # Calculate annual averages
  vic_annual <- vic_data_long %>%
    mutate(year = year(date)) %>%
    group_by(year, variable) %>%
    summarise(annual_value = mean(value, na.rm = TRUE),
              .groups = "drop")
  
  # Create directory for output
  dir.create("images", showWarnings = FALSE)
  
  # Create combined time series plot
  p <- ggplot(vic_annual, aes(x = year, y = annual_value, color = variable)) +
    geom_line(linewidth = 1) +
    geom_point(size = 1) +
    facet_wrap(~variable, scales = "free_y", ncol = 2) +
    theme_minimal() +
    labs(title = "VIC Model Outputs - Basin-wide Annual Averages (1982-2024)",
         x = "Year",
         y = "Value",
         color = "Variable") +
    scale_color_viridis_d() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom",
      strip.text = element_text(size = 10),
      panel.grid.minor = element_blank(),
      panel.spacing = unit(1, "lines")
    )
  
  # Save the plot
  ggsave("images/vic_combined_timeseries.png", p, 
         width = 15, height = 12, dpi = 300)
  
  # Create a normalized version for direct comparison
  vic_annual_normalized <- vic_annual %>%
    group_by(variable) %>%
    mutate(normalized_value = scale(annual_value)[,1]) %>%
    ungroup()
  
  # Create normalized combined time series plot
  p_normalized <- ggplot(vic_annual_normalized, 
                        aes(x = year, y = normalized_value, color = variable)) +
    geom_line(linewidth = 1) +
    theme_minimal() +
    labs(title = "Normalized VIC Model Outputs (1982-2024)",
         subtitle = "All variables scaled to mean=0, sd=1 for comparison",
         x = "Year",
         y = "Normalized Value (z-score)",
         color = "Variable") +
    scale_color_viridis_d() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14),
      plot.subtitle = element_text(hjust = 0.5, size = 12),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "right",
      panel.grid.minor = element_blank()
    )
  
  # Save the normalized plot
  ggsave("images/vic_combined_timeseries_normalized.png", p_normalized, 
         width = 15, height = 8, dpi = 300)
}

# Execute the function
generate_combined_timeseries() 