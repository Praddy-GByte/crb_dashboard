# Load required packages
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(sf)
library(raster)
library(ncdf4)

# Create sample data since actual data files are not available
create_sample_data <- function() {
  # Create date sequence from 2015 to 2024
  dates <- seq(as.Date("2015-01-01"), as.Date("2024-01-01"), by = "month")
  
  # Create sample VIC data
  vic_data <- list(
    data = array(rnorm(length(dates), mean = 0.3, sd = 0.1), dim = c(1, 1, length(dates))),
    dates = dates
  )
  
  # Create sample SMAP data
  smap_data <- list(
    surface_means = rnorm(length(dates), mean = 0.2, sd = 0.05),
    rootzone_means = rnorm(length(dates), mean = 0.25, sd = 0.03),
    dates = dates
  )
  
  # Create sample GRACE data
  grace_data <- list(
    tws = array(rnorm(length(dates), mean = -10, sd = 5), dim = c(1, 1, length(dates))),
    uncertainty = array(rep(1, length(dates)), dim = c(1, 1, length(dates))),
    dates = dates
  )
  
  # Create sample SNOTEL data
  snotel_data <- data.frame(
    date = rep(dates, each = 5),
    variable = "WTEQ",
    value = rnorm(length(dates) * 5, mean = 10, sd = 5)
  )
  
  return(list(
    vic = vic_data,
    smap = smap_data,
    grace = grace_data,
    snotel = snotel_data
  ))
}

# Create output directory if it doesn't exist
if (!dir.exists("images")) {
  dir.create("images")
}

# Function to create trend plot
create_trend_plot <- function(data, title, y_label, color) {
  p <- ggplot(data, aes(x = Date, y = Value)) +
    geom_line(linewidth = 1, color = color) +
    geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
    theme_minimal() +
    labs(title = title,
         x = "Date",
         y = y_label) +
    theme(plot.title = element_text(hjust = 0.5, size = 16),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10))
  return(p)
}

# Load sample data
sample_data <- create_sample_data()

# 1. VIC Soil Moisture Trend
vic_means <- as.vector(sample_data$vic$data)
vic_df <- data.frame(
  Date = sample_data$vic$dates,
  Value = vic_means
)

p <- create_trend_plot(vic_df, 
                      "VIC Model Soil Moisture Trend - Colorado River Basin",
                      "Soil Moisture",
                      "#8C1D40")
ggsave("images/vic_trend.png", p, width = 10, height = 6, dpi = 300)

# 2. SMAP Trends
# Surface Soil Moisture
smap_surface_df <- data.frame(
  Date = sample_data$smap$dates,
  Value = sample_data$smap$surface_means
)

p <- create_trend_plot(smap_surface_df,
                      "SMAP Surface Soil Moisture Trend - Colorado River Basin",
                      "Soil Moisture (m³/m³)",
                      "#FFB81C")
ggsave("images/smap_surface_trend.png", p, width = 10, height = 6, dpi = 300)

# Root Zone Soil Moisture
smap_rootzone_df <- data.frame(
  Date = sample_data$smap$dates,
  Value = sample_data$smap$rootzone_means
)

p <- create_trend_plot(smap_rootzone_df,
                      "SMAP Root Zone Soil Moisture Trend - Colorado River Basin",
                      "Soil Moisture (m³/m³)",
                      "#00A3E0")
ggsave("images/smap_rootzone_trend.png", p, width = 10, height = 6, dpi = 300)

# 3. GRACE TWS Trend
grace_means <- as.vector(sample_data$grace$tws)
grace_df <- data.frame(
  Date = sample_data$grace$dates,
  Value = grace_means
)

p <- create_trend_plot(grace_df,
                      "GRACE TWS Trend - Colorado River Basin",
                      "Terrestrial Water Storage (cm)",
                      "#78BE20")
ggsave("images/grace_trend.png", p, width = 10, height = 6, dpi = 300)

# 4. SNOTEL SWE Trend
snotel_means <- sample_data$snotel %>%
  group_by(date) %>%
  summarise(Value = mean(value, na.rm = TRUE)) %>%
  rename(Date = date)

p <- create_trend_plot(snotel_means,
                      "SNOTEL SWE Trend - Colorado River Basin",
                      "Snow Water Equivalent (inches)",
                      "#FF7F32")
ggsave("images/snotel_trend.png", p, width = 10, height = 6, dpi = 300)

# 5. Combined Trend Plot
# Create a combined data frame
combined_df <- bind_rows(
  vic_df %>% mutate(Variable = "VIC Soil Moisture"),
  smap_surface_df %>% mutate(Variable = "SMAP Surface"),
  smap_rootzone_df %>% mutate(Variable = "SMAP Root Zone"),
  grace_df %>% mutate(Variable = "GRACE TWS"),
  snotel_means %>% mutate(Variable = "SNOTEL SWE")
)

# Create combined plot
p <- ggplot(combined_df, aes(x = Date, y = Value, color = Variable)) +
  geom_line(linewidth = 1) +
  theme_minimal() +
  labs(title = "Historical Trends Analysis - Colorado River Basin",
       x = "Date",
       y = "Value (Normalized)") +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.position = "bottom",
        legend.title = element_blank()) +
  scale_color_manual(values = c("#8C1D40", "#FFB81C", "#00A3E0", "#78BE20", "#FF7F32"))

# Normalize the values for better comparison
combined_df <- combined_df %>%
  group_by(Variable) %>%
  mutate(Value = (Value - mean(Value)) / sd(Value)) %>%
  ungroup()

# Create normalized combined plot
p_normalized <- ggplot(combined_df, aes(x = Date, y = Value, color = Variable)) +
  geom_line(linewidth = 1) +
  theme_minimal() +
  labs(title = "Historical Trends Analysis - Colorado River Basin (Normalized)",
       x = "Date",
       y = "Normalized Value (z-score)") +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.position = "bottom",
        legend.title = element_blank()) +
  scale_color_manual(values = c("#8C1D40", "#FFB81C", "#00A3E0", "#78BE20", "#FF7F32"))

ggsave("images/combined_trends.png", p, width = 12, height = 8, dpi = 300)
ggsave("images/combined_trends_normalized.png", p_normalized, width = 12, height = 8, dpi = 300)

# Print completion message
cat("All trend images have been generated and saved to the 'images' directory.\n") 