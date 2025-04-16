# Load required packages
library(sf)
library(ggplot2)
library(dplyr)
library(tidyr)
library(viridis)
library(plotly)
library(leaflet)
library(htmlwidgets)

# Read all data
basin <- st_read("data/Analysis_Basin_Shapefiles/basins_all_unique.shp")
huc10 <- st_read("data/huc10s/huc10.shp")
snotel_meta <- read.csv("data/snotel/snotel_metadata.csv")
snotel_data <- read.csv("data/snotel/snotel_CAP.csv")

# Convert date to Date type
snotel_data$date <- as.Date(snotel_data$date)

# 1. Interactive Basin Map with HUC10 and SNOTEL
basin_map <- ggplot() +
  geom_sf(data = basin, fill = "lightblue", color = "darkblue", size = 0.5) +
  geom_sf(data = huc10, fill = NA, color = "darkgreen", size = 0.1, alpha = 0.3) +
  geom_point(data = snotel_meta, aes(x = longitude, y = latitude, 
                                    color = state, size = elev,
                                    text = paste("Station:", site_name,
                                               "\nElevation:", elev, "m",
                                               "\nState:", state)),
             alpha = 0.7) +
  scale_color_viridis_d() +
  scale_size_continuous(range = c(1, 5)) +
  theme_minimal() +
  labs(title = "Colorado River Basin with HUC10 and SNOTEL Stations",
       color = "State",
       size = "Elevation (m)") +
  theme(plot.title = element_text(hjust = 0.5, size = 16))

# Convert to interactive plot
basin_map_interactive <- ggplotly(basin_map, tooltip = "text")
saveWidget(basin_map_interactive, "basin_huc10_snotel_interactive.html")

# 2. Enhanced SNOTEL Station Elevation Distribution
elev_plot <- ggplot(snotel_meta, aes(x = state, y = elev, fill = state,
                                    text = paste("State:", state,
                                               "\nMedian Elevation:", median(elev), "m",
                                               "\nNumber of Stations:", n()))) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.3) +
  scale_fill_viridis_d() +
  theme_minimal() +
  labs(title = "SNOTEL Station Elevation Distribution by State",
       x = "State",
       y = "Elevation (m)") +
  theme(plot.title = element_text(hjust = 0.5, size = 16))

# Convert to interactive plot
elev_plot_interactive <- ggplotly(elev_plot, tooltip = "text")
saveWidget(elev_plot_interactive, "snotel_elevation_interactive.html")

# 3. Enhanced HUC10 Area Distribution
area_plot <- ggplot(huc10, aes(x = areasqkm, 
                              text = paste("Area:", round(areasqkm, 2), "sq km",
                                         "\nState:", states))) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  geom_density(aes(y = ..count.. * 30), color = "red", size = 1) +
  theme_minimal() +
  labs(title = "Distribution of HUC10 Watershed Areas",
       x = "Area (sq km)",
       y = "Count") +
  theme(plot.title = element_text(hjust = 0.5, size = 16))

# Convert to interactive plot
area_plot_interactive <- ggplotly(area_plot, tooltip = "text")
saveWidget(area_plot_interactive, "huc10_area_interactive.html")

# 4. Enhanced SNOTEL Data Time Series
# Convert to long format
snotel_long <- snotel_data %>%
  pivot_longer(cols = -date, names_to = "station", values_to = "swe") %>%
  filter(!is.na(swe)) %>%
  left_join(snotel_meta, by = c("station" = "site_id"))

# Plot time series for a sample of stations with more information
ts_plot <- ggplot(snotel_long %>% filter(station %in% unique(station)[1:5]), 
                 aes(x = date, y = swe, color = station,
                     text = paste("Station:", site_name,
                                "\nDate:", date,
                                "\nSWE:", round(swe, 2), "mm",
                                "\nElevation:", elev, "m"))) +
  geom_line() +
  geom_point(alpha = 0.3) +
  scale_color_viridis_d() +
  theme_minimal() +
  labs(title = "Snow Water Equivalent Time Series (Sample Stations)",
       x = "Date",
       y = "SWE (mm)") +
  theme(plot.title = element_text(hjust = 0.5, size = 16))

# Convert to interactive plot
ts_plot_interactive <- ggplotly(ts_plot, tooltip = "text")
saveWidget(ts_plot_interactive, "snotel_timeseries_interactive.html")

# 5. New: Monthly SWE Statistics
monthly_stats <- snotel_long %>%
  mutate(month = format(date, "%m")) %>%
  group_by(station, month) %>%
  summarise(mean_swe = mean(swe, na.rm = TRUE),
            max_swe = max(swe, na.rm = TRUE),
            min_swe = min(swe, na.rm = TRUE)) %>%
  left_join(snotel_meta, by = c("station" = "site_id"))

monthly_plot <- ggplot(monthly_stats %>% filter(station %in% unique(station)[1:5]), 
                      aes(x = month, y = mean_swe, group = station,
                          text = paste("Station:", site_name,
                                     "\nMonth:", month,
                                     "\nMean SWE:", round(mean_swe, 2), "mm",
                                     "\nMax SWE:", round(max_swe, 2), "mm",
                                     "\nMin SWE:", round(min_swe, 2), "mm"))) +
  geom_line(aes(color = station)) +
  geom_point(aes(color = station)) +
  scale_color_viridis_d() +
  theme_minimal() +
  labs(title = "Monthly Snow Water Equivalent Statistics",
       x = "Month",
       y = "Mean SWE (mm)") +
  theme(plot.title = element_text(hjust = 0.5, size = 16))

# Convert to interactive plot
monthly_plot_interactive <- ggplotly(monthly_plot, tooltip = "text")
saveWidget(monthly_plot_interactive, "snotel_monthly_stats_interactive.html")

# 6. New: Elevation vs SWE Relationship
elev_swe_plot <- ggplot(snotel_long %>% filter(station %in% unique(station)[1:5]), 
                       aes(x = elev, y = swe, color = station,
                           text = paste("Station:", site_name,
                                      "\nElevation:", elev, "m",
                                      "\nSWE:", round(swe, 2), "mm"))) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_viridis_d() +
  theme_minimal() +
  labs(title = "Relationship between Elevation and Snow Water Equivalent",
       x = "Elevation (m)",
       y = "SWE (mm)") +
  theme(plot.title = element_text(hjust = 0.5, size = 16))

# Convert to interactive plot
elev_swe_plot_interactive <- ggplotly(elev_swe_plot, tooltip = "text")
saveWidget(elev_swe_plot_interactive, "elevation_swe_relationship_interactive.html") 