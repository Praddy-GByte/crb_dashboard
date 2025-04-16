# Load required packages
library(sf)
library(ggplot2)
library(viridis)

# Function to save plot as image
save_plot <- function(plot, filename, width = 10, height = 8) {
  ggsave(filename, plot, width = width, height = height, dpi = 300)
}

# 1. Colorado Basin Map
basin <- st_read("data/Analysis_Basin_Shapefiles/basins_all_unique.shp")
basin_plot <- ggplot() +
  geom_sf(data = basin, aes(fill = as.factor(Id)), color = "darkblue", size = 0.5) +
  scale_fill_viridis_d() +
  theme_minimal() +
  labs(title = "Colorado River Basin", fill = "Basin ID") +
  theme(plot.title = element_text(hjust = 0.5, size = 16))
save_plot(basin_plot, "colorado_basin.png")

# 2. HUC10 Map
huc10 <- st_read("data/huc10s/huc10.shp")
huc10_plot <- ggplot() +
  geom_sf(data = huc10, aes(fill = states), color = "darkblue", size = 0.2) +
  scale_fill_viridis_d() +
  theme_minimal() +
  labs(title = "HUC10 Watersheds", fill = "States") +
  theme(plot.title = element_text(hjust = 0.5, size = 16))
save_plot(huc10_plot, "huc10_map.png")

# 3. SNOTEL Stations Map
snotel <- read.csv("data/snotel/snotel_metadata.csv")
snotel_plot <- ggplot() +
  geom_sf(data = basin, fill = "lightblue", color = "darkblue", size = 0.5) +
  geom_point(data = snotel, aes(x = longitude, y = latitude), 
             color = "red", size = 2) +
  theme_minimal() +
  labs(title = "SNOTEL Stations in Colorado Basin") +
  theme(plot.title = element_text(hjust = 0.5, size = 16))
save_plot(snotel_plot, "snotel_map.png")

# 4. Combined Map
combined_plot <- ggplot() +
  geom_sf(data = basin, fill = "lightblue", color = "darkblue", size = 0.5) +
  geom_sf(data = huc10, fill = NA, color = "darkgreen", size = 0.1, alpha = 0.3) +
  geom_point(data = snotel, aes(x = longitude, y = latitude), 
             color = "red", size = 2) +
  theme_minimal() +
  labs(title = "Colorado Basin with HUC10 and SNOTEL Stations") +
  theme(plot.title = element_text(hjust = 0.5, size = 16))
save_plot(combined_plot, "combined_map.png") 