library(sf)
library(raster)
library(plotly)
library(dplyr)
library(tidyr)
library(htmlwidgets)

# Load CRB boundary
crb <- st_read('data/Analysis_Basin_Shapefiles/basin_CRB_poly.shp')
crb_coords <- st_coordinates(crb)

# Load SMAP data
smap_data <- readRDS('data/smap_processed/smap.rds')

# Create a function to process SMAP data for a specific date and layer
process_smap_date <- function(data, date_index = 1, layer = "surface") {
  # Extract soil moisture for the given date and first time period
  if (layer == "surface") {
    sm <- data$surface[1,,,date_index]
  } else if (layer == "rootzone") {
    sm <- data$rootzone[1,,,date_index]
  } else {
    sm <- data$profile[1,,,date_index]
  }
  
  # Create a data frame with coordinates and values
  df <- expand.grid(
    lon = data$lon,
    lat = data$lat
  )
  
  # Reshape the 2D array into a vector
  df$value <- as.vector(sm)
  
  # Remove NA values
  df <- na.omit(df)
  
  # Filter to CRB extent
  df <- df %>%
    filter(
      lon >= st_bbox(crb)["xmin"] & 
      lon <= st_bbox(crb)["xmax"] &
      lat >= st_bbox(crb)["ymin"] & 
      lat <= st_bbox(crb)["ymax"]
    )
  
  return(df)
}

# Process time series data
process_time_series <- function(data) {
  n_dates <- length(data$dates)
  
  # Initialize vectors for mean values
  surface_means <- numeric(n_dates)
  rootzone_means <- numeric(n_dates)
  profile_means <- numeric(n_dates)
  
  # Calculate mean for each date
  for(i in 1:n_dates) {
    surface_means[i] <- mean(process_smap_date(data, i, "surface")$value, na.rm = TRUE)
    rootzone_means[i] <- mean(process_smap_date(data, i, "rootzone")$value, na.rm = TRUE)
    profile_means[i] <- mean(process_smap_date(data, i, "profile")$value, na.rm = TRUE)
  }
  
  # Create data frame
  df <- data.frame(
    date = data$dates,
    surface = surface_means,
    rootzone = rootzone_means,
    profile = profile_means
  )
  
  return(df)
}

# Process the most recent date for all layers
latest_surface <- process_smap_date(smap_data, length(smap_data$dates), "surface")
latest_rootzone <- process_smap_date(smap_data, length(smap_data$dates), "rootzone")
latest_profile <- process_smap_date(smap_data, length(smap_data$dates), "profile")

# Create spatial plots
create_spatial_plot <- function(data, title, colorscale = "Viridis") {
  plot_ly() %>%
    add_trace(
      data = data,
      x = ~lon,
      y = ~lat,
      z = ~value,
      type = "contour",
      colorscale = colorscale,
      contours = list(
        showlabels = TRUE,
        labelfont = list(size = 12, color = "white")
      )
    ) %>%
    add_trace(
      x = crb_coords[,1],
      y = crb_coords[,2],
      type = "scatter",
      mode = "lines",
      line = list(color = "black", width = 2),
      name = "CRB Boundary",
      showlegend = TRUE
    ) %>%
    layout(
      title = title,
      xaxis = list(title = "Longitude", range = c(st_bbox(crb)["xmin"], st_bbox(crb)["xmax"])),
      yaxis = list(title = "Latitude", range = c(st_bbox(crb)["ymin"], st_bbox(crb)["ymax"])),
      showlegend = TRUE
    )
}

# Create spatial plots for each layer
p1 <- create_spatial_plot(latest_surface, 
                         paste("SMAP Surface Soil Moisture (0-5cm) -", 
                               format(smap_data$dates[length(smap_data$dates)], "%Y-%m-%d")))

p2 <- create_spatial_plot(latest_rootzone,
                         paste("SMAP Root Zone Soil Moisture (0-100cm) -",
                               format(smap_data$dates[length(smap_data$dates)], "%Y-%m-%d")),
                         "Earth")

p3 <- create_spatial_plot(latest_profile,
                         paste("SMAP Profile Soil Moisture (0-bedrock) -",
                               format(smap_data$dates[length(smap_data$dates)], "%Y-%m-%d")),
                         "Jet")

# Process and create time series plot
ts_data <- process_time_series(smap_data)

p4 <- plot_ly(data = ts_data) %>%
  add_trace(x = ~date, y = ~surface, name = "Surface (0-5cm)", 
            type = "scatter", mode = "lines", line = list(color = "blue")) %>%
  add_trace(x = ~date, y = ~rootzone, name = "Root Zone (0-100cm)", 
            type = "scatter", mode = "lines", line = list(color = "green")) %>%
  add_trace(x = ~date, y = ~profile, name = "Profile (0-bedrock)", 
            type = "scatter", mode = "lines", line = list(color = "red")) %>%
  layout(
    title = "SMAP Soil Moisture Time Series for CRB",
    xaxis = list(title = "Date"),
    yaxis = list(title = "Soil Moisture (m³/m³)"),
    showlegend = TRUE
  )

# Create a subplot with all visualizations
subplot_all <- subplot(p1, p2, p3, p4, nrows = 2, shareX = FALSE, shareY = FALSE) %>%
  layout(
    title = "SMAP Soil Moisture Analysis for Colorado River Basin",
    showlegend = TRUE,
    height = 1000
  )

# Save the plots
saveWidget(subplot_all, "smap_crb_comprehensive.html")
saveWidget(p4, "smap_crb_timeseries.html") 