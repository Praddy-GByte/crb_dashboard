# Helper functions for data processing and visualization

# Function to get available VIC years
get_available_vic_years <- function() {
  # Get the years from config
  years <- get_config("vic_years")
  
  # Return the years
  return(years)
}

# Data quality check functions
check_data_quality <- function(data, type) {
  quality <- list(
    completeness = 1,
    consistency = 1,
    timeliness = 1,
    validity = 1,
    accuracy = 1,
    precision = 1,
    coverage = 1,
    reliability = 1,
    temporal_resolution = 1,
    spatial_resolution = 1,
    data_gaps = 1,
    outlier_frequency = 1,
    measurement_uncertainty = 1,
    processing_quality = 1,
    metadata_completeness = 1,
    data_freshness = 1
  )
  
  tryCatch({
    switch(type,
      "basin" = {
        if (inherits(data, "sf")) {
          quality$validity <- 1
        }
        if (any(is.na(data$geometry))) {
          quality$completeness <- 0.9
        }
        areas <- st_area(data)
        if (length(areas) > 1) {
          areas_km2 <- set_units(areas, "km^2")
          quality$consistency <- ifelse(sd(as.numeric(areas_km2)) > mean(as.numeric(areas_km2)) * 0.1, 0.9, 1)
        }
        bbox <- st_bbox(data)
        bbox_width <- set_units(bbox[3] - bbox[1], "km")
        bbox_height <- set_units(bbox[4] - bbox[2], "km")
        if (as.numeric(bbox_width) < 100 || as.numeric(bbox_height) < 100) {
          quality$coverage <- 0.8
        }
      },
      "huc10" = {
        if (inherits(data, "sf")) {
          quality$validity <- 1
        }
        if (any(is.na(data$geometry))) {
          quality$completeness <- 0.9
        }
        if (!all(grepl("^\\d{10}$", data$huc10))) {
          quality$accuracy <- 0.9
        }
        areas <- st_area(data)
        if (length(areas) > 1) {
          areas_km2 <- set_units(areas, "km^2")
          quality$consistency <- ifelse(sd(as.numeric(areas_km2)) > mean(as.numeric(areas_km2)) * 0.1, 0.9, 1)
        }
      },
      "snotel" = {
        missing_pct <- mean(is.na(data$WTEQ))
        quality$completeness <- 1 - missing_pct
        outliers <- sum(abs(scale(data$WTEQ)) > 3, na.rm = TRUE)
        if (outliers > 0) {
          quality$consistency <- 0.9
        }
        date_seq <- seq(min(data$date), max(data$date), by = "day")
        missing_dates <- sum(!date_seq %in% data$date)
        if (missing_dates > 0) {
          quality$timeliness <- 0.9
        }
        stations <- unique(data$site_id)
        if (length(stations) < 100) {
          quality$coverage <- 0.8
        }
        if (any(data$WTEQ %% 0.1 != 0, na.rm = TRUE)) {
          quality$precision <- 0.9
        }
      },
      "vic" = {
        if (is.list(data)) {
          missing_pct <- mean(sapply(data, function(x) mean(is.na(x))))
          quality$completeness <- 1 - missing_pct
          if (length(unique(sapply(data, length))) > 1) {
            quality$consistency <- 0.9
          }
          ranges <- sapply(data, function(x) {
            if (is.numeric(x)) {
              return(max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
            }
            return(0)
          })
          if (any(ranges == 0)) {
            quality$reliability <- 0.9
          }
        }
      },
      "smap" = {
        if (is.list(data)) {
          missing_pct <- mean(sapply(data, function(x) mean(is.na(x))))
          quality$completeness <- 1 - missing_pct
          if (length(data) < 365) {
            quality$timeliness <- 0.8
          }
          res <- res(data[[1]])
          if (res[1] > 9000 || res[2] > 9000) {
            quality$precision <- 0.9
          }
          ranges <- sapply(data, function(x) {
            if (is.numeric(x)) {
              return(max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
            }
            return(0)
          })
          if (any(ranges == 0)) {
            quality$reliability <- 0.9
          }
        }
      },
      "grace" = {
        if (is.list(data)) {
          missing_pct <- mean(sapply(data, function(x) mean(is.na(x))))
          quality$completeness <- 1 - missing_pct
          if (length(data) < 12) {
            quality$timeliness <- 0.8
          }
          res <- res(data[[1]])
          if (res[1] > 100000 || res[2] > 100000) {
            quality$precision <- 0.9
          }
          if ("uncertainty" %in% names(data)) {
            if (mean(data$uncertainty, na.rm = TRUE) > 2) {
              quality$accuracy <- 0.9
            }
          }
        }
      }
    )
    return(quality)
  }, error = function(e) {
    message(paste("Error checking data quality:", e$message))
    return(quality)
  })
}

# Plotting helper functions
create_time_series_plot <- function(data, x_var, y_var, title = NULL) {
  p <- ggplot(data, aes_string(x = x_var, y = y_var)) +
    geom_line(color = "blue") +
    theme_minimal() +
    labs(x = x_var, y = y_var, title = title) +
    theme(plot.title = element_text(hjust = 0.5))
  return(ggplotly(p))
}

create_monthly_plot <- function(data, x_var, y_var, title = NULL) {
  monthly_data <- data %>%
    mutate(month = format(.data[[x_var]], "%Y-%m")) %>%
    group_by(month) %>%
    summarise(
      mean = mean(.data[[y_var]], na.rm = TRUE),
      min = min(.data[[y_var]], na.rm = TRUE),
      max = max(.data[[y_var]], na.rm = TRUE),
      .groups = "drop"
    )
  
  p <- ggplot(monthly_data, aes(x = month)) +
    geom_line(aes(y = mean, group = 1), color = "blue") +
    geom_ribbon(aes(ymin = min, ymax = max), alpha = 0.2, fill = "blue") +
    theme_minimal() +
    labs(x = "Month", y = y_var, title = title) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(hjust = 0.5))
  return(ggplotly(p))
}

create_spatial_plot <- function(data, x_var, y_var, value_var, title = NULL) {
  if (inherits(data, "RasterLayer")) {
    map_data <- as.data.frame(data, xy = TRUE)
    names(map_data) <- c("x", "y", "value")
    
    p <- ggplot(map_data, aes(x = x, y = y, fill = value)) +
      geom_raster() +
      scale_fill_viridis_c() +
      theme_minimal() +
      labs(x = "Longitude", y = "Latitude", fill = value_var, title = title) +
      theme(plot.title = element_text(hjust = 0.5))
  } else {
    p <- ggplot(data, aes_string(x = value_var)) +
      geom_histogram(fill = "blue", alpha = 0.7) +
      theme_minimal() +
      labs(x = value_var, y = "Count", title = title) +
      theme(plot.title = element_text(hjust = 0.5))
  }
  return(ggplotly(p))
}

# Data processing helper functions
process_vic_data <- function(file_path) {
  tryCatch({
    nc <- nc_open(file_path)
    vic_data <- list(
      OUT_PREC = ncvar_get(nc, "OUT_PREC"),
      OUT_SWE = ncvar_get(nc, "OUT_SWE"),
      OUT_RUNOFF = ncvar_get(nc, "OUT_RUNOFF"),
      OUT_BASEFLOW = ncvar_get(nc, "OUT_BASEFLOW"),
      OUT_EVAP = ncvar_get(nc, "OUT_EVAP"),
      OUT_AIR_TEMP = ncvar_get(nc, "OUT_AIR_TEMP"),
      OUT_SOIL_MOIST_1 = ncvar_get(nc, "OUT_SOIL_MOIST_1"),
      OUT_SOIL_MOIST_2 = ncvar_get(nc, "OUT_SOIL_MOIST_2"),
      OUT_SOIL_MOIST_3 = ncvar_get(nc, "OUT_SOIL_MOIST_3")
    )
    nc_close(nc)
    return(vic_data)
  }, error = function(e) {
    message(paste("Error processing VIC data:", e$message))
    return(NULL)
  })
}

# Data validation helper functions
validate_data <- function(data, type) {
  tryCatch({
    switch(type,
      "basin" = {
        if (!inherits(data, "sf")) {
          stop("Basin data is not a valid sf object")
        }
        if (!all(c("geometry") %in% names(data))) {
          stop("Basin data is missing required columns")
        }
      },
      "huc10" = {
        if (!inherits(data, "sf")) {
          stop("HUC10 data is not a valid sf object")
        }
        if (!all(c("geometry", "huc10") %in% names(data))) {
          stop("HUC10 data is missing required columns")
        }
      },
      "snotel" = {
        if (!is.data.frame(data)) {
          stop("SNOTEL data is not a valid data frame")
        }
        if (!all(c("date", "WTEQ", "PREC", "TEMP", "station_id") %in% names(data))) {
          stop("SNOTEL data is missing required columns")
        }
        if (!inherits(data$date, "Date")) {
          stop("SNOTEL date column is not of type Date")
        }
      },
      "vic" = {
        if (!is.list(data)) {
          stop("VIC data is not a valid list")
        }
        required_vars <- c("OUT_PREC", "OUT_SWE", "OUT_RUNOFF", "OUT_BASEFLOW", 
                          "OUT_EVAP", "OUT_AIR_TEMP", "OUT_SOIL_MOIST_1", 
                          "OUT_SOIL_MOIST_2", "OUT_SOIL_MOIST_3")
        if (!all(required_vars %in% names(data))) {
          stop("VIC data is missing required variables")
        }
      }
    )
    return(TRUE)
  }, error = function(e) {
    message(paste("Data validation error:", e$message))
    return(FALSE)
  })
} 