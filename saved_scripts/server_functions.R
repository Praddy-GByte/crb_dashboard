# Server functions for the Hydrological Dashboard

# VIC Data functions
vic_data <- reactive({
  year <- input$vic_year
  variable <- input$vic_variable
  
  # Create a mapping of display names to NetCDF variable names
  variable_mapping <- list(
    "Precipitation" = "OUT_PREC",
    "Evapotranspiration" = "OUT_EVAP",
    "Runoff" = "OUT_RUNOFF",
    "Snow Water Equivalent" = "OUT_SWE",
    "Soil Moisture" = "OUT_SOIL_MOIST"
  )
  
  # Get the actual NetCDF variable name
  variable_name <- variable_mapping[[variable]]
  if (is.null(variable_name)) {
    showNotification("Invalid variable selected", type = "error")
    return(NULL)
  }
  
  # Read the NetCDF file for the selected year
  vic_file <- file.path("data/VIC_outputs", paste0("VIC_", year, ".nc"))
  if (!file.exists(vic_file)) {
    showNotification("File not found for selected year", type = "error")
    return(NULL)
  }
  
  tryCatch({
    nc <- nc_open(vic_file)
    var_data <- ncvar_get(nc, variable_name)
    lon <- ncvar_get(nc, "lon")
    lat <- ncvar_get(nc, "lat")
    time <- ncvar_get(nc, "time")
    dates <- as.Date(time, origin = "1900-01-01")
    nc_close(nc)
    
    return(list(
      data = var_data,
      lon = lon,
      lat = lat,
      dates = dates,
      variable = variable
    ))
  }, error = function(e) {
    showNotification(paste("Error reading NetCDF file:", e$message), type = "error")
    return(NULL)
  })
})

# GRACE Data functions
grace_data <- reactive({
  year <- input$grace_year
  variable <- input$grace_variable
  
  # Create a mapping of display names to NetCDF variable names
  variable_mapping <- list(
    "Total Water Storage" = "tws",
    "Uncertainty" = "uncertainty"
  )
  
  # Get the actual NetCDF variable name
  variable_name <- variable_mapping[[variable]]
  if (is.null(variable_name)) {
    showNotification("Invalid variable selected", type = "error")
    return(NULL)
  }
  
  # Read the NetCDF file
  grace_file <- "data/GRCTellus.JPL.200204_202401.GLO.RL06.1M.MSCNv03CRI.nc"
  if (!file.exists(grace_file)) {
    showNotification("GRACE data file not found", type = "error")
    return(NULL)
  }
  
  tryCatch({
    nc <- nc_open(grace_file)
    var_data <- ncvar_get(nc, variable_name)
    lon <- ncvar_get(nc, "lon")
    lat <- ncvar_get(nc, "lat")
    time <- ncvar_get(nc, "time")
    dates <- as.Date(time, origin = "2002-01-01")
    nc_close(nc)
    
    # Filter for Colorado River Basin
    lon_idx <- which(lon >= -115 & lon <= -105)
    lat_idx <- which(lat >= 31 & lat <= 42)
    
    filtered_data <- var_data[lon_idx, lat_idx, ]
    filtered_lon <- lon[lon_idx]
    filtered_lat <- lat[lat_idx]
    
    return(list(
      data = filtered_data,
      lon = filtered_lon,
      lat = filtered_lat,
      dates = dates,
      variable = variable
    ))
  }, error = function(e) {
    showNotification(paste("Error reading GRACE data:", e$message), type = "error")
    return(NULL)
  })
})

# PRISM Data functions
prism_data <- reactive({
  year <- input$prism_year
  variable <- input$prism_variable
  
  # Create a mapping of display names to NetCDF variable names
  variable_mapping <- list(
    "Precipitation" = "precipitation",
    "Temperature" = "temperature"
  )
  
  # Get the actual NetCDF variable name
  variable_name <- variable_mapping[[variable]]
  if (is.null(variable_name)) {
    showNotification("Invalid variable selected", type = "error")
    return(NULL)
  }
  
  # Read the NetCDF file for the selected year
  prism_file <- file.path("data/PRISM_outputs", paste0("PRISM_", year, "_", variable, ".nc"))
  if (!file.exists(prism_file)) {
    showNotification("File not found for selected year", type = "error")
    return(NULL)
  }
  
  tryCatch({
    nc <- nc_open(prism_file)
    var_data <- ncvar_get(nc, variable_name)
    lon <- ncvar_get(nc, "lon")
    lat <- ncvar_get(nc, "lat")
    time <- ncvar_get(nc, "time")
    dates <- as.Date(time, origin = "1900-01-01")
    nc_close(nc)
    
    # Filter for Colorado River Basin
    lon_idx <- which(lon >= -115 & lon <= -105)
    lat_idx <- which(lat >= 31 & lat <= 42)
    
    filtered_data <- var_data[lon_idx, lat_idx, ]
    filtered_lon <- lon[lon_idx]
    filtered_lat <- lat[lat_idx]
    
    return(list(
      data = filtered_data,
      lon = filtered_lon,
      lat = filtered_lat,
      dates = dates,
      variable = variable
    ))
  }, error = function(e) {
    showNotification(paste("Error reading PRISM data:", e$message), type = "error")
    return(NULL)
  })
})

# Output functions for GRACE data
output$grace_spatial_map <- renderPlotly({
  data <- grace_data()
  if (is.null(data)) return(NULL)
  
  tryCatch({
    # Calculate spatial mean for the selected year
    spatial_mean <- apply(data$data, c(1,2), mean, na.rm = TRUE)
    
    # Create a data frame for plotting
    plot_data <- expand.grid(lon = data$lon, lat = data$lat)
    plot_data$value <- as.vector(spatial_mean)
    
    plot_ly(
      data = plot_data,
      x = ~lon,
      y = ~lat,
      z = ~value,
      type = "heatmap",
      colorscale = if(data$variable == "Total Water Storage") "RdBu" else "Viridis"
    ) %>%
      layout(
        title = paste("Spatial Distribution -", data$variable),
        xaxis = list(title = "Longitude", range = c(-115, -105)),
        yaxis = list(title = "Latitude", range = c(31, 42)),
        plot_bgcolor = 'white',
        paper_bgcolor = 'white'
      )
  }, error = function(e) {
    showNotification(paste("Error creating spatial plot:", e$message), type = "error")
    return(NULL)
  })
})

# Output functions for PRISM data
output$prism_spatial_map <- renderPlotly({
  data <- prism_data()
  if (is.null(data)) return(NULL)
  
  tryCatch({
    # Calculate spatial mean for the selected year
    spatial_mean <- apply(data$data, c(1,2), mean, na.rm = TRUE)
    
    # Create a data frame for plotting
    plot_data <- expand.grid(lon = data$lon, lat = data$lat)
    plot_data$value <- as.vector(spatial_mean)
    
    plot_ly(
      data = plot_data,
      x = ~lon,
      y = ~lat,
      z = ~value,
      type = "heatmap",
      colorscale = if(data$variable == "Precipitation") "Viridis" else "RdBu"
    ) %>%
      layout(
        title = paste("Spatial Distribution -", data$variable),
        xaxis = list(title = "Longitude", range = c(-115, -105)),
        yaxis = list(title = "Latitude", range = c(31, 42)),
        plot_bgcolor = 'white',
        paper_bgcolor = 'white'
      )
  }, error = function(e) {
    showNotification(paste("Error creating spatial plot:", e$message), type = "error")
    return(NULL)
  })
})

# Additional output functions for GRACE data
output$grace_timeseries <- renderPlotly({
  data <- grace_data()
  if (is.null(data)) return(NULL)
  
  tryCatch({
    # Calculate spatial mean for each time step
    time_series <- apply(data$data, 3, mean, na.rm = TRUE)
    
    # Create a data frame for plotting
    plot_data <- data.frame(
      date = data$dates,
      value = time_series
    )
    
    # Aggregate based on selection
    if (input$grace_aggregation == "Monthly") {
      plot_data <- plot_data %>%
        mutate(date = floor_date(date, "month")) %>%
        group_by(date) %>%
        summarise(value = mean(value, na.rm = TRUE))
    } else if (input$grace_aggregation == "Seasonal") {
      plot_data <- plot_data %>%
        mutate(season = paste(year(date), quarter(date))) %>%
        group_by(season) %>%
        summarise(value = mean(value, na.rm = TRUE))
    } else { # Annual
      plot_data <- plot_data %>%
        mutate(year = year(date)) %>%
        group_by(year) %>%
        summarise(value = mean(value, na.rm = TRUE))
    }
    
    plot_ly(
      data = plot_data,
      x = ~if(input$grace_aggregation == "Monthly") date else if(input$grace_aggregation == "Seasonal") season else year,
      y = ~value,
      type = "scatter",
      mode = "lines+markers"
    ) %>%
      layout(
        title = paste("Time Series -", data$variable),
        xaxis = list(title = if(input$grace_aggregation == "Monthly") "Date" else if(input$grace_aggregation == "Seasonal") "Season" else "Year"),
        yaxis = list(title = data$variable),
        showlegend = FALSE,
        plot_bgcolor = 'white',
        paper_bgcolor = 'white'
      )
  }, error = function(e) {
    showNotification(paste("Error creating time series plot:", e$message), type = "error")
    return(NULL)
  })
})

output$grace_seasonal <- renderPlotly({
  data <- grace_data()
  if (is.null(data)) return(NULL)
  
  tryCatch({
    # Calculate monthly means and standard deviations
    seasonal_data <- data.frame(
      date = data$dates,
      value = apply(data$data, 3, mean, na.rm = TRUE)
    ) %>%
      mutate(month = month(date, label = TRUE, abbr = FALSE)) %>%
      group_by(month) %>%
      summarise(
        mean_val = mean(value, na.rm = TRUE),
        sd_val = sd(value, na.rm = TRUE),
        n = n(),
        se = sd_val / sqrt(n),
        lower = mean_val - 1.96 * se,
        upper = mean_val + 1.96 * se
      ) %>%
      arrange(match(month, month.name))
    
    plot_ly() %>%
      add_trace(
        data = seasonal_data,
        x = ~month,
        y = ~mean_val,
        type = "scatter",
        mode = "lines+markers",
        name = "Mean",
        line = list(color = "blue"),
        marker = list(color = "blue")
      ) %>%
      add_ribbons(
        data = seasonal_data,
        x = ~month,
        ymin = ~lower,
        ymax = ~upper,
        name = "95% CI",
        line = list(color = "transparent"),
        fillcolor = "rgba(0, 0, 255, 0.2)"
      ) %>%
      layout(
        title = paste("Seasonal Patterns -", data$variable),
        xaxis = list(
          title = "Month",
          categoryorder = "array",
          categoryarray = month.name
        ),
        yaxis = list(title = data$variable),
        plot_bgcolor = 'white',
        paper_bgcolor = 'white'
      )
  }, error = function(e) {
    showNotification(paste("Error creating seasonal plot:", e$message), type = "error")
    return(NULL)
  })
})

output$grace_trend <- renderPlotly({
  data <- grace_data()
  if (is.null(data)) return(NULL)
  
  tryCatch({
    # Calculate annual means
    annual_data <- data.frame(
      date = data$dates,
      value = apply(data$data, 3, mean, na.rm = TRUE)
    ) %>%
      mutate(year = year(date)) %>%
      group_by(year) %>%
      summarise(
        mean_val = mean(value, na.rm = TRUE),
        sd_val = sd(value, na.rm = TRUE)
      )
    
    # Fit linear trend
    trend <- lm(mean_val ~ year, data = annual_data)
    trend_line <- predict(trend, newdata = data.frame(year = annual_data$year))
    
    plot_ly() %>%
      add_trace(
        data = annual_data,
        x = ~year,
        y = ~mean_val,
        type = "scatter",
        mode = "markers",
        name = "Annual Mean",
        marker = list(color = "blue")
      ) %>%
      add_trace(
        x = annual_data$year,
        y = trend_line,
        type = "scatter",
        mode = "lines",
        name = "Trend Line",
        line = list(color = "red", dash = "dash")
      ) %>%
      add_ribbons(
        data = annual_data,
        x = ~year,
        ymin = ~mean_val - sd_val,
        ymax = ~mean_val + sd_val,
        name = "Standard Deviation",
        line = list(color = "transparent"),
        fillcolor = "rgba(0, 0, 255, 0.2)"
      ) %>%
      layout(
        title = paste("Trend Analysis -", data$variable),
        xaxis = list(title = "Year"),
        yaxis = list(title = data$variable),
        plot_bgcolor = 'white',
        paper_bgcolor = 'white'
      )
  }, error = function(e) {
    showNotification(paste("Error creating trend plot:", e$message), type = "error")
    return(NULL)
  })
})

# Additional output functions for PRISM data
output$prism_timeseries <- renderPlotly({
  data <- prism_data()
  if (is.null(data)) return(NULL)
  
  tryCatch({
    # Calculate spatial mean for each time step
    time_series <- apply(data$data, 3, mean, na.rm = TRUE)
    
    # Create a data frame for plotting
    plot_data <- data.frame(
      date = data$dates,
      value = time_series
    )
    
    # Aggregate based on selection
    if (input$prism_aggregation == "Monthly") {
      plot_data <- plot_data %>%
        mutate(date = floor_date(date, "month")) %>%
        group_by(date) %>%
        summarise(value = mean(value, na.rm = TRUE))
    } else if (input$prism_aggregation == "Seasonal") {
      plot_data <- plot_data %>%
        mutate(season = paste(year(date), quarter(date))) %>%
        group_by(season) %>%
        summarise(value = mean(value, na.rm = TRUE))
    } else { # Annual
      plot_data <- plot_data %>%
        mutate(year = year(date)) %>%
        group_by(year) %>%
        summarise(value = mean(value, na.rm = TRUE))
    }
    
    plot_ly(
      data = plot_data,
      x = ~if(input$prism_aggregation == "Monthly") date else if(input$prism_aggregation == "Seasonal") season else year,
      y = ~value,
      type = "scatter",
      mode = "lines+markers"
    ) %>%
      layout(
        title = paste("Time Series -", data$variable),
        xaxis = list(title = if(input$prism_aggregation == "Monthly") "Date" else if(input$prism_aggregation == "Seasonal") "Season" else "Year"),
        yaxis = list(title = paste(data$variable, if(data$variable == "Precipitation") "(mm)" else "(°C)")),
        showlegend = FALSE,
        plot_bgcolor = 'white',
        paper_bgcolor = 'white'
      )
  }, error = function(e) {
    showNotification(paste("Error creating time series plot:", e$message), type = "error")
    return(NULL)
  })
})

output$prism_seasonal <- renderPlotly({
  data <- prism_data()
  if (is.null(data)) return(NULL)
  
  tryCatch({
    # Calculate monthly means and standard deviations
    seasonal_data <- data.frame(
      date = data$dates,
      value = apply(data$data, 3, mean, na.rm = TRUE)
    ) %>%
      mutate(month = month(date, label = TRUE, abbr = FALSE)) %>%
      group_by(month) %>%
      summarise(
        mean_val = mean(value, na.rm = TRUE),
        sd_val = sd(value, na.rm = TRUE),
        n = n(),
        se = sd_val / sqrt(n),
        lower = mean_val - 1.96 * se,
        upper = mean_val + 1.96 * se
      ) %>%
      arrange(match(month, month.name))
    
    plot_ly() %>%
      add_trace(
        data = seasonal_data,
        x = ~month,
        y = ~mean_val,
        type = "scatter",
        mode = "lines+markers",
        name = "Mean",
        line = list(color = "blue"),
        marker = list(color = "blue")
      ) %>%
      add_ribbons(
        data = seasonal_data,
        x = ~month,
        ymin = ~lower,
        ymax = ~upper,
        name = "95% CI",
        line = list(color = "transparent"),
        fillcolor = "rgba(0, 0, 255, 0.2)"
      ) %>%
      layout(
        title = paste("Seasonal Patterns -", data$variable),
        xaxis = list(
          title = "Month",
          categoryorder = "array",
          categoryarray = month.name
        ),
        yaxis = list(title = paste(data$variable, if(data$variable == "Precipitation") "(mm)" else "(°C)")),
        plot_bgcolor = 'white',
        paper_bgcolor = 'white'
      )
  }, error = function(e) {
    showNotification(paste("Error creating seasonal plot:", e$message), type = "error")
    return(NULL)
  })
})

output$prism_trend <- renderPlotly({
  data <- prism_data()
  if (is.null(data)) return(NULL)
  
  tryCatch({
    # Calculate annual means
    annual_data <- data.frame(
      date = data$dates,
      value = apply(data$data, 3, mean, na.rm = TRUE)
    ) %>%
      mutate(year = year(date)) %>%
      group_by(year) %>%
      summarise(
        mean_val = mean(value, na.rm = TRUE),
        sd_val = sd(value, na.rm = TRUE)
      )
    
    # Fit linear trend
    trend <- lm(mean_val ~ year, data = annual_data)
    trend_line <- predict(trend, newdata = data.frame(year = annual_data$year))
    
    plot_ly() %>%
      add_trace(
        data = annual_data,
        x = ~year,
        y = ~mean_val,
        type = "scatter",
        mode = "markers",
        name = "Annual Mean",
        marker = list(color = "blue")
      ) %>%
      add_trace(
        x = annual_data$year,
        y = trend_line,
        type = "scatter",
        mode = "lines",
        name = "Trend Line",
        line = list(color = "red", dash = "dash")
      ) %>%
      add_ribbons(
        data = annual_data,
        x = ~year,
        ymin = ~mean_val - sd_val,
        ymax = ~mean_val + sd_val,
        name = "Standard Deviation",
        line = list(color = "transparent"),
        fillcolor = "rgba(0, 0, 255, 0.2)"
      ) %>%
      layout(
        title = paste("Trend Analysis -", data$variable),
        xaxis = list(title = "Year"),
        yaxis = list(title = paste(data$variable, if(data$variable == "Precipitation") "(mm)" else "(°C)")),
        plot_bgcolor = 'white',
        paper_bgcolor = 'white'
      )
  }, error = function(e) {
    showNotification(paste("Error creating trend plot:", e$message), type = "error")
    return(NULL)
  })
}) 