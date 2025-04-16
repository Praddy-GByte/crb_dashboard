# Server Components

# Function to create plotly plots for a variable
create_variable_plots <- function(variable, data) {
  if (is.null(data)) return(NULL)
  
  # Create spatial map
  spatial_plot <- plot_ly(data, 
                         type = "scattermapbox",
                         mode = "markers",
                         lat = ~lat,
                         lon = ~lon,
                         text = ~paste("Value:", value),
                         marker = list(size = 10, color = ~value, colorscale = "Viridis"),
                         showlegend = FALSE) %>%
    layout(mapbox = list(style = "carto-positron",
                        zoom = 5,
                        center = list(lon = mean(data$lon), lat = mean(data$lat))))
  
  # Create time series
  timeseries_plot <- plot_ly(data, 
                            x = ~date,
                            y = ~value,
                            type = "scatter",
                            mode = "lines",
                            name = variable) %>%
    layout(title = paste(variable, "Time Series"),
           xaxis = list(title = "Date"),
           yaxis = list(title = "Value"))
  
  # Create monthly statistics
  monthly_stats <- data %>%
    group_by(month = month(date)) %>%
    summarise(mean_value = mean(value, na.rm = TRUE),
              sd_value = sd(value, na.rm = TRUE))
  
  monthly_plot <- plot_ly(monthly_stats,
                         x = ~month,
                         y = ~mean_value,
                         type = "bar",
                         name = "Mean") %>%
    add_trace(y = ~sd_value, name = "Standard Deviation") %>%
    layout(title = paste(variable, "Monthly Statistics"),
           xaxis = list(title = "Month"),
           yaxis = list(title = "Value"),
           barmode = "group")
  
  return(list(spatial = spatial_plot,
              timeseries = timeseries_plot,
              monthly = monthly_plot))
}

# PRISM Data server logic
prism_server <- function(input, output, session) {
  # Load PRISM data
  prism_data <- reactive({
    tryCatch({
      nc <- nc_open("data/CRB_PRISM_Calibrated.2024-01-01.nc")
      on.exit(nc_close(nc))
      
      # Get dimensions
      lon <- ncvar_get(nc, "lon")
      lat <- ncvar_get(nc, "lat")
      time <- ncvar_get(nc, "time")
      dates <- as.Date(time, origin = "0001-01-01")
      
      # Get precipitation data
      precip <- ncvar_get(nc, "OUT_PREC")
      
      # Calculate spatial mean
      spatial_mean <- apply(precip, c(1,2), mean, na.rm=TRUE)
      
      # Calculate time series
      time_series <- apply(precip, 3, mean, na.rm=TRUE)
      
      # Create data frames
      spatial_data <- expand.grid(lon = lon, lat = lat)
      spatial_data$value <- as.vector(spatial_mean)
      
      ts_data <- data.frame(
        date = dates,
        value = time_series
      )
      
      return(list(
        spatial = spatial_data,
        timeseries = ts_data
      ))
    }, error = function(e) {
      showNotification(paste("Error loading PRISM data:", e$message), type = "error")
      return(NULL)
    })
  })
  
  # Render spatial distribution plot
  output$prism_spatial <- renderPlotly({
    data <- prism_data()
    if (!is.null(data)) {
      plot_ly(data$spatial, 
              type = "scattermapbox",
              mode = "markers",
              lat = ~lat,
              lon = ~lon,
              text = ~paste("Precipitation:", round(value, 2), "mm"),
              marker = list(size = 10, color = ~value, colorscale = "Viridis"),
              showlegend = FALSE) %>%
        layout(mapbox = list(style = "carto-positron",
                            zoom = 5,
                            center = list(lon = mean(data$spatial$lon), 
                                        lat = mean(data$spatial$lat))))
    }
  })
  
  # Render time series plot
  output$prism_timeseries <- renderPlotly({
    data <- prism_data()
    if (!is.null(data)) {
      plot_ly(data$timeseries, 
              x = ~date,
              y = ~value,
              type = "scatter",
              mode = "lines",
              name = "Precipitation") %>%
        layout(title = "PRISM Precipitation Time Series",
               xaxis = list(title = "Date"),
               yaxis = list(title = "Precipitation (mm)"))
    }
  })
  
  # Render monthly statistics
  output$prism_monthly <- renderPlotly({
    data <- prism_data()
    if (!is.null(data)) {
      monthly_stats <- data$timeseries %>%
        mutate(month = month(date, label = TRUE)) %>%
        group_by(month) %>%
        summarise(mean_value = mean(value, na.rm = TRUE),
                  sd_value = sd(value, na.rm = TRUE))
      
      plot_ly(monthly_stats,
              x = ~month,
              y = ~mean_value,
              type = "bar",
              name = "Mean") %>%
        add_trace(y = ~sd_value, name = "Standard Deviation") %>%
        layout(title = "Monthly Precipitation Statistics",
               xaxis = list(title = "Month"),
               yaxis = list(title = "Precipitation (mm)"),
               barmode = "group")
    }
  })
  
  # Render value boxes
  output$prism_mean <- renderValueBox({
    data <- prism_data()
    if (!is.null(data)) {
      mean_value <- mean(data$timeseries$value, na.rm = TRUE)
      valueBox(
        round(mean_value, 2),
        "Mean Precipitation (mm)",
        icon = icon("cloud-rain"),
        color = "blue"
      )
    }
  })
  
  output$prism_max <- renderValueBox({
    data <- prism_data()
    if (!is.null(data)) {
      max_value <- max(data$timeseries$value, na.rm = TRUE)
      valueBox(
        round(max_value, 2),
        "Maximum Precipitation (mm)",
        icon = icon("cloud-showers-heavy"),
        color = "red"
      )
    }
  })
  
  output$prism_min <- renderValueBox({
    data <- prism_data()
    if (!is.null(data)) {
      min_value <- min(data$timeseries$value, na.rm = TRUE)
      valueBox(
        round(min_value, 2),
        "Minimum Precipitation (mm)",
        icon = icon("cloud"),
        color = "green"
      )
    }
  })
  
  output$prism_std <- renderValueBox({
    data <- prism_data()
    if (!is.null(data)) {
      std_value <- sd(data$timeseries$value, na.rm = TRUE)
      valueBox(
        round(std_value, 2),
        "Standard Deviation (mm)",
        icon = icon("chart-line"),
        color = "yellow"
      )
    }
  })
}

# VIC Data server logic
vic_server <- function(input, output, session, values) {
  # Load VIC data
  observeEvent(input$load_vic, {
    showModal(modalDialog("Loading VIC data...", footer = NULL))
    tryCatch({
      values$vic_data <- load_processed_data("vic", input$vic_year, session)
      removeModal()
      
      # Create and render plots for each variable
      if (!is.null(values$vic_data)) {
        # Precipitation plots
        prec_plots <- create_variable_plots("Precipitation", values$vic_data$precipitation)
        output$prec_spatial <- renderPlotly({ prec_plots$spatial })
        output$prec_timeseries <- renderPlotly({ prec_plots$timeseries })
        output$prec_monthly <- renderPlotly({ prec_plots$monthly })
        
        # Evapotranspiration plots
        evap_plots <- create_variable_plots("Evapotranspiration", values$vic_data$evapotranspiration)
        output$evap_spatial <- renderPlotly({ evap_plots$spatial })
        output$evap_timeseries <- renderPlotly({ evap_plots$timeseries })
        output$evap_monthly <- renderPlotly({ evap_plots$monthly })
      }
    }, error = function(e) {
      removeModal()
      showNotification(paste("Error loading VIC data:", e$message), type = "error")
    })
  })
}

# SNOTEL Data server logic
snotel_server <- function(input, output, session, values) {
  # Load SNOTEL data
  observeEvent(input$load_snotel, {
    showModal(modalDialog("Loading SNOTEL data...", footer = NULL))
    tryCatch({
      values$snotel_data <- load_processed_data("snotel", input$snotel_year, session)
      removeModal()
      
      # Create and render plots for each variable
      if (!is.null(values$snotel_data)) {
        # Snow Water Equivalent plots
        swe_plots <- create_variable_plots("Snow Water Equivalent", values$snotel_data$swe)
        output$swe_spatial <- renderPlotly({ swe_plots$spatial })
        output$swe_timeseries <- renderPlotly({ swe_plots$timeseries })
        output$swe_monthly <- renderPlotly({ swe_plots$monthly })
        
        # Precipitation plots
        prec_plots <- create_variable_plots("Precipitation", values$snotel_data$precipitation)
        output$snotel_prec_spatial <- renderPlotly({ prec_plots$spatial })
        output$snotel_prec_timeseries <- renderPlotly({ prec_plots$timeseries })
        output$snotel_prec_monthly <- renderPlotly({ prec_plots$monthly })
      }
    }, error = function(e) {
      removeModal()
      showNotification(paste("Error loading SNOTEL data:", e$message), type = "error")
    })
  })
}

# Analysis server logic
analysis_server <- function(input, output, session, values) {
  observeEvent(input$run_analysis, {
    showModal(modalDialog("Running analysis...", footer = NULL))
    tryCatch({
      # Get the selected analysis type
      analysis_type <- input$analysis_type
      
      # Create analysis plot based on selected type
      analysis_plot <- switch(analysis_type,
                            "Trend Analysis" = create_trend_plot(values$vic_data, values$snotel_data),
                            "Correlation Analysis" = create_correlation_plot(values$vic_data, values$snotel_data),
                            "Anomaly Analysis" = create_anomaly_plot(values$vic_data, values$snotel_data))
      
      # Render the analysis plot
      output$analysis_plot <- renderPlotly({ analysis_plot })
      
      removeModal()
    }, error = function(e) {
      removeModal()
      showNotification(paste("Error running analysis:", e$message), type = "error")
    })
  })
}

# Helper functions for analysis plots
create_trend_plot <- function(vic_data, snotel_data) {
  if (is.null(vic_data) || is.null(snotel_data)) return(NULL)
  
  # Combine VIC and SNOTEL data
  combined_data <- bind_rows(
    vic_data$precipitation %>% mutate(source = "VIC"),
    snotel_data$precipitation %>% mutate(source = "SNOTEL")
  )
  
  # Calculate trends
  trends <- combined_data %>%
    group_by(source) %>%
    summarise(
      slope = coef(lm(value ~ as.numeric(date)))[2],
      intercept = coef(lm(value ~ as.numeric(date)))[1]
    )
  
  # Create plot
  plot_ly(combined_data, x = ~date, y = ~value, color = ~source,
          type = "scatter", mode = "lines+markers") %>%
    add_trace(data = trends,
              x = ~date,
              y = ~slope * as.numeric(date) + intercept,
              type = "scatter",
              mode = "lines",
              name = ~paste(source, "Trend"),
              showlegend = FALSE) %>%
    layout(title = "Precipitation Trends",
           xaxis = list(title = "Date"),
           yaxis = list(title = "Value"))
}

create_correlation_plot <- function(vic_data, snotel_data) {
  if (is.null(vic_data) || is.null(snotel_data)) return(NULL)
  
  # Combine VIC and SNOTEL data
  combined_data <- inner_join(
    vic_data$precipitation %>% select(date, vic_value = value),
    snotel_data$precipitation %>% select(date, snotel_value = value),
    by = "date"
  )
  
  # Create correlation plot
  plot_ly(combined_data,
          x = ~vic_value,
          y = ~snotel_value,
          type = "scatter",
          mode = "markers",
          text = ~paste("Date:", date)) %>%
    add_trace(x = ~vic_value,
              y = ~fitted(lm(snotel_value ~ vic_value)),
              type = "scatter",
              mode = "lines",
              name = "Regression Line") %>%
    layout(title = "VIC vs SNOTEL Precipitation Correlation",
           xaxis = list(title = "VIC Precipitation"),
           yaxis = list(title = "SNOTEL Precipitation"))
}

create_anomaly_plot <- function(vic_data, snotel_data) {
  if (is.null(vic_data) || is.null(snotel_data)) return(NULL)
  
  # Calculate anomalies
  anomalies <- bind_rows(
    vic_data$precipitation %>%
      group_by(month = month(date)) %>%
      mutate(
        monthly_mean = mean(value, na.rm = TRUE),
        monthly_sd = sd(value, na.rm = TRUE),
        anomaly = (value - monthly_mean) / monthly_sd
      ) %>%
      mutate(source = "VIC"),
    snotel_data$precipitation %>%
      group_by(month = month(date)) %>%
      mutate(
        monthly_mean = mean(value, na.rm = TRUE),
        monthly_sd = sd(value, na.rm = TRUE),
        anomaly = (value - monthly_mean) / monthly_sd
      ) %>%
      mutate(source = "SNOTEL")
  )
  
  # Create anomaly plot
  plot_ly(anomalies,
          x = ~date,
          y = ~anomaly,
          color = ~source,
          type = "scatter",
          mode = "lines+markers") %>%
    add_hline(y = 0, line = list(dash = "dash")) %>%
    layout(title = "Precipitation Anomalies",
           xaxis = list(title = "Date"),
           yaxis = list(title = "Standardized Anomaly"))
}

# Main server logic
dashboard_server <- function(input, output, session) {
  # Reactive values
  values <- reactiveValues(
    vic_data = NULL,
    snotel_data = NULL
  )
  
  # Initialize server components
  vic_server(input, output, session, values)
  snotel_server(input, output, session, values)
  prism_server(input, output, session)
  analysis_server(input, output, session, values)
}

# VIC Model Server Components
output$precipitation_spatial <- renderImage({
  list(src = "images/dashboard_new/spatial_prec.png",
       contentType = "image/png",
       width = "100%",
       height = "400px")
}, deleteFile = FALSE)

output$precipitation_timeseries <- renderImage({
  list(src = "images/dashboard_new/timeseries_prec.png",
       contentType = "image/png",
       width = "100%",
       height = "400px")
}, deleteFile = FALSE)

output$precipitation_monthly <- renderImage({
  list(src = "images/dashboard_new/monthly_prec.png",
       contentType = "image/png",
       width = "100%",
       height = "400px")
}, deleteFile = FALSE)

output$swe_spatial <- renderImage({
  list(src = "images/dashboard_new/spatial_swe.png",
       contentType = "image/png",
       width = "100%",
       height = "400px")
}, deleteFile = FALSE)

output$swe_timeseries <- renderImage({
  list(src = "images/dashboard_new/timeseries_swe.png",
       contentType = "image/png",
       width = "100%",
       height = "400px")
}, deleteFile = FALSE)

output$swe_monthly <- renderImage({
  list(src = "images/dashboard_new/monthly_swe.png",
       contentType = "image/png",
       width = "100%",
       height = "400px")
}, deleteFile = FALSE)

output$runoff_spatial <- renderImage({
  list(src = "images/dashboard_new/spatial_runoff.png",
       contentType = "image/png",
       width = "100%",
       height = "400px")
}, deleteFile = FALSE)

output$runoff_timeseries <- renderImage({
  list(src = "images/dashboard_new/timeseries_runoff.png",
       contentType = "image/png",
       width = "100%",
       height = "400px")
}, deleteFile = FALSE)

output$runoff_monthly <- renderImage({
  list(src = "images/dashboard_new/monthly_runoff.png",
       contentType = "image/png",
       width = "100%",
       height = "400px")
}, deleteFile = FALSE)

output$baseflow_spatial <- renderImage({
  list(src = "images/dashboard_new/spatial_baseflow.png",
       contentType = "image/png",
       width = "100%",
       height = "400px")
}, deleteFile = FALSE)

output$baseflow_timeseries <- renderImage({
  list(src = "images/dashboard_new/timeseries_baseflow.png",
       contentType = "image/png",
       width = "100%",
       height = "400px")
}, deleteFile = FALSE)

output$baseflow_monthly <- renderImage({
  list(src = "images/dashboard_new/monthly_baseflow.png",
       contentType = "image/png",
       width = "100%",
       height = "400px")
}, deleteFile = FALSE)

output$evap_spatial <- renderImage({
  list(src = "images/dashboard_new/spatial_evap.png",
       contentType = "image/png",
       width = "100%",
       height = "400px")
}, deleteFile = FALSE)

output$evap_timeseries <- renderImage({
  list(src = "images/dashboard_new/timeseries_evap.png",
       contentType = "image/png",
       width = "100%",
       height = "400px")
}, deleteFile = FALSE)

output$evap_monthly <- renderImage({
  list(src = "images/dashboard_new/monthly_evap.png",
       contentType = "image/png",
       width = "100%",
       height = "400px")
}, deleteFile = FALSE)

output$soil_moisture_spatial <- renderImage({
  list(src = "images/dashboard_new/spatial_soil_moisture.png",
       contentType = "image/png",
       width = "100%",
       height = "400px")
}, deleteFile = FALSE)

output$soil_moisture_timeseries <- renderImage({
  list(src = "images/dashboard_new/timeseries_soil_moisture.png",
       contentType = "image/png",
       width = "100%",
       height = "400px")
}, deleteFile = FALSE)

output$soil_moisture_monthly <- renderImage({
  list(src = "images/dashboard_new/monthly_soil_moisture.png",
       contentType = "image/png",
       width = "100%",
       height = "400px")
}, deleteFile = FALSE)

output$temp_spatial <- renderImage({
  list(src = "images/dashboard_new/spatial_temp.png",
       contentType = "image/png",
       width = "100%",
       height = "400px")
}, deleteFile = FALSE)

output$temp_timeseries <- renderImage({
  list(src = "images/dashboard_new/timeseries_temp.png",
       contentType = "image/png",
       width = "100%",
       height = "400px")
}, deleteFile = FALSE)

output$temp_monthly <- renderImage({
  list(src = "images/dashboard_new/monthly_temp.png",
       contentType = "image/png",
       width = "100%",
       height = "400px")
}, deleteFile = FALSE)

output$evap_canop_spatial <- renderImage({
  list(src = "images/dashboard_new/spatial_evap_canop.png",
       contentType = "image/png",
       width = "100%",
       height = "400px")
}, deleteFile = FALSE)

output$evap_canop_timeseries <- renderImage({
  list(src = "images/dashboard_new/timeseries_evap_canop.png",
       contentType = "image/png",
       width = "100%",
       height = "400px")
}, deleteFile = FALSE)

output$evap_canop_monthly <- renderImage({
  list(src = "images/dashboard_new/monthly_evap_canop.png",
       contentType = "image/png",
       width = "100%",
       height = "400px")
}, deleteFile = FALSE)

output$evap_bare_spatial <- renderImage({
  list(src = "images/dashboard_new/spatial_evap_bare.png",
       contentType = "image/png",
       width = "100%",
       height = "400px")
}, deleteFile = FALSE)

output$evap_bare_timeseries <- renderImage({
  list(src = "images/dashboard_new/timeseries_evap_bare.png",
       contentType = "image/png",
       width = "100%",
       height = "400px")
}, deleteFile = FALSE)

output$evap_bare_monthly <- renderImage({
  list(src = "images/dashboard_new/monthly_evap_bare.png",
       contentType = "image/png",
       width = "100%",
       height = "400px")
}, deleteFile = FALSE)

output$transp_veg_spatial <- renderImage({
  list(src = "images/dashboard_new/spatial_transp_veg.png",
       contentType = "image/png",
       width = "100%",
       height = "400px")
}, deleteFile = FALSE)

output$transp_veg_timeseries <- renderImage({
  list(src = "images/dashboard_new/timeseries_transp_veg.png",
       contentType = "image/png",
       width = "100%",
       height = "400px")
}, deleteFile = FALSE)

output$transp_veg_monthly <- renderImage({
  list(src = "images/dashboard_new/monthly_transp_veg.png",
       contentType = "image/png",
       width = "100%",
       height = "400px")
}, deleteFile = FALSE)

output$surfstor_spatial <- renderImage({
  list(src = "images/dashboard_new/spatial_surfstor.png",
       contentType = "image/png",
       width = "100%",
       height = "400px")
}, deleteFile = FALSE)

output$surfstor_timeseries <- renderImage({
  list(src = "images/dashboard_new/timeseries_surfstor.png",
       contentType = "image/png",
       width = "100%",
       height = "400px")
}, deleteFile = FALSE)

output$surfstor_monthly <- renderImage({
  list(src = "images/dashboard_new/monthly_surfstor.png",
       contentType = "image/png",
       width = "100%",
       height = "400px")
}, deleteFile = FALSE)

output$surf_temp_spatial <- renderImage({
  list(src = "images/dashboard_new/spatial_surf_temp.png",
       contentType = "image/png",
       width = "100%",
       height = "400px")
}, deleteFile = FALSE)

output$surf_temp_timeseries <- renderImage({
  list(src = "images/dashboard_new/timeseries_surf_temp.png",
       contentType = "image/png",
       width = "100%",
       height = "400px")
}, deleteFile = FALSE)

output$surf_temp_monthly <- renderImage({
  list(src = "images/dashboard_new/monthly_surf_temp.png",
       contentType = "image/png",
       width = "100%",
       height = "400px")
}, deleteFile = FALSE)

output$snow_pack_temp_spatial <- renderImage({
  list(src = "images/dashboard_new/spatial_snow_pack_temp.png",
       contentType = "image/png",
       width = "100%",
       height = "400px")
}, deleteFile = FALSE)

output$snow_pack_temp_timeseries <- renderImage({
  list(src = "images/dashboard_new/timeseries_snow_pack_temp.png",
       contentType = "image/png",
       width = "100%",
       height = "400px")
}, deleteFile = FALSE)

output$snow_pack_temp_monthly <- renderImage({
  list(src = "images/dashboard_new/monthly_snow_pack_temp.png",
       contentType = "image/png",
       width = "100%",
       height = "400px")
}, deleteFile = FALSE)

output$soil_temp_spatial <- renderImage({
  list(src = "images/dashboard_new/spatial_soil_temp.png",
       contentType = "image/png",
       width = "100%",
       height = "400px")
}, deleteFile = FALSE)

output$soil_temp_timeseries <- renderImage({
  list(src = "images/dashboard_new/timeseries_soil_temp.png",
       contentType = "image/png",
       width = "100%",
       height = "400px")
}, deleteFile = FALSE)

output$soil_temp_monthly <- renderImage({
  list(src = "images/dashboard_new/monthly_soil_temp.png",
       contentType = "image/png",
       width = "100%",
       height = "400px")
}, deleteFile = FALSE)

output$air_temp_spatial <- renderImage({
  list(src = "images/dashboard_new/spatial_air_temp.png",
       contentType = "image/png",
       width = "100%",
       height = "400px")
}, deleteFile = FALSE)

output$air_temp_timeseries <- renderImage({
  list(src = "images/dashboard_new/timeseries_air_temp.png",
       contentType = "image/png",
       width = "100%",
       height = "400px")
}, deleteFile = FALSE)

output$air_temp_monthly <- renderImage({
  list(src = "images/dashboard_new/monthly_air_temp.png",
       contentType = "image/png",
       width = "100%",
       height = "400px")
}, deleteFile = FALSE)

output$snowf_spatial <- renderImage({
  list(src = "images/dashboard_new/spatial_snowf.png",
       contentType = "image/png",
       width = "100%",
       height = "400px")
}, deleteFile = FALSE)

output$snowf_timeseries <- renderImage({
  list(src = "images/dashboard_new/timeseries_snowf.png",
       contentType = "image/png",
       width = "100%",
       height = "400px")
}, deleteFile = FALSE)

output$snowf_monthly <- renderImage({
  list(src = "images/dashboard_new/monthly_snowf.png",
       contentType = "image/png",
       width = "100%",
       height = "400px")
}, deleteFile = FALSE)

output$rainf_spatial <- renderImage({
  list(src = "images/dashboard_new/spatial_rainf.png",
       contentType = "image/png",
       width = "100%",
       height = "400px")
}, deleteFile = FALSE)

output$rainf_timeseries <- renderImage({
  list(src = "images/dashboard_new/timeseries_rainf.png",
       contentType = "image/png",
       width = "100%",
       height = "400px")
}, deleteFile = FALSE)

output$rainf_monthly <- renderImage({
  list(src = "images/dashboard_new/monthly_rainf.png",
       contentType = "image/png",
       width = "100%",
       height = "400px")
}, deleteFile = FALSE)

# Analysis Output server logic
analysis_output_server <- function(input, output, session) {
  # Combined Analysis Images
  output$combined_trends <- renderImage({
    list(src = "images/combined_trends.png",
         contentType = "image/png",
         width = "100%",
         height = "400px")
  }, deleteFile = FALSE)
  
  output$combined_trends_normalized <- renderImage({
    list(src = "images/combined_trends_normalized.png",
         contentType = "image/png",
         width = "100%",
         height = "400px")
  }, deleteFile = FALSE)
  
  output$vic_combined_timeseries <- renderImage({
    list(src = "images/vic_combined_timeseries.png",
         contentType = "image/png",
         width = "100%",
         height = "400px")
  }, deleteFile = FALSE)
  
  output$vic_combined_timeseries_normalized <- renderImage({
    list(src = "images/vic_combined_timeseries_normalized.png",
         contentType = "image/png",
         width = "100%",
         height = "400px")
  }, deleteFile = FALSE)
  
  output$monthly_statistics <- renderImage({
    list(src = "images/monthly_statistics.png",
         contentType = "image/png",
         width = "100%",
         height = "400px")
  }, deleteFile = FALSE)
  
  # Precipitation Analysis Images
  output$precipitation_spatial <- renderImage({
    list(src = "images/precipitation_spatial.png",
         contentType = "image/png",
         width = "100%",
         height = "400px")
  }, deleteFile = FALSE)
  
  output$precipitation_timeseries <- renderImage({
    list(src = "images/precipitation_timeseries.png",
         contentType = "image/png",
         width = "100%",
         height = "400px")
  }, deleteFile = FALSE)
  
  output$precipitation_rain_snow <- renderImage({
    list(src = "images/precipitation_rain_snow.png",
         contentType = "image/png",
         width = "100%",
         height = "400px")
  }, deleteFile = FALSE)
  
  output$precipitation_monthly <- renderImage({
    list(src = "images/precipitation_monthly.png",
         contentType = "image/png",
         width = "100%",
         height = "400px")
  }, deleteFile = FALSE)
  
  # Snow Water Equivalent Analysis Images
  output$april1_swe_trends <- renderImage({
    list(src = "images/april1_swe_trends.png",
         contentType = "image/png",
         width = "100%",
         height = "400px")
  }, deleteFile = FALSE)
  
  output$april1_swe_anomalies <- renderImage({
    list(src = "images/april1_swe_anomalies.png",
         contentType = "image/png",
         width = "100%",
         height = "400px")
  }, deleteFile = FALSE)
  
  output$swe_analysis <- renderImage({
    list(src = "images/swe_analysis.png",
         contentType = "image/png",
         width = "100%",
         height = "400px")
  }, deleteFile = FALSE)
  
  # Soil Moisture Analysis Images
  output$soil_moisture <- renderImage({
    list(src = "images/soil_moisture.png",
         contentType = "image/png",
         width = "100%",
         height = "400px")
  }, deleteFile = FALSE)
  
  output$smap_surface_trend <- renderImage({
    list(src = "images/smap_surface_trend.png",
         contentType = "image/png",
         width = "100%",
         height = "400px")
  }, deleteFile = FALSE)
  
  output$smap_rootzone_trend <- renderImage({
    list(src = "images/smap_rootzone_trend.png",
         contentType = "image/png",
         width = "100%",
         height = "400px")
  }, deleteFile = FALSE)
  
  # Water Storage Analysis Images
  output$water_storage <- renderImage({
    list(src = "images/water_storage.png",
         contentType = "image/png",
         width = "100%",
         height = "400px")
  }, deleteFile = FALSE)
  
  output$grace_trend <- renderImage({
    list(src = "images/grace_trend.png",
         contentType = "image/png",
         width = "100%",
         height = "400px")
  }, deleteFile = FALSE)
} 