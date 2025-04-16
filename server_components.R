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
  analysis_server(input, output, session, values)
} 