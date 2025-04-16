# Load required packages
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(sf)
library(raster)
library(ncdf4)
library(viridis)
library(gridExtra)
library(reshape2)

# Create output directory if it doesn't exist
if (!dir.exists("images/analysis")) {
  dir.create("images/analysis", recursive = TRUE)
}

# Function to create trend plot
create_trend_plot <- function(data, title, y_label, color, add_seasonal = FALSE) {
  p <- ggplot(data, aes(x = Date, y = Value)) +
    geom_line(linewidth = 1, color = color) +
    geom_smooth(method = "lm", se = TRUE, color = "red", linetype = "dashed") +
    theme_minimal() +
    labs(title = title,
         x = "Date",
         y = y_label) +
    theme(plot.title = element_text(hjust = 0.5, size = 16),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10))
  
  if (add_seasonal) {
    # Add seasonal decomposition
    ts_data <- ts(data$Value, frequency = 12)
    if (length(ts_data) >= 24) {  # Need at least 2 years of data for decomposition
      decomp <- stl(ts_data, s.window = "periodic")
      seasonal <- as.vector(decomp$time.series[,"seasonal"])
      trend <- as.vector(decomp$time.series[,"trend"])
      
      data$Seasonal <- seasonal
      data$Trend <- trend
      
      p <- p +
        geom_line(data = data, aes(y = Trend), color = "blue", linetype = "solid", linewidth = 1) +
        geom_line(data = data, aes(y = Seasonal + mean(Value)), color = "green", linetype = "dotted", linewidth = 1)
    }
  }
  
  return(p)
}

# Load VIC data
vic_files <- list.files("data/vic_processed", pattern = "vic_.*\\.rds", full.names = TRUE)
vic_data <- data.frame()

for (file in vic_files) {
  tryCatch({
    year_data <- readRDS(file)
    if (is.data.frame(year_data)) {
      if (nrow(vic_data) == 0) {
        vic_data <- year_data
      } else {
        vic_data <- rbind(vic_data, year_data)
      }
    }
  }, error = function(e) {
    cat("Error reading file:", file, "\n")
    cat("Error message:", e$message, "\n")
  })
}

# Process VIC data
if (nrow(vic_data) > 0) {
  vic_df <- vic_data %>%
    arrange(date) %>%
    group_by(date) %>%
    summarise(Value = mean(soil_moisture, na.rm = TRUE)) %>%
    rename(Date = date)
  
  # Create VIC trend plot
  p_vic <- create_trend_plot(vic_df,
                            "VIC Model Soil Moisture Historical Trend - Colorado River Basin",
                            "Soil Moisture",
                            "#8C1D40",
                            add_seasonal = TRUE)
  ggsave("images/analysis/vic_historical_trend.png", p_vic, width = 12, height = 8, dpi = 300)
} else {
  cat("No VIC data available for processing\n")
}

# Load SMAP data
tryCatch({
  smap_data <- readRDS("data/smap_processed/smap.rds")
  
  # Process SMAP surface data
  smap_surface_df <- data.frame(
    Date = smap_data$dates[1:length(smap_data$surface_means)],
    Value = smap_data$surface_means
  )
  
  # Create SMAP surface trend plot
  p_smap_surface <- create_trend_plot(smap_surface_df,
                                     "SMAP Surface Soil Moisture Historical Trend - Colorado River Basin",
                                     "Surface Soil Moisture (m³/m³)",
                                     "#FFB81C",
                                     add_seasonal = TRUE)
  ggsave("images/analysis/smap_surface_historical_trend.png", p_smap_surface, width = 12, height = 8, dpi = 300)
  
  # Process SMAP rootzone data
  smap_rootzone_df <- data.frame(
    Date = smap_data$dates[1:length(smap_data$rootzone_means)],
    Value = smap_data$rootzone_means
  )
  
  # Create SMAP rootzone trend plot
  p_smap_rootzone <- create_trend_plot(smap_rootzone_df,
                                      "SMAP Root Zone Soil Moisture Historical Trend - Colorado River Basin",
                                      "Root Zone Soil Moisture (m³/m³)",
                                      "#00A3E0",
                                      add_seasonal = TRUE)
  ggsave("images/analysis/smap_rootzone_historical_trend.png", p_smap_rootzone, width = 12, height = 8, dpi = 300)
}, error = function(e) {
  cat("Error processing SMAP data:", e$message, "\n")
})

# Load GRACE data
tryCatch({
  grace_data <- readRDS("data/grace_processed/grace.rds")
  
  # Calculate spatial mean for each time step
  grace_means <- apply(grace_data$tws, c(3), mean, na.rm = TRUE)
  
  # Process GRACE data
  grace_df <- data.frame(
    Date = grace_data$dates,
    Value = grace_means
  )
  
  # Create GRACE trend plot
  p_grace <- create_trend_plot(grace_df,
                              "GRACE TWS Historical Trend - Colorado River Basin",
                              "Terrestrial Water Storage (cm)",
                              "#78BE20",
                              add_seasonal = TRUE)
  ggsave("images/analysis/grace_historical_trend.png", p_grace, width = 12, height = 8, dpi = 300)
}, error = function(e) {
  cat("Error processing GRACE data:", e$message, "\n")
})

# Create combined normalized plot if we have at least two datasets
datasets <- list()
if (exists("vic_df")) datasets$vic <- vic_df %>% mutate(Variable = "VIC Soil Moisture")
if (exists("smap_surface_df")) datasets$smap_surface <- smap_surface_df %>% mutate(Variable = "SMAP Surface")
if (exists("smap_rootzone_df")) datasets$smap_rootzone <- smap_rootzone_df %>% mutate(Variable = "SMAP Root Zone")
if (exists("grace_df")) datasets$grace <- grace_df %>% mutate(Variable = "GRACE TWS")

if (length(datasets) >= 2) {
  # Combine available datasets
  combined_df <- bind_rows(datasets)
  
  # Normalize the values for better comparison
  combined_df <- combined_df %>%
    group_by(Variable) %>%
    mutate(Value = (Value - mean(Value)) / sd(Value)) %>%
    ungroup()
  
  # Create combined plot
  p_combined <- ggplot(combined_df, aes(x = Date, y = Value, color = Variable)) +
    geom_line(linewidth = 1) +
    geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
    theme_minimal() +
    labs(title = "Historical Trends Analysis - Colorado River Basin",
         subtitle = "Normalized Values (Z-scores)",
         x = "Date",
         y = "Normalized Value (z-score)") +
    theme(plot.title = element_text(hjust = 0.5, size = 16),
          plot.subtitle = element_text(hjust = 0.5, size = 12),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10),
          legend.position = "bottom",
          legend.title = element_blank()) +
    scale_color_manual(values = c("#8C1D40", "#FFB81C", "#00A3E0", "#78BE20"))
  
  ggsave("images/analysis/combined_historical_trends.png", p_combined, width = 12, height = 8, dpi = 300)
  
  # Create correlation matrix
  tryCatch({
    # Prepare data for correlation
    cor_data <- combined_df %>%
      select(Date, Value, Variable) %>%
      tidyr::pivot_wider(names_from = Variable, values_from = Value)
    
    # Remove Date column and calculate correlation
    cor_matrix <- cor(select(cor_data, -Date), use = "pairwise.complete.obs")
    
    # Create correlation plot
    p_cor <- ggplot(data = melt(cor_matrix)) +
      geom_tile(aes(x = Var1, y = Var2, fill = value)) +
      scale_fill_viridis() +
      theme_minimal() +
      labs(title = "Correlation Matrix of Water Variables",
           x = "",
           y = "",
           fill = "Correlation") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            plot.title = element_text(hjust = 0.5))
    
    ggsave("images/analysis/correlation_matrix.png", p_cor, width = 10, height = 8, dpi = 300)
  }, error = function(e) {
    cat("Error creating correlation matrix:", e$message, "\n")
  })
  
  # Calculate and save summary statistics
  tryCatch({
    summary_stats <- combined_df %>%
      group_by(Variable) %>%
      summarise(
        Mean = mean(Value, na.rm = TRUE),
        SD = sd(Value, na.rm = TRUE),
        Min = min(Value, na.rm = TRUE),
        Max = max(Value, na.rm = TRUE),
        Trend = coef(lm(Value ~ as.numeric(Date)))[2]
      )
    
    write.csv(summary_stats, "images/analysis/summary_statistics.csv", row.names = FALSE)
    
    # Print completion message and summary
    cat("All historical trend analysis images have been generated and saved to the 'images/analysis' directory.\n")
    cat("\nSummary of findings:\n")
    print(summary_stats)
  }, error = function(e) {
    cat("Error calculating summary statistics:", e$message, "\n")
  })
} else {
  cat("Not enough datasets available for combined analysis\n")
} 