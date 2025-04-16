# Load required packages
library(ggplot2)
library(gridExtra)

# Create directory if it doesn't exist
dir.create("images/dashboard_new", recursive = TRUE, showWarnings = FALSE)

# Function to create a placeholder plot
create_placeholder_plot <- function(title, type) {
  if (type == "spatial") {
    # Create a simple spatial plot
    df <- expand.grid(x = 1:10, y = 1:10)
    df$value <- rnorm(100)
    
    p <- ggplot(df, aes(x, y, fill = value)) +
      geom_tile() +
      scale_fill_gradient2(low = "blue", mid = "white", high = "red") +
      theme_minimal() +
      labs(title = paste("Spatial Distribution -", title),
           x = "Longitude", y = "Latitude") +
      theme(plot.title = element_text(hjust = 0.5))
    
  } else if (type == "timeseries") {
    # Create a simple time series plot
    df <- data.frame(
      date = seq(as.Date("2020-01-01"), as.Date("2020-12-31"), by = "day"),
      value = cumsum(rnorm(366))
    )
    
    p <- ggplot(df, aes(date, value)) +
      geom_line(color = "blue") +
      theme_minimal() +
      labs(title = paste("Time Series -", title),
           x = "Date", y = "Value") +
      theme(plot.title = element_text(hjust = 0.5))
    
  } else if (type == "monthly") {
    # Create a simple monthly statistics plot
    df <- data.frame(
      month = month.abb,
      value = rnorm(12, mean = 50, sd = 10)
    )
    
    p <- ggplot(df, aes(month, value)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      theme_minimal() +
      labs(title = paste("Monthly Statistics -", title),
           x = "Month", y = "Value") +
      theme(plot.title = element_text(hjust = 0.5))
  }
  
  return(p)
}

# Variables to create placeholders for
variables <- c("prec", "evap", "runoff", "soil_moisture", "swe", "temp")
types <- c("spatial", "timeseries", "monthly")

# Generate and save plots
for (var in variables) {
  for (type in types) {
    # Create the plot
    p <- create_placeholder_plot(toupper(var), type)
    
    # Save the plot
    filename <- paste0("images/dashboard_new/", type, "_", var, ".png")
    ggsave(filename, p, width = 8, height = 6, dpi = 300)
  }
}

print("Placeholder images created successfully!") 