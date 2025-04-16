# Load required packages
library(dplyr)
library(ggplot2)
library(lubridate)

# Read SNOTEL data
snotel_data <- read.csv("data/snotel/snotel_CAP.csv")

# Process the data
snotel_processed <- snotel_data %>%
  mutate(date = as.Date(date)) %>%
  group_by(date) %>%
  summarise(Value = mean(WTEQ, na.rm = TRUE)) %>%
  arrange(date)

# Create the trend plot
p <- ggplot(snotel_processed, aes(x = date, y = Value)) +
  geom_line(color = '#78BE20', size = 1) +
  theme_minimal() +
  labs(
    title = "SNOTEL SWE Trend - Colorado River Basin",
    x = "Date",
    y = "Snow Water Equivalent (inches)"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white")
  )

# Save the plot
ggsave("images/snotel_trend.png", p, width = 10, height = 6, dpi = 300) 