# Load required packages
library(ncdf4)
library(raster)
library(ggplot2)
library(dplyr)
library(lubridate)
library(plotly)

# Read VIC output
nc <- nc_open("data/VICOut2.nc")

# Get dimensions
lon <- ncvar_get(nc, "lon")
lat <- ncvar_get(nc, "lat")
time <- ncvar_get(nc, "time")

# Convert time to dates
dates <- as.Date(time, origin = "0001-01-01")

# Extract precipitation variables
prec <- ncvar_get(nc, "OUT_PREC")
rain <- ncvar_get(nc, "OUT_RAINF")
snow <- ncvar_get(nc, "OUT_SNOWF")

# Close the NetCDF file
nc_close(nc)

# 1. Spatial Distribution
# Create spatial mean for the last time step
spatial_mean <- apply(prec[,,dim(prec)[3]], c(1,2), mean, na.rm = TRUE)

# Create raster
r <- raster(t(spatial_mean),
            xmn = min(lon), xmx = max(lon),
            ymn = min(lat), ymx = max(lat))

# Convert to data frame for plotting
df_spatial <- as.data.frame(r, xy = TRUE)
colnames(df_spatial) <- c("lon", "lat", "value")

# Create spatial plot
p_spatial <- ggplot(df_spatial, aes(x = lon, y = lat, fill = value)) +
  geom_raster() +
  scale_fill_viridis_c(name = "Precipitation (mm)") +
  labs(title = "Spatial Distribution of Precipitation",
       x = "Longitude", y = "Latitude") +
  theme_minimal()

ggsave("images/precipitation_analysis/spatial_distribution.png", p_spatial, width = 10, height = 8)

# 2. Time Series
# Calculate daily spatial mean
daily_mean <- apply(prec, 3, mean, na.rm = TRUE)

# Create time series data frame
df_ts <- data.frame(
  Date = dates,
  Precipitation = daily_mean
)

# Create time series plot
p_ts <- ggplot(df_ts, aes(x = Date, y = Precipitation)) +
  geom_line(color = "blue") +
  labs(title = "Precipitation Time Series",
       x = "Date",
       y = "Precipitation (mm)") +
  theme_minimal()

ggsave("images/precipitation_analysis/time_series.png", p_ts, width = 12, height = 6)

# 3. Monthly Statistics
# Calculate monthly statistics
monthly_stats <- df_ts %>%
  mutate(Month = month(Date, label = TRUE)) %>%
  group_by(Month) %>%
  summarise(
    Mean = mean(Precipitation, na.rm = TRUE),
    Min = min(Precipitation, na.rm = TRUE),
    Max = max(Precipitation, na.rm = TRUE)
  )

# Create monthly statistics plot
p_monthly <- ggplot(monthly_stats, aes(x = Month, y = Mean)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_errorbar(aes(ymin = Min, ymax = Max), width = 0.2) +
  labs(title = "Monthly Precipitation Statistics",
       x = "Month",
       y = "Precipitation (mm)") +
  theme_minimal()

ggsave("images/precipitation_analysis/monthly_statistics.png", p_monthly, width = 10, height = 6)

# 4. Rain vs Snow
# Calculate daily means for rain and snow
rain_mean <- apply(rain, 3, mean, na.rm = TRUE)
snow_mean <- apply(snow, 3, mean, na.rm = TRUE)

# Create rain vs snow data frame
df_rain_snow <- data.frame(
  Date = dates,
  Rainfall = rain_mean,
  Snowfall = snow_mean
)

# Create rain vs snow plot
p_rain_snow <- ggplot(df_rain_snow, aes(x = Date)) +
  geom_line(aes(y = Rainfall, color = "Rainfall")) +
  geom_line(aes(y = Snowfall, color = "Snowfall")) +
  scale_color_manual(values = c("Rainfall" = "blue", "Snowfall" = "red")) +
  labs(title = "Rainfall vs Snowfall",
       x = "Date",
       y = "Amount (mm)",
       color = "Type") +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("images/precipitation_analysis/rain_vs_snow.png", p_rain_snow, width = 12, height = 6)

# Print summary statistics
cat("\nPrecipitation Summary Statistics:\n")
cat("Mean:", mean(daily_mean, na.rm = TRUE), "mm\n")
cat("Maximum:", max(daily_mean, na.rm = TRUE), "mm\n")
cat("Minimum:", min(daily_mean, na.rm = TRUE), "mm\n")
cat("Standard Deviation:", sd(daily_mean, na.rm = TRUE), "mm\n") 