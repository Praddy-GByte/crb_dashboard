library(ncdf4)

# Read VIC data
nc <- nc_open("data/VICOut2.nc")

# Print file information
cat("NetCDF File Information:\n")
print(nc)

# Get time dimension
time <- ncvar_get(nc, "time")
cat("\nTime dimension length:", length(time), "\n")

# Get time units
time_units <- ncatt_get(nc, "time")$units
cat("Time units:", time_units, "\n")

# Get first and last time values
cat("First time value:", time[1], "\n")
cat("Last time value:", time[length(time)], "\n")

# Try different origins
origins <- c("1982-01-01", "1900-01-01", "0001-01-01")
for (origin in origins) {
  dates <- as.Date(time, origin = origin)
  cat("\nUsing origin", origin, ":\n")
  cat("Start date:", format(min(dates), "%Y-%m-%d"), "\n")
  cat("End date:", format(max(dates), "%Y-%m-%d"), "\n")
  cat("Number of days:", length(dates), "\n")
}

# Close the NetCDF file
nc_close(nc) 