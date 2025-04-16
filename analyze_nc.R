# Load required packages
library(ncdf4)
library(ggplot2)
library(dplyr)
library(tidyr)

# Open the NetCDF file
nc_file <- nc_open("data/VICOut2.nc")

# Get information about the file
print("File information:")
print(nc_file)

# Get variable names
variables <- names(nc_file$var)
print("\nAvailable variables:")
print(variables)

# Get dimensions
dimensions <- names(nc_file$dim)
print("\nDimensions:")
print(dimensions)

# Function to analyze a variable
analyze_variable <- function(var_name) {
  var_data <- ncvar_get(nc_file, var_name)
  print(paste("\nAnalysis for variable:", var_name))
  print(paste("Dimensions:", paste(dim(var_data), collapse=" x ")))
  print(paste("Min:", min(var_data, na.rm=TRUE)))
  print(paste("Max:", max(var_data, na.rm=TRUE)))
  print(paste("Mean:", mean(var_data, na.rm=TRUE)))
  print(paste("NA count:", sum(is.na(var_data))))
}

# Analyze each variable
for (var in variables) {
  analyze_variable(var)
}

# Close the file
nc_close(nc_file) 