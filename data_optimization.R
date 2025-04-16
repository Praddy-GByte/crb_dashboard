# Load required packages
library(tools)
library(fs)

# Function to compress NetCDF files
compress_netcdf <- function(input_file, output_file) {
  system(paste("nccopy -d 5", input_file, output_file))
}

# Function to convert large HTML files to static images
convert_html_to_static <- function(html_file, output_dir) {
  # Create output directory if it doesn't exist
  dir_create(output_dir)
  
  # Convert HTML to static images using webshot
  webshot::webshot(html_file, 
                   file.path(output_dir, "preview.png"),
                   vwidth = 1200,
                   vheight = 800)
}

# Main optimization process
optimize_data <- function() {
  # Create optimized data directory
  dir_create("docs/data_optimized")
  
  # Compress NetCDF files
  netcdf_files <- list.files("data", pattern = "\\.nc$", full.names = TRUE)
  for (file in netcdf_files) {
    output_file <- file.path("docs/data_optimized", 
                            paste0(tools::file_path_sans_ext(basename(file)), 
                                  "_compressed.nc"))
    compress_netcdf(file, output_file)
  }
  
  # Convert large HTML files to static images
  html_files <- list.files(".", pattern = "\\.html$", full.names = TRUE)
  for (file in html_files) {
    if (file.size(file) > 10 * 1024 * 1024) {  # Files larger than 10MB
      output_dir <- file.path("docs/static_images", 
                             tools::file_path_sans_ext(basename(file)))
      convert_html_to_static(file, output_dir)
    }
  }
  
  # Create data manifest
  manifest <- data.frame(
    original_file = c(netcdf_files, html_files),
    optimized_file = c(
      file.path("data_optimized", 
                paste0(tools::file_path_sans_ext(basename(netcdf_files)), 
                      "_compressed.nc")),
      file.path("static_images", 
                tools::file_path_sans_ext(basename(html_files)))
    ),
    size_original = file.size(c(netcdf_files, html_files)),
    size_optimized = file.size(c(
      file.path("docs/data_optimized", 
                paste0(tools::file_path_sans_ext(basename(netcdf_files)), 
                      "_compressed.nc")),
      file.path("docs/static_images", 
                tools::file_path_sans_ext(basename(html_files)))
    ))
  )
  
  write.csv(manifest, "docs/data_manifest.csv", row.names = FALSE)
}

# Run optimization
optimize_data() 