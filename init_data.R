# Initialize required directories and sample data
library(sf)
library(raster)
library(dplyr)
library(lubridate)
library(ncdf4)
library(units)

# Create directories if they don't exist
dirs <- c("data", "cache", 
          "data/Analysis_Basin_Shapefiles",
          "data/huc10s",
          "data/snotel",
          "data/VIC_outputs",
          "data/SMAP",
          "data/GRACE")

for (dir in dirs) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }
}

# Create sample basin data with valid geometry and required columns
basin <- st_sfc(st_polygon(list(rbind(
  c(-120, 35),
  c(-110, 35),
  c(-110, 45),
  c(-120, 45),
  c(-120, 35)
))), crs = 4326)

basin_data <- st_sf(
  name = "Colorado River Basin",
  area_km2 = as.numeric(set_units(st_area(basin), "km^2")),
  elevation = 2000,
  slope = 5,
  aspect = 180,
  land_cover = "Mixed",
  soil_type = "Loam",
  source = "Sample Data",
  version = "1.0",
  last_updated = format(Sys.time(), "%Y-%m-%d"),
  geometry = basin
)

# Ensure valid geometry
basin_data <- st_make_valid(basin_data)
st_write(basin_data, "data/Analysis_Basin_Shapefiles/basins_all_unique.shp", delete_layer = TRUE)

# Create sample HUC10 data with valid geometry
huc10 <- st_sfc(st_polygon(list(rbind(
  c(-115, 40),
  c(-114, 40),
  c(-114, 41),
  c(-115, 41),
  c(-115, 40)
))), crs = 4326)

huc10_data <- st_sf(
  huc10 = "1234567890",
  name = "Sample HUC10",
  area_km2 = as.numeric(set_units(st_area(huc10), "km^2")),
  elevation = 1800,
  slope = 3,
  aspect = 90,
  land_cover = "Forest",
  soil_type = "Sandy Loam",
  geometry = huc10
)

# Ensure valid geometry
huc10_data <- st_make_valid(huc10_data)
st_write(huc10_data, "data/huc10s/huc10.shp", delete_layer = TRUE)

# Create sample SNOTEL data
dates <- seq(as.Date("2020-01-01"), as.Date("2024-01-01"), by = "day")
set.seed(123)  # For reproducibility
snotel_data <- data.frame(
  date = rep(dates, 2),
  station_id = rep(c("1001", "1002"), each = length(dates)),
  WTEQ = round(pmax(0, rnorm(length(dates) * 2, mean = 10, sd = 2)), 1),
  PREC = round(pmax(0, rnorm(length(dates) * 2, mean = 5, sd = 1)), 1),
  TEMP = round(rnorm(length(dates) * 2, mean = 15, sd = 5), 1),
  lat = rep(c(40, 41), each = length(dates)),
  lon = rep(c(-115, -114), each = length(dates)),
  elevation = rep(c(2500, 2700), each = length(dates)),
  name = rep(c("Station 1", "Station 2"), each = length(dates))
)
write.csv(snotel_data, "data/snotel/snotel_CAP.csv", row.names = FALSE)

# Create sample VIC data with proper variables
vic_file <- "data/VIC_outputs/vic_sample.nc"
nx <- 20
ny <- 20
nt <- 365

# Create dimensions
londim <- ncdim_def("lon", "degrees_east", seq(-120, -110, length.out = nx))
latdim <- ncdim_def("lat", "degrees_north", seq(35, 45, length.out = ny))
timedim <- ncdim_def("time", "days since 2020-01-01", 1:nt)

# Define variables
vars <- list(
  OUT_PREC = ncvar_def("OUT_PREC", "mm/day", list(londim, latdim, timedim), -999),
  OUT_SWE = ncvar_def("OUT_SWE", "mm", list(londim, latdim, timedim), -999),
  OUT_RUNOFF = ncvar_def("OUT_RUNOFF", "mm/day", list(londim, latdim, timedim), -999),
  OUT_BASEFLOW = ncvar_def("OUT_BASEFLOW", "mm/day", list(londim, latdim, timedim), -999),
  OUT_EVAP = ncvar_def("OUT_EVAP", "mm/day", list(londim, latdim, timedim), -999),
  OUT_AIR_TEMP = ncvar_def("OUT_AIR_TEMP", "C", list(londim, latdim, timedim), -999),
  OUT_SOIL_MOIST_1 = ncvar_def("OUT_SOIL_MOIST_1", "mm", list(londim, latdim, timedim), -999),
  OUT_SOIL_MOIST_2 = ncvar_def("OUT_SOIL_MOIST_2", "mm", list(londim, latdim, timedim), -999),
  OUT_SOIL_MOIST_3 = ncvar_def("OUT_SOIL_MOIST_3", "mm", list(londim, latdim, timedim), -999)
)

# Create netCDF file
ncout <- nc_create(vic_file, vars)

# Add data to variables
for (var in names(vars)) {
  set.seed(which(names(vars) == var))  # Different seed for each variable
  data <- array(pmax(0, rnorm(nx * ny * nt, mean = 10, sd = 2)), dim = c(nx, ny, nt))
  ncvar_put(ncout, vars[[var]], data)
}

nc_close(ncout)

# Create sample SMAP data
smap_file <- "data/SMAP/SPL4SMGP.007_9km_aid0001.nc"
smap_vars <- list(
  sm_profile = ncvar_def("sm_profile", "m3/m3", list(londim, latdim, timedim), -999),
  sm_rootzone = ncvar_def("sm_rootzone", "m3/m3", list(londim, latdim, timedim), -999)
)

ncout <- nc_create(smap_file, smap_vars)
for (var in names(smap_vars)) {
  set.seed(which(names(smap_vars) == var))
  data <- array(pmax(0, pmin(1, rnorm(nx * ny * nt, mean = 0.3, sd = 0.1))), dim = c(nx, ny, nt))
  ncvar_put(ncout, smap_vars[[var]], data)
}
nc_close(ncout)

# Create sample GRACE data
grace_file <- "data/GRACE/GRCTellus.JPL.200204_202401.GLO.RL06.1M.MSCNv03CRI.nc"
grace_vars <- list(
  lwe_thickness = ncvar_def("lwe_thickness", "cm", list(londim, latdim, timedim), -999),
  uncertainty = ncvar_def("uncertainty", "cm", list(londim, latdim, timedim), -999)
)

ncout <- nc_create(grace_file, grace_vars)
for (var in names(grace_vars)) {
  set.seed(which(names(grace_vars) == var))
  data <- array(rnorm(nx * ny * nt, mean = 0, sd = 5), dim = c(nx, ny, nt))
  if (var == "uncertainty") {
    data <- abs(data)
  }
  ncvar_put(ncout, grace_vars[[var]], data)
}
nc_close(ncout)

# Create VIC summaries for caching
vic_summaries <- list()
for (var in names(vars)) {
  vic_summaries[[var]] <- array(runif(nx * ny), dim = c(nx, ny))
}
saveRDS(vic_summaries, "cache/vic_summaries.rds")

# Create last update timestamp
writeLines(as.character(Sys.time()), "cache/last_update.txt")

print("Sample data initialization complete with proper data structures and required columns!") 