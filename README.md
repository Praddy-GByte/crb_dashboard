# Colorado River Basin Dashboard

An interactive dashboard for visualizing and analyzing hydrological data in the Colorado River Basin.

## Data Sources

- VIC Model Outputs
- SMAP Soil Moisture Data
- GRACE Terrestrial Water Storage Data
- Basin and HUC10 Shapefiles

## Dependencies

Required R packages:
```r
install.packages(c(
  "shiny",
  "shinydashboard",
  "ggplot2",
  "plotly",
  "raster",
  "sf",
  "dplyr",
  "tidyr",
  "lubridate",
  "DT",
  "ncdf4",
  "leaflet",
  "viridis",
  "markdown"
))
```

## Directory Structure

```
Dashboard/
├── data/
│   ├── raw_data/
│   │   ├── VICOut2.nc
│   │   ├── SPL4SMGP.007_9km_aid0001.nc
│   │   ├── GRCTellus.JPL.200204_202401.GLO.RL06.1M.MSCNv03CRI.nc
│   │   ├── Analysis_Basin_Shapefiles/
│   │   └── huc10s/
│   └── processed/
├── www/
│   ├── images/
│   └── css/
└── R/
```

## Running the Dashboard

1. Make sure all required packages are installed
2. Place your data files in the appropriate directories
3. Run the dashboard using:

```r
shiny::runApp()
```

## Features

- Interactive maps of the Colorado River Basin
- VIC model output visualization
- SMAP soil moisture analysis
- GRACE water storage analysis
- Precipitation patterns
- Snow water equivalent analysis
- Soil moisture analysis
- Statistical analysis and trends

## Contact

For questions or issues, please contact [Your Contact Information] 