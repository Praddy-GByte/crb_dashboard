# Data Structure Documentation

## Directory Organization

```
data/
├── VIC_outputs/
│   ├── {variable}_{year}_annual.tif
│   ├── {variable}_{year}_seasonal.tif
│   └── {variable}_{year}_monthly.tif
├── VIC_json/
│   ├── metadata.json
│   └── vic_{year}.json
├── SMAP_outputs/
│   ├── SMAP_{year}_annual.tif
│   └── SMAP_{year}_timeseries.parquet
└── GRACE_outputs/
    ├── GRACE_{year}_annual.tif
    └── GRACE_{year}_timeseries.parquet
```

## File Formats

### GeoTIFF Files
- Used for spatial data representation
- Contains georeferenced raster data
- Efficient for web visualization
- Example: `OUT_PREC_2024_annual.tif`

### JSON Files
- Metadata and configuration
- Time series summaries
- Variable descriptions
- Example: `vic_2024.json`

### Parquet Files
- Efficient columnar storage
- Used for time series data
- Optimized for analytics
- Example: `SMAP_2024_timeseries.parquet`

## Data Processing Pipeline

1. **Raw Data Input**
   - NetCDF files from models
   - CSV files from stations
   - Binary data from satellites

2. **Processing Steps**
   - Spatial aggregation
   - Temporal averaging
   - Format conversion
   - Quality control

3. **Output Generation**
   - GeoTIFF for maps
   - Parquet for time series
   - JSON for metadata

## Data Update Frequency

- VIC Model: Monthly updates
- SNOTEL: Daily updates
- SMAP: Weekly updates
- GRACE: Monthly updates

# Data Structure and Files

## Directory Structure
```
Dashboard/
├── app.R                 # Main Shiny application file
├── prepare_data.R        # Data preparation and caching functions
├── help.md              # This help documentation
├── images/              # Static image files
│   ├── basin_maps/      # Basin-related maps
│   ├── vic_outputs/     # VIC model output visualizations
│   └── data_plots/      # Various data visualizations
└── data/                # Data files and directories
    ├── static_images/   # Static map images
    ├── Analysis_Basin_Shapefiles/  # Basin shapefiles
    ├── snotel/          # SNOTEL station data
    ├── VIC_outputs/     # VIC model outputs
    ├── huc10s/          # HUC10 watershed data
    └── CRB_poly/        # Colorado River Basin polygons
```

## Data Files

### NetCDF Files
- `SPL4SMGP.007_9km_aid0001.nc` (355MB)
  - SMAP soil moisture data
  - 9km resolution
  - 2015-2024 coverage

- `GRCTellus.JPL.200204_202401.GLO.RL06.1M.MSCNv03CRI.nc` (81MB)
  - GRACE terrestrial water storage
  - Monthly resolution
  - 2002-2024 coverage

- `VICOut2.nc` (1.4GB)
  - VIC model outputs
  - Daily resolution
  - 1982-2024 coverage

- `CRB_PRISM_Calibrated.2024-01-01.nc` (989MB)
  - PRISM calibrated precipitation
  - Daily resolution
  - Basin-wide coverage

### Shapefiles
- `data/CRB_poly/basin_CRB_poly.shp`
  - Main Colorado River Basin boundary
  - Includes attributes for area and population

- `data/Analysis_Basin_Shapefiles/`
  - Sub-basin boundaries
  - Includes Upper, Lower, Central, and other sub-basins

- `data/huc10s/huc10.shp`
  - HUC10 watershed boundaries
  - Includes watershed names and areas

### SNOTEL Data
- `data/snotel/`
  - Daily station data
  - Includes SWE, precipitation, temperature
  - Metadata in `snotel_metadata.csv`

## Data Processing

### Cached Data
The application uses RDS files for faster data loading:
- `basin_data.rds`: Basin shapefiles and attributes
- `huc10_data.rds`: HUC10 watershed data
- `snotel_data.rds`: SNOTEL station data
- `vic_data.rds`: VIC model outputs
- `smap_data.rds`: SMAP soil moisture data
- `grace_data.rds`: GRACE water storage data

### Data Preparation
The `prepare_data.R` script handles:
1. Loading raw data files
2. Processing and formatting
3. Creating cached RDS files
4. Generating summary statistics

### Data Updates
- SNOTEL data: Daily updates
- VIC model: Monthly updates
- SMAP data: 2-3 day updates
- GRACE data: Monthly updates

### Data Quality Control
- Automated validation checks
- Missing data handling
- Outlier detection
- Temporal consistency checks

### Data Access
- All data is stored locally
- No external API calls required
- Data can be exported in various formats
- Raw data files are read-only 