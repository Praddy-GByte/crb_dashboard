# Colorado River Basin Dashboard Help

## Overview
This dashboard provides comprehensive visualization and analysis of the Colorado River Basin's water resources. It integrates data from multiple sources including VIC model outputs, SNOTEL stations, SMAP soil moisture, and GRACE terrestrial water storage.

## Data Sources

### VIC Model
- **Resolution**: 4km spatial, daily temporal
- **Coverage**: 1982-2024
- **Variables**:
  - Precipitation
  - Evapotranspiration
  - Runoff
  - Soil Moisture
  - Snow Water Equivalent
  - Baseflow

### SNOTEL Network
- **Stations**: 100+ across the basin
- **Update Frequency**: Daily
- **Metrics**:
  - Snow Water Equivalent (SWE)
  - Snow Depth
  - Temperature
  - Precipitation

### SMAP (Soil Moisture Active Passive)
- **Resolution**: 9km spatial, 2-3 day temporal
- **Coverage**: 2015-2024
- **Layers**:
  - Surface (0-5 cm)
  - Root Zone (0-100 cm)
  - Profile (0 cm to bedrock)

### GRACE (Gravity Recovery and Climate Experiment)
- **Resolution**: ~300km spatial, monthly temporal
- **Coverage**: 2002-2024
- **Primary Metric**: Total Water Storage (TWS)

## Dashboard Tabs

### Overview
- Displays key basin statistics
- Shows recent conditions across all data sources
- Provides data coverage information

### Basin Analysis
- Interactive maps of the Colorado River Basin
- HUC10 watershed boundaries
- SNOTEL station locations
- Basin characteristics and statistics

### Sub-Basins
- Detailed information about major sub-basins
- Interactive sub-basin maps
- Comparative statistics

### VIC Model
- Spatial distribution of model outputs
- Time series analysis
- Monthly statistics
- Variable selection and year comparison

### SNOTEL Data
- Station locations and data
- Time series analysis
- Elevation-based analysis
- Year-to-year comparisons

### SMAP Data
- Soil moisture maps
- Layer comparison
- Seasonal analysis
- Anomaly detection

### GRACE Data
- Total water storage maps
- Time series with trend analysis
- Seasonal patterns
- TWS anomalies

### Data Comparison
- Cross-validation between data sources
- Model performance assessment
- Trend analysis
- Anomaly detection

## Usage Tips

1. **Interactive Maps**:
   - Zoom in/out using mouse wheel or +/- buttons
   - Click on features for detailed information
   - Use layer controls to toggle different data layers

2. **Time Series Plots**:
   - Hover over data points for values
   - Use the legend to toggle different variables
   - Zoom in on specific time periods

3. **Data Selection**:
   - Use dropdown menus to select variables
   - Adjust year sliders for temporal analysis
   - Select different layers for comparison

4. **Exporting Data**:
   - Right-click on plots to save images
   - Use the download buttons for data export
   - Copy-paste tables for further analysis

## Troubleshooting

1. **Slow Loading**:
   - Clear browser cache
   - Reduce the number of active tabs
   - Check internet connection

2. **Missing Data**:
   - Verify data source availability
   - Check date ranges
   - Contact support if issues persist

3. **Visualization Issues**:
   - Try refreshing the page
   - Update browser to latest version
   - Check browser compatibility

## Contact Information

For technical support or data inquiries, please contact:
- Email: support@coloradoriverbasin.org
- Phone: (555) 123-4567
- Website: www.coloradoriverbasin.org

## Data Structure and Files

### Directory Structure
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

### Data Files

#### NetCDF Files
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

#### Shapefiles
- `data/CRB_poly/basin_CRB_poly.shp`
  - Main Colorado River Basin boundary
  - Includes attributes for area and population

- `data/Analysis_Basin_Shapefiles/`
  - Sub-basin boundaries
  - Includes Upper, Lower, Central, and other sub-basins

- `data/huc10s/huc10.shp`
  - HUC10 watershed boundaries
  - Includes watershed names and areas

#### SNOTEL Data
- `data/snotel/`
  - Daily station data
  - Includes SWE, precipitation, temperature
  - Metadata in `snotel_metadata.csv`

### Data Processing

#### Cached Data
The application uses RDS files for faster data loading:
- `basin_data.rds`: Basin shapefiles and attributes
- `huc10_data.rds`: HUC10 watershed data
- `snotel_data.rds`: SNOTEL station data
- `vic_data.rds`: VIC model outputs
- `smap_data.rds`: SMAP soil moisture data
- `grace_data.rds`: GRACE water storage data

#### Data Preparation
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