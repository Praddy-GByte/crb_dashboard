# Data Sources Documentation

## SNOTEL (Snow Telemetry) Network
- **Source**: Natural Resources Conservation Service (NRCS)
- **Resolution**: Point measurements at station locations
- **Variables**: 
  - Snow Water Equivalent (SWE)
  - Snow Depth
  - Temperature
  - Precipitation
- **Update Frequency**: Hourly
- **Coverage**: Western United States
- **Website**: https://www.nrcs.usda.gov/wps/portal/wcc/home/

## VIC (Variable Infiltration Capacity) Model
- **Source**: University of Washington
- **Resolution**: 1/16 degree (~6 km)
- **Variables**:
  - Precipitation
  - Evapotranspiration
  - Runoff
  - Baseflow
  - Soil Moisture
  - Snow Water Equivalent
- **Update Frequency**: Daily
- **Coverage**: Colorado River Basin
- **Website**: https://vic.readthedocs.io/

## SMAP (Soil Moisture Active Passive)
- **Source**: NASA
- **Resolution**: 9 km
- **Variables**:
  - Surface Soil Moisture (0-5 cm)
  - Root Zone Soil Moisture (0-100 cm)
  - Profile Soil Moisture (0-200 cm)
- **Update Frequency**: 3-hourly
- **Coverage**: Global
- **Website**: https://smap.jpl.nasa.gov/

## GRACE (Gravity Recovery and Climate Experiment)
- **Source**: NASA/JPL
- **Resolution**: ~300 km
- **Variables**:
  - Terrestrial Water Storage
  - Groundwater Storage
  - Surface Water Storage
  - Snow Water Storage
- **Update Frequency**: Monthly
- **Coverage**: Global
- **Website**: https://grace.jpl.nasa.gov/

## Data Processing
All data sources are processed and harmonized to ensure consistency in:
- Spatial resolution
- Temporal resolution
- Units and measurements
- Quality control
- Gap filling

## Data Access
- Raw data can be accessed through the respective data provider websites
- Processed data is available through the dashboard's export functionality
- API access is available for programmatic data retrieval

## Data Quality
- All data undergoes quality control checks
- Missing data is flagged and handled appropriately
- Uncertainty estimates are provided where available
- Data validation is performed against ground truth measurements

## VIC Model Data
- **Source**: Variable Infiltration Capacity (VIC) Model
- **Resolution**: 4km spatial, daily temporal
- **Variables**: Precipitation, evapotranspiration, runoff, soil moisture, snow water equivalent
- **Period**: 1982-2024
- **Processing**: Daily aggregation, spatial averaging

## SMAP Data
- **Source**: NASA Soil Moisture Active Passive Mission
- **Resolution**: 9km spatial, daily temporal
- **Variables**: Surface soil moisture, root zone soil moisture
- **Period**: 2015-2024
- **Processing**: Daily aggregation, quality control

## GRACE Data
- **Source**: NASA Gravity Recovery and Climate Experiment
- **Resolution**: ~300km spatial, monthly temporal
- **Variables**: Terrestrial water storage, groundwater storage
- **Period**: 2002-2024
- **Processing**: Monthly aggregation, spatial averaging

## SNOTEL Data
- **Source**: NRCS Snow Telemetry Network
- **Resolution**: Point measurements, daily temporal
- **Variables**: Snow water equivalent, snow depth, temperature, precipitation
- **Period**: 1982-2024
- **Processing**: Daily aggregation, quality control

## USGS Data
- **Source**: United States Geological Survey
- **Resolution**: Point measurements, daily temporal
- **Variables**: Streamflow, water quality
- **Period**: Varies by station
- **Processing**: Daily aggregation, quality control 