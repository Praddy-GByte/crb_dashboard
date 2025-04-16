# Data Documentation

## SNOTEL Data

SNOTEL (Snow Telemetry) stations provide automated measurements of snowpack and related climate data.

### Variables
- `WTEQ`: Snow water equivalent (inches)
- `PREC`: Precipitation (inches)
- `TEMP`: Air temperature (°F)

### Data Processing
- Daily measurements
- Monthly averages calculated
- Quality flags applied
- Missing data handled using interpolation

## VIC Model Data

The Variable Infiltration Capacity (VIC) model provides simulated hydrological variables.

### Variables
- `SOIL_MOISTURE`: Soil moisture content
- `RUNOFF`: Surface runoff
- `EVAPOTRANSPIRATION`: Evapotranspiration rate
- `PRECIPITATION`: Precipitation amount

### Data Processing
- Daily time step
- Gridded data at 1/16° resolution
- Aggregated to basin and HUC10 scales

## SMAP Data

Soil Moisture Active Passive (SMAP) satellite provides soil moisture observations.

### Variables
- `SOIL_MOISTURE`: Surface soil moisture
- `SOIL_TEMPERATURE`: Soil temperature
- `VEGETATION_WATER_CONTENT`: Vegetation water content

### Data Processing
- Daily observations
- Quality flags applied
- Gap-filled using interpolation

## GRACE Data

Gravity Recovery and Climate Experiment (GRACE) satellites measure terrestrial water storage.

### Variables
- `TWS`: Terrestrial water storage
- `GROUNDWATER`: Groundwater storage
- `SURFACE_WATER`: Surface water storage

### Data Processing
- Monthly observations
- Gap-filled using interpolation
- Quality flags applied 