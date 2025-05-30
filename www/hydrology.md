# VIC Hydrology Model

The Variable Infiltration Capacity (VIC) model is a large-scale, semi-distributed hydrologic model that solves full water and energy balances.

## Model Components

### 1. Water Balance
- Precipitation (rain and snow)
- Evapotranspiration
- Runoff generation
- Baseflow
- Soil moisture dynamics
- Snow accumulation and melt

### 2. Energy Balance
- Radiation
- Sensible heat flux
- Latent heat flux
- Ground heat flux
- Snow energy balance

## Model Configuration

### Spatial Resolution
- Grid cell size: 4km
- Coverage: Entire Colorado River Basin
- Elevation bands: Multiple per grid cell

### Temporal Resolution
- Time step: Daily
- Output frequency: Daily
- Analysis period: 1982-2024

### Soil Layers
1. Surface layer (0-10cm)
2. Root zone layer (10-40cm)
3. Deep layer (40-100cm)

## Key Variables

### Water Balance Components
- **Precipitation**: Total incoming precipitation (rain + snow)
- **Evapotranspiration**: Total water loss to atmosphere
- **Runoff**: Surface water flow
- **Baseflow**: Subsurface water flow
- **Soil Moisture**: Water content in soil layers
- **Snow Water Equivalent**: Water content in snowpack

### Efficiency Metrics
- **Supply Efficiency**: (Runoff + Baseflow) / Precipitation
- **Runoff Efficiency**: Runoff / Precipitation
- **Baseflow Efficiency**: Baseflow / Precipitation

## Model Applications

### 1. Water Resource Assessment
- Surface water availability
- Groundwater recharge
- Drought monitoring
- Flood potential

### 2. Climate Impact Studies
- Temperature effects
- Precipitation changes
- Extreme events
- Long-term trends

### 3. Management Support
- Reservoir operations
- Water allocation
- Drought planning
- Climate adaptation 