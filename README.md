# Colorado River Basin Dashboard

A Shiny application for analyzing Colorado River Basin hydrological and climate data, including precipitation, evapotranspiration, runoff, soil moisture, snow water equivalent, and temperature data.

## Features

- Spatial distribution visualization
- Time series analysis
- Monthly statistics
- Temperature analysis
- Basin-specific analysis
- Climate change impact assessment
- SNOTEL station data integration
- GRACE and SMAP data visualization

## Prerequisites

- R (version 4.0.0 or higher)
- Required R packages:
  - shiny
  - ncdf4
  - ggplot2
  - dplyr
  - lubridate
  - sf
  - raster
  - terra
  - gridExtra
  - scales
  - viridis
  - plotly

## Installation

1. Clone the repository:
```bash
git clone https://github.com/yourusername/crb_dashboard.git
cd crb_dashboard
```

2. Install required R packages:
```R
install.packages(c("shiny", "ncdf4", "ggplot2", "dplyr", "lubridate", "sf", 
                  "raster", "terra", "gridExtra", "scales", "viridis", "plotly"))
```

3. Place your data files in the `data` directory:
   - VIC output files
   - CRB basin shapefiles
   - Temperature data
   - Elevation data
   - SNOTEL station data
   - GRACE and SMAP data

## Running the App

1. Start the Shiny app:
```R
Rscript run_app.R
```

2. Open your web browser and navigate to:
```
http://localhost:3838
```

## Deployment

### Option 1: Deploy to shinyapps.io

1. Install the rsconnect package:
```R
install.packages('rsconnect')
```

2. Configure your shinyapps.io account:
```R
rsconnect::setAccountInfo(name='your-account-name',
                         token='your-token',
                         secret='your-secret')
```

3. Deploy the app:
```R
rsconnect::deployApp()
```

### Option 2: Deploy to RStudio Connect

1. Install the rsconnect package:
```R
install.packages('rsconnect')
```

2. Configure your RStudio Connect server:
```R
rsconnect::addServer(url = "https://your-connect-server.com",
                    name = "your-server-name")
```

3. Deploy the app:
```R
rsconnect::deployApp()
```

### Option 3: Deploy to Shiny Server

1. Install Shiny Server on your Linux server:
```bash
sudo apt-get install gdebi-core
wget https://download3.rstudio.org/ubuntu-14.04/x86_64/shiny-server-1.5.17.973-amd64.deb
sudo gdebi shiny-server-1.5.17.973-amd64.deb
```

2. Copy your app to the Shiny Server directory:
```bash
sudo cp -R /path/to/your/app /srv/shiny-server/
```

3. Set proper permissions:
```bash
sudo chown -R shiny:shiny /srv/shiny-server/your-app
```

## Data Requirements

The app requires the following data files:
- VIC model outputs (NetCDF format)
- CRB basin boundary shapefiles
- Temperature data (NetCDF format)
- Elevation data (GeoTIFF format)
- SNOTEL station data (CSV format)
- GRACE and SMAP data (NetCDF format)

## License

This project is licensed under the MIT License - see the LICENSE file for details.

## Contact

For questions or support, please open an issue in the GitHub repository. 