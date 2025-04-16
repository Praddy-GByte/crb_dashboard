import os
import xarray as xr
import matplotlib.pyplot as plt
import numpy as np
from pathlib import Path
import rasterio
from rasterio.plot import show
import matplotlib.colors as colors

# Set up paths
data_dir = Path('../data/VIC_outputs')
output_dir = Path('../data/static_images/vic_analysis')
output_dir.mkdir(parents=True, exist_ok=True)

def process_tiff_file(tiff_path, output_path):
    """Process a single TIFF file and save its visualization."""
    try:
        with rasterio.open(tiff_path) as src:
            data = src.read(1)
            # Create a masked array for nodata values
            data = np.ma.masked_where(data == src.nodata, data)
            
            # Create figure
            plt.figure(figsize=(10, 8))
            
            # Plot the data
            im = plt.imshow(data, cmap='viridis')
            plt.colorbar(im, label='Value')
            
            # Add title
            var_name = tiff_path.stem.replace('OUT_', '').replace('_annual', '')
            plt.title(f'{var_name} - Annual Average')
            
            # Save the figure
            plt.savefig(output_path, dpi=300, bbox_inches='tight')
            plt.close()
            
            print(f"Generated image for {var_name}")
    except Exception as e:
        print(f"Error processing {tiff_path}: {str(e)}")

def process_netcdf_file(nc_path, output_path):
    """Process a single NetCDF file and save its visualization."""
    try:
        ds = xr.open_dataset(nc_path)
        
        # Get the first variable in the dataset
        var_name = list(ds.data_vars.keys())[0]
        data = ds[var_name]
        
        # Create figure
        plt.figure(figsize=(10, 8))
        
        # Plot the data
        if len(data.dims) > 2:  # If there's a time dimension
            data = data.mean(dim='time')
        
        im = data.plot(cmap='viridis')
        plt.colorbar(im, label=var_name)
        
        # Add title
        year = nc_path.stem.split('.')[1].split('-')[0]
        plt.title(f'PRISM Calibrated Data - {year}')
        
        # Save the figure
        plt.savefig(output_path, dpi=300, bbox_inches='tight')
        plt.close()
        
        print(f"Generated image for {year}")
    except Exception as e:
        print(f"Error processing {nc_path}: {str(e)}")

def main():
    # Process all TIFF files
    tiff_files = list(data_dir.glob('*.tif'))
    for tiff_file in tiff_files:
        if tiff_file.name != 'time_bnds_annual.tif':  # Skip time bounds file
            output_path = output_dir / f"{tiff_file.stem}.png"
            process_tiff_file(tiff_file, output_path)
    
    # Process all NetCDF files
    nc_files = list(data_dir.glob('*.nc'))
    for nc_file in nc_files:
        year = nc_file.stem.split('.')[1].split('-')[0]
        output_path = output_dir / f"prism_calibrated_{year}.png"
        process_netcdf_file(nc_file, output_path)

if __name__ == "__main__":
    main() 