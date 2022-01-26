import os
import numpy as np
import pandas as pd
import geopandas as gpd
import rasterio as rio
from rasterio import mask
import rioxarray
import xarray
from pathlib import Path

project_dir = Path(__file__).resolve().parents[1]
data_dir = os.path.join(project_dir, "data")

def clean_mask(geom, dataset, **mask_kw):
    mask_kw.setdefault('crop', True)
    mask_kw.setdefault('all_touched', True)
    mask_kw.setdefault('filled', False)
    masked, mask_transform = mask.mask(dataset=dataset, shapes=(geom,), **mask_kw)
    return masked

def raster_calc_annual(geodf, raster_file, year, func, col, all_touched = True):
    """
    Apply raster calculation function to geometries of a GeoDataFrame for a specified year.
    Specify which column of
    """
    with rio.open(raster_file) as file:
        geodf.loc[(geodf.Year == year), col] = \
        geodf.loc[(geodf.Year == year)].geometry.apply(clean_mask, all_touched = all_touched, dataset = file).apply(func)

def raster_calc_seasonal(geodf, raster_file, year, month, func, col, all_touched = True):
    """
    Apply raster calculation function to geometries of a GeoDataFrame for a specified year.
    Specify which column of
    """
    with rio.open(raster_file) as file:
        geodf.loc[(geodf.Year == year) & (geodf.Month == month), col] = \
        geodf.loc[(geodf.Year == year) & (geodf.Month == month)].geometry.apply(clean_mask,
                    all_touched = all_touched, dataset = file).apply(func)

def xarray_calc_seasonal(geodf, data, start_date, end_date, func, col, all_touched = True):
    data_slice = data.sel(T=slice(start_date, end_date)).sum(dim = 'T')

    if not os.path.exists(os.path.join(data_dir, 'intermediate_files')):
        os.mkdir(os.path.join(data_dir,'intermediate_files'))

    data_slice.precipitation.rio.set_spatial_dims(x_dim = 'x', y_dim = 'y')\
                            .rio.set_crs('EPSG:4326')\
                            .rio.to_raster(os.path.join(data_dir, 'intermediate_files', 'clipped_precip.tif'))

    with rio.open(os.path.join(data_dir, 'intermediate_files', 'clipped_precip.tif')) as raster:

        geodf.loc[(geodf['datetime_start'] == start_date) & (geodf['datetime_end'] == end_date), col] = \
        geodf.loc[(geodf['datetime_start'] == start_date) & (geodf['datetime_end'] == end_date)].geometry.apply(clean_mask,
                    all_touched = all_touched, pad = True, dataset = raster).apply(func)

    os.remove(os.path.join(data_dir, 'intermediate_files', 'clipped_precip.tif'))

def mean_ipc(masked):
    """
    Return the mean IPC rating for a masked region.
    """
    x = masked
    x[(x > 5) | (x < 1)] = 0
    return(np.ma.mean(x[x.nonzero()]))

def samp_var_ipc(masked):
    """
    Return the sample variance of IPC rating for a masked region
    """
    x = masked
    x[(x > 5) | (x < 1)] = 0
    return(np.ma.var(x[x.nonzero()], ddof = 1))

def prop_flood(masked):
    """
    Return proportion of total raster cells in a masked area that have indicated flooding
    for at least 1 day and less than 90 days in a given year
    """
    return(np.ma.sum((masked > 0) & (masked < 9))/ np.ma.sum(masked.mask))

def mean_flood_duration(masked):
    """
    Return mean flood duration for a masked area in days. Only calcualte for areas where
    water was detected between 0 and 90 days of the year
    """
    return(np.ma.mean(masked[(masked > 0) & (masked < 9)]) * 10)
