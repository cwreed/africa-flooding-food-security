import numpy as np
import pandas as pd
import geopandas as gpd
import rasterio as rio
from rasterio import mask
import os
import re

def add_time_to_df(geodf, geo_ID_col, years, months):
    IDs = list(geodf[geo_ID_col])

    time = list(it.product(IDs, years, months))
    inter_df = pd.DataFrame(time, columns = [geo_ID_col, 'Year', 'Month']).set_index(geo_ID_col, drop = True)
    inter_df['datetime'] = pd.to_datetime(['{}-{}-01'.format(y,m) for y, m in list(zip(inter_df.Year, inter_df.Month))])

    new_df = geodf.join(inter_df, on = geo_ID_col)
    return(new_df)

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

def mean_ipc(masked):
    """
    Return the mean IPC rating for a masked region.
    """
    return(np.ma.mean(masked.data))

def prop_flood(masked):
    """
    Return proportion of total raster cells in a masked area that have indicated flooding
    for at least 1 day and less than 90 days in a given year
    """
    return(np.ma.sum((masked.data > 0) & (masked.data < 9))/ np.ma.sum(masked.mask))

def mean_flood_duration(masked):
    """
    Return mean flood duration for a masked area in days. Only calcualte for areas where
    water was detected between 0 and 90 days of the year
    """
    return(np.ma.mean(masked[(masked.data > 0) & (masked.data < 9)]) * 10)
