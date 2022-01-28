import numpy as np
import pandas as pd
import geopandas as gpd
import rasterio as rio
from datetime import datetime, timedelta
from cftime import num2date, date2num
import os
import re
import xarray as xr
import rioxarray
import mask_utils
import utils
import logging
from pathlib import Path

logger = logging.getLogger('dataset_builder')
logger.setLevel(logging.INFO)

project_dir = Path(__file__).resolve().parents[1]
data_dir = os.path.join(project_dir, "data")

shp_file = os.path.join(data_dir, 'IPC/LZ_adm2_geometryFix_population.gpkg')
ipc_dir = os.path.join(data_dir, 'IPC/updated_files/')
flood_file = os.path.join(data_dir, 'DFO flood extents/FloodsArchived_shp/floods_with_dambreaks.shp')
country_names = sorted(['Chad', 'Mali', 'Niger', 'Nigeria', 'Mauritania',
                        'Burkina Faso', 'Sudan', 'South Sudan',
                        'Ethiopia', 'Uganda', 'Kenya', 'Somalia', 'Zambia',
                        'Zimbabwe', 'Mozambique', 'Malawi'])

ipc_files = sorted([i for i in os.listdir(ipc_dir) if re.match(r'^.*.tif$', i)])
ipc_timestamps = [fname.split('_')[1] for fname in ipc_files]
ipc_year_month = [(int(ym[:4]), int(ym[4:])) for ym in ipc_timestamps]

precip_url = 'http://iridl.ldeo.columbia.edu/SOURCES/.UCSB/.CHIRPS/.v2p0/.monthly/.global/.precipitation/dods'

def main():

    places_df = gpd.read_file(shp_file)
    places_df = places_df[places_df['ADMIN0'].isin(country_names)]

    df = utils.add_time_to_df_ipc(places_df, 'FIDcalc', ipc_year_month)

    bounding_dates = list(zip(df['datetime_start'].unique(), df['datetime_end'].unique()))

    df = df.rename(columns={'ADMIN0': 'Name'})
    df = df.to_crs('EPSG:4326')
    df = df.reset_index()

    logger.info("Initial locations dataset created.")

    # Append flood data from DFO polygons
    dfo_df = gpd.read_file(flood_file)

    df = utils.build_flood_poly_feats(df, dfo_df, country_names)

    logger.info("Flood data added successfully.")

    # Add Mean IPC rating data
    df['mean_ipc'] = None
    df['var_ipc'] = None

    for i, file in enumerate(ipc_files):
        mask_utils.raster_calc_seasonal(df, os.path.join(ipc_dir, file), ipc_year_month[i][0], ipc_year_month[i][1],
        mask_utils.mean_ipc, 'mean_ipc', all_touched = True)
        mask_utils.raster_calc_seasonal(df, os.path.join(ipc_dir, file), ipc_year_month[i][0], ipc_year_month[i][1],
        mask_utils.samp_var_ipc, 'var_ipc', all_touched = True)

    logger.info("IPC data added successfully.")

    df['mean_ipc'] = df['mean_ipc'].astype(float)

    df['total_precip_mm'] = None
    df['max_monthly_precip_mm'] = None

    precip = xr.open_dataset(precip_url, engine='netcdf4', decode_times = False)
    precip['T'] = num2date(precip['T'], precip['T'].units, calendar = '360_day') - timedelta(days = 15)
    datetimeindex = precip.indexes['T'].to_datetimeindex()
    precip['T'] = datetimeindex

    precip = precip.rename({'X': 'x', 'Y': 'y'})
    precip = precip.rio.write_crs('EPSG:4326')
    precip = precip.reindex(x = list(reversed(precip.x)))

    for start_date, end_date in bounding_dates:
        mask_utils.xarray_calc_seasonal(df, precip, start_date, end_date, np.ma.sum, 'total_precip_mm')
        mask_utils.xarray_calc_seasonal(df, precip, start_date, end_date, np.ma.max, 'max_monthly_precip_mm')

    logger.info("Precipitation data added successfully.")

    # First difference variables to build stationary features
    df = df.assign(
    season = lambda x: x['Month'].map(lambda month: "0" if month == 10 else
                                                       ("1" if ((month == 12) | (month == 1)) else
                                                           ("2" if ((month == 2) | (month == 4)) else
                                                               "3")))
    )

    df['mean_ipc_diff'] = df.groupby(['Name', 'ADMIN1', 'ADMIN2', 'FIDcalc'])['mean_ipc'].diff()
    df['var_ipc_diff'] = df.groupby(['Name', 'ADMIN1', 'ADMIN2', 'FIDcalc'])['var_ipc'].diff()
    df['no_floods_diff'] = df.groupby(['Name', 'ADMIN1', 'ADMIN2', 'FIDcalc'])['no_flood_events'].diff()
    df['total_flood_area_diff'] = df.groupby(['Name', 'ADMIN1', 'ADMIN2', 'FIDcalc'])['total_flood_area_std'].diff()
    df['total_flood_dur_diff'] = df.groupby(['Name', 'ADMIN1', 'ADMIN2', 'FIDcalc'])['total_flood_dur_days'].diff()
    logger.info("Time series features calculated successfully.")

    df.to_file(os.path.join(data_dir, 'flood_ipc_seasonal_adm2_timeseries.gpkg'), driver = 'GPKG')

    logger.info("Geopackage written: available at {}flood_ipc_seasonal_adm2_timeseries.gpkg\n".format(data_dir))

if __name__ == '__main__':
    main()
