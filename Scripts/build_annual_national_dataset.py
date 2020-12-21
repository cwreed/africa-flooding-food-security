import numpy as np
import pandas as pd
import geopandas as gpd
import rasterio as rio
from rasterio import mask
import os
import re
import mask_utils
import utils

"""
    Load files and directories to local data
"""
data_dir = '../../Data/'

flood_dir = os.path.join(data_dir, 'DFO flood extents/uncropped/')
flood_files = [i for i in os.listdir(flood_dir)]

ipc_dir = os.path.join(data_dir, 'IPC/updated_files/')
ipc_files = sorted([i for i in os.listdir(ipc_dir) if re.match(r'^.*201[4-9].*\.tif$', i)])[3:]

price_file = os.path.join(data_dir, 'Market prices/price_food_country_staples_index.csv')
displacement_file = os.path.join(data_dir, 'flood_displacement.csv')

shp_dir = os.path.join(data_dir, 'ADM shapefiles/')
country_names = sorted(['TCD', 'MLI', 'NER', 'NGA', 'MRT', 'BFA', 'SDN', 'SSD',
                        'ETH', 'UGA', 'KEN', 'SOM', 'ZMB', 'ZWE', 'MOZ', 'MWI'])
country_shapes = {}
years = np.arange(2014, 2019)

def main():

    # Build out GeoDataFrame using all countries of interest
    for c in range(len(country_names)):
        d = gpd.read_file(os.path.join(shp_dir, '{}_adm/{}_adm0.shp'.format(country_names[c], country_names[c])))
        country_shapes[c] = d

    country_df = pd.concat(country_shapes.values(), ignore_index = True)

    country_df = country_df.drop(['ISO3', 'NAME_ENGLI',
       'NAME_ISO', 'NAME_FAO', 'NAME_LOCAL', 'NAME_OBSOL', 'NAME_VARIA',
       'NAME_NONLA', 'NAME_FRENC', 'NAME_SPANI', 'NAME_RUSSI',
       'NAME_ARABI', 'NAME_CHINE', 'WASPARTOF', 'CONTAINS', 'SOVEREIGN',
       'ISO2', 'WWW', 'FIPS', 'ISON', 'VALIDFR', 'VALIDTO', 'UNREGION1', 'UNREGION2', 'DEVELOPING', 'CIS',
       'Transition', 'OECD', 'WBREGION', 'WBINCOME', 'WBDEBT', 'WBOTHER',
       'CEEAC', 'CEMAC', 'CEPLG', 'COMESA', 'EAC', 'ECOWAS', 'IGAD',
       'IOC', 'MRU', 'SACU', 'UEMOA', 'UMA', 'PALOP', 'PARTA', 'CACM',
       'EurAsEC', 'Agadir', 'SAARC', 'ASEAN', 'NAFTA', 'GCC', 'CSN',
       'CARICOM', 'EU', 'CAN', 'ACP', 'Landlocked', 'AOSIS', 'SIDS',
       'Islands', 'LDC',], 1)

    df = utils.add_time_to_df(country_df, 'ID_0', years, [1]).drop(['Month', 'datetime'], 1)
    df = df.rename(columns={'NAME_0': 'Name'})
    df = df.to_crs('EPSG:4326')
    df = df.reset_index()

    print("Initial country dataset created.\n")

    # Add flood data (annual)
    flood_years = np.arange(2014, 2020)

    ## Calculate flood extent as percentage of national area
    for i, file in enumerate(flood_files):
        mask_utils.raster_calc_annual(df, flood_dir + file, flood_years[i], mask_utils.prop_flood,
                                   'flood_extent', all_touched = True)

    ## Calculate mean flood duration in days for a given country year/country
    for i, file in enumerate(flood_files):
        mask_utils.raster_calc_annual(df, flood_dir + file, flood_years[i], mask_utils.mean_flood_duration,
                                   'mean_flood_duration', all_touched = True)

    print("Flood data added successfully.\n")

    # Add IPC data
    ## This is not annual, so values are binned by number of months from October of each year
    ## i.e., the end of the rainy season in most of these countries.
    ##       Time 0 = 0-1 months after (months 10-11)
    ##       Time 1 = 2-4 months after (months 12-2)
    ##       Time 2 = 5-7 months after (months 3-5)
    ##       Time 3 = 8-9 months after (months 6-7)

    cleaned_ipc_files, ipc_years = utils.ipc_prep(ipc_files)

    ipc_cols = ['mean_ipc_{}'.format(i) for i in range(4)]
    df[ipc_cols] = None

    for i, file_set in enumerate(cleaned_ipc_files):
        for j, file in enumerate(file_set):
            mask_utils.raster_calc_annual(df, ipc_dir + file, ipc_years[i][j], np.ma.mean, 'mean_ipc_{}'.format(i), all_touched = True)

    print("IPC data added successfully.\n")

    # Add price data
    ## This needs to line up properly with the IPC data, meaning the data needs to precede the publishing of the IPC data
    ## Using the same 0-3 time stamp as for IPC data, we have:
    ##      Time 0 = months 6-9
    ##      Time 1 = months 10-12
    ##      Time 2 = months 1-3
    ##      Time 3 = months 4-5

    # Using 'price_usd_index_normalized_2015' data from Charles Taylor's 'price_food_country_staples_index' dataset
    df = utils.build_price_feats(df, price_file, 'price_usd_index_normalized_2015')

    print("Price data added successfully.\n")

    # Merge displacement data
    displacement_df = pd.read_csv(displacement_file, index_col = 'ID')

    df = df.merge(displacement_df, on = ['Name', 'Year'], how = 'outer')

    print("Displacement data merged successfully.")

    df.to_file(os.path.join(data_dir, 'flood_ipc_covariates.gpkg'), driver = 'GPKG')

    print("Geopackage written: available at {}flood_ipc_covariates.gpkg\n".format(data_dir))

if __name__ == '__main__':
    main()
