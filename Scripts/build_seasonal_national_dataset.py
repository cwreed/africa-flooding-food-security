import numpy as np
import pandas as pd
import geopandas as gpd
import rasterio as rio
from rasterio import mask
from datetime import datetime
import os
import re
import mask_utils
import utils

"""
    Load files and directories to local data
"""
data_dir = '../Data/'
shp_dir = os.path.join(data_dir, 'ADM shapefiles/')
ipc_dir = os.path.join(data_dir, 'IPC/updated_files/')
flood_file = os.path.join(data_dir, 'DFO flood extents/FloodsArchived_shp/floods_with_dambreaks.shp')
country_iso = sorted(['TCD', 'MLI', 'NER', 'NGA', 'MRT', 'BFA', 'SDN', 'SSD',
                        'ETH', 'UGA', 'KEN', 'SOM', 'ZMB', 'ZWE', 'MOZ', 'MWI'])
country_names = sorted(['Chad', 'Mali', 'Niger', 'Nigeria', 'Mauritania',
                        'Burkina Faso', 'Sudan', 'South Sudan',
                        'Ethiopia', 'Uganda', 'Kenya', 'Somalia', 'Zambia',
                        'Zimbabwe', 'Mozambique', 'Malawi'])
country_shapes = {}

ipc_files = sorted([i for i in os.listdir(ipc_dir) if re.match(r'^.*.tif$', i)])
ipc_timestamps = [fname.split('_')[1] for fname in ipc_files]
ipc_year_month = [(int(ym[:4]), int(ym[4:])) for ym in ipc_timestamps]

def main():

    # Build out GeoDataFrame using all countries of interest
    for c in range(len(country_iso)):
        d = gpd.read_file(os.path.join(shp_dir, '{}_adm/{}_adm0.shp'.format(country_iso[c], country_iso[c])))
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

    country_df.loc[country_df['NAME_0'] == 'South Sudan', 'ID_0'] = 222
    country_df.loc[country_df['NAME_0'] == 'South Sudan', 'ISO'] = 'SSD'

    df = utils.add_time_to_df_ipc(country_df, 'ID_0', ipc_year_month)
    df = df.rename(columns={'NAME_0': 'Name'})
    df = df.to_crs('EPSG:4326')
    df = df.reset_index()

    print("Initial country dataset created.\n")

    # Append flood data from DFO polygons
    dfo_df = gpd.read_file(flood_file)

    df = utils.build_flood_poly_feats(df, dfo_df, country_names)

    print("Flood data added successfully.\n")

    # Add Mean IPC rating data
    df['mean_ipc'] = None

    for i, file in enumerate(ipc_files):
        mask_utils.raster_calc_seasonal(df, os.path.join(ipc_dir, file), ipc_year_month[i][0], ipc_year_month[i][1],
        mask_utils.mean_ipc, 'mean_ipc', all_touched = True)

    print("IPC data added successfully.\n")

    df['mean_ipc'] = df['mean_ipc'].astype(float)

    df.to_file(os.path.join(data_dir, 'flood_ipc_seasonal_national.gpkg'), driver = 'GPKG')

    print("Geopackage written: available at {}flood_ipc_seasonal_national.gpkg\n".format(data_dir))

if __name__ == '__main__':
    main()
