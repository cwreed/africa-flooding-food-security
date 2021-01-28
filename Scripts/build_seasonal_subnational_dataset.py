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

data_dir = '../Data/'
shp_file = os.path.join(data_dir, 'IPC/LZ_adm2_geometryFix.gpkg')

ipc_dir = os.path.join(data_dir, 'IPC/updated_files/')
flood_file = os.path.join(data_dir, 'DFO flood extents/FloodsArchived_shp/floods_with_dambreaks.shp')
country_names = sorted(['Chad', 'Mali', 'Niger', 'Nigeria', 'Mauritania',
                        'Burkina Faso', 'Sudan', 'South Sudan',
                        'Ethiopia', 'Uganda', 'Kenya', 'Somalia', 'Zambia',
                        'Zimbabwe', 'Mozambique', 'Malawi'])

ipc_files = sorted([i for i in os.listdir(ipc_dir) if re.match(r'^.*.tif$', i)])
ipc_timestamps = [fname.split('_')[1] for fname in ipc_files]
ipc_year_month = [(int(ym[:4]), int(ym[4:])) for ym in ipc_timestamps]

def main():

    places_df = gpd.read_file(shp_file)
    places_df = places_df[places_df['ADMIN0'].isin(country_names)]

    df = utils.add_time_to_df_ipc(places_df, 'FIDcalc', ipc_year_month)
    df = df.rename(columns={'ADMIN0': 'Name'})
    df = df.to_crs('EPSG:4326')
    df = df.reset_index()

    print("Initial locations dataset created.\n")

    # Append flood data from DFO polygons
    dfo_df = gpd.read_file(flood_file)

    df = utils.build_seasonal_flood_poly_feats(df, dfo_df, country_names)

    print("Flood data added successfully.\n")

    # Add Mean IPC rating data
    df['mean_ipc'] = None

    for i, file in enumerate(ipc_files):
        mask_utils.raster_calc_seasonal(df, os.path.join(ipc_dir, file), ipc_year_month[i][0], ipc_year_month[i][1],
        mask_utils.mean_ipc, 'mean_ipc', all_touched = True)

    print("IPC data added successfully.\n")

    df['mean_ipc'] = df['mean_ipc'].astype(float)

    df.to_file(os.path.join(data_dir, 'flood_ipc_seasonal_subnational.gpkg'), driver = 'GPKG')

    print("Geopackage written: available at {}flood_ipc_seasonal_subnational.gpkg\n".format(data_dir))

if __name__ == '__main__':
    main()
