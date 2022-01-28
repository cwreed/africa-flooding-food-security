import numpy as np
import pandas as pd
import geopandas as gpd
import os
import rasterio as rio
import mask_utils
import utils

project_dir = Path(__file__).resolve().parents[1]
data_dir = os.path.join(project_dir, "data")

shp_file = os.path.join(data_dir, 'IPC', 'LZ_adm2_geometryFix.gpkg')
pop_file = os.path.join(data_dir, 'ppp_2020_1km_Aggregated.tif') # Source https://data.worldpop.org/GIS/Population/Global_2000_2020/2020/0_Mosaicked/ppp_2020_1km_Aggregated.tif

def main():
    df = gpd.read_file(shp_file, driver = 'GPKG')

    with rio.open(pop_file) as file:
        df['2020_pop'] = df.geometry.apply(mask_utils.clean_mask, all_touched = True, dataset = file)\
                                    .apply(np.ma.sum)

    df.to_file(os.path.join(data_dir, "IPC/LZ_adm2_geometryFix_population.gpkg"))

if __name__ == "__main__":
    main()
