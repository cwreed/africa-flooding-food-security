import numpy as np
import pandas as pd
import geopandas as gpd
import os
import re
import itertools as it
from datetime import datetime, timedelta
from typing import Tuple

def add_time_to_df(
    geodf: gpd.GeoDataFrame, 
    geo_ID_col: str,
    years: list, 
    months: list) -> gpd.GeoDataFrame:
    """Adds datetime column to GeoDataFrame based on provided years and months"""
    IDs = list(geodf[geo_ID_col])
    time = list(it.product(IDs, years, months))
    inter_df = pd.DataFrame(time, columns = [geo_ID_col, 'Year', 'Month']).set_index(geo_ID_col, drop = True)
    inter_df['datetime'] = pd.to_datetime(['{}-{}-01'.format(y,m) for y, m in list(zip(inter_df.Year, inter_df.Month))])

    new_df = geodf.join(inter_df, on = geo_ID_col)
    return new_df

def add_time_to_df_ipc(
    geodf: gpd.GeoDataFrame, 
    geo_ID_col: str, 
    ipc_time_tup: list) -> gpd.GeoDataFrame:
    """Adds datetime start and end columns to each row of GeoDataFrame based on IPC reporting periods."""
    IDs = list(geodf[geo_ID_col])

    time = list(it.product(IDs, ipc_time_tup))
    time2 = [(x, *y) for x,y in time]
    inter_df = pd.DataFrame(time2, columns = [geo_ID_col, 'Year', 'Month']).set_index(geo_ID_col, drop = True)
    inter_df['datetime_end'] = pd.to_datetime(['{}-{}-01'.format(y,m) for y, m in list(zip(inter_df.Year, inter_df.Month))])
    inter_df['datetime_start'] = inter_df['datetime_end'].shift(1, fill_value = datetime(2009, 3 ,31)) + timedelta(days = 1)

    inter_df['datetime_start'] = inter_df['datetime_start'].dt.date
    inter_df['datetime_end'] = inter_df['datetime_end'].dt.date

    new_df = geodf.join(inter_df, on = geo_ID_col)
    del inter_df
    return new_df

def ipc_prep(ipc_files: list) -> Tuple[list, list]:
    """Organize IPC files based on season as encoded in filename"""
    ipc_0 = [i for i in ipc_files if re.match(r'^.*\d\d\d\d10.*$', i)]
    ipc_1 = [i for i in ipc_files if re.match(r'^.*\d\d\d\d((01)|(12)|(02)).*$', i)]
    ipc_2 = [i for i in ipc_files if re.match(r'^.*\d\d\d\d(04).*$', i)]
    ipc_3 = [i for i in ipc_files if re.match(r'^.*\d\d\d\d((06)|(07)).*$', i)]

    ipc_2.append(ipc_1[-1])
    ipc_1.pop()

    ipc_0_years = [int(fname.split('_')[1][:4]) for fname in ipc_0]
    ipc_1_years = [int(fname.split('_')[1][:4]) for fname in ipc_1]
    ipc_2_years = [int(fname.split('_')[1][:4]) for fname in ipc_2]
    ipc_3_years = [int(fname.split('_')[1][:4]) for fname in ipc_3]

    ipc_file_list = [ipc_0, ipc_1, ipc_2, ipc_3]
    ipc_years = [ipc_0_years, ipc_1_years, ipc_2_years, ipc_3_years]

    return ipc_file_list, ipc_years

# def find_dam_breaks(row):
#     if row['MainCause'] is not None:
#         if 'Dam' in row['MainCause']:
#             return(1)
#         else:
#             return(0)
#     else:
#         return(0)

def build_flood_poly_feats(
    df: gpd.GeoDataFrame, 
    dfo_df: gpd.GeoDataFrame, 
    country_names: list) -> gpd.GeoDataFrame:
    """Helper function to build features from flood polygons"""
    dfo_df = dfo_df[dfo_df.Country.isin(country_names)]

    dfo_df['Began'] = pd.to_datetime(dfo_df['Began'])
    dfo_df['Ended'] = pd.to_datetime(dfo_df['Ended'])
    dfo_df = dfo_df.loc[(dfo_df.Began >= datetime(2009, 1, 1))]

    df = df.to_crs('esri:102022')
    dfo_df = dfo_df.to_crs('esri:102022')

    new_dfs = []
    unique_dates = list(zip(df['datetime_start'].unique(), df['datetime_end'].unique()))

    for i, dates in enumerate(unique_dates):
        print(dates)
        df_select = df.loc[(df['datetime_start'] == dates[0]) & (df['datetime_end'] == dates[1])].copy().reset_index()
        floods_select = dfo_df.loc[((dfo_df['Began'] > dates[0]) &
                                        (dfo_df['Began'] < dates[1])) |
                                        ((dfo_df['Ended'] > dates[0]) &
                                        (dfo_df['Ended'] < dates[1]))].reset_index()

        df_select['no_flood_events'] = 0
        df_select['no_dam_breaks'] = 0

        for j, geom_place in enumerate(df_select.geometry):

            flood_indices = []
            total_displaced_dfo = 0

            for k, geom_flood in enumerate(floods_select.geometry):
                if geom_place.buffer(0).intersects(geom_flood.buffer(0)):
                    #print("Bing")

                    df_select.loc[j, 'no_flood_events'] += 1
                    df_select.loc[j, 'no_dam_breaks'] += floods_select.loc[k, 'Dam break']

                    total_displaced_dfo += floods_select.iloc[k].loc['Displaced']
                    flood_indices.append(k)


            floods_select2 = floods_select.iloc[flood_indices]
            began = floods_select2.Began.copy()
            ended = floods_select2.Ended.copy()

            date_ranges = sorted(list(zip(began, ended)))

            for i in range(len(date_ranges) - 1):
                if date_ranges[i][1] > date_ranges[i+1][0]:
                    began.iloc[i+1] = ended.iloc[i]


            if date_ranges:
                df_select.loc[j, 'total_flood_dur_days'] = np.sum(ended-began).days
                df_select.loc[j, 'flood_1_7_days'] = np.count_nonzero((ended - began).dt.days <= 7)
                df_select.loc[j, 'flood_8_14_days'] = np.count_nonzero(((ended - began).dt.days > 7) & ((ended - began).dt.days <= 14))
                df_select.loc[j, 'flood_15_21_days'] = np.count_nonzero(((ended - began).dt.days > 14) & ((ended - began).dt.days <= 21))
                df_select.loc[j, 'flood_22_28_days'] = np.count_nonzero(((ended - began).dt.days > 21) & ((ended - began).dt.days <= 28))
                df_select.loc[j, 'flood_29_plus_days'] = np.count_nonzero((ended - began).dt.days > 28)
            else:
                df_select.loc[j, 'total_flood_dur_days'] = 0
                df_select.loc[j, 'flood_1_7_days'] = 0
                df_select.loc[j, 'flood_8_14_days'] = 0
                df_select.loc[j, 'flood_15_21_days'] = 0
                df_select.loc[j, 'flood_22_28_days'] = 0
                df_select.loc[j, 'flood_29_plus_days'] = 0

            try:
                df_select.loc[j, 'total_flood_area_km2'] = floods_select2.buffer(0).unary_union.intersection(geom_place.buffer(0)).area/1e06
                df_select.loc[j, 'total_flood_area_std'] = floods_select2.buffer(0).unary_union.intersection(geom_place.buffer(0)).area / geom_place.buffer(0).area
            except ValueError:
                df_select.loc[j, 'total_flood_area_km2'] = 0

            df_select.loc[j, 'total_displaced_dfo'] = total_displaced_dfo

        new_dfs.append(df_select)
        del df_select

    df_all = pd.concat(new_dfs, ignore_index = True)
    return(df_all.to_crs('epsg:4326'))