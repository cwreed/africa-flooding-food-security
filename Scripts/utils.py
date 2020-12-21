import numpy as np
import pandas as pd
import geopandas as gpd
import os
import re
import itertools as it

def add_time_to_df(geodf, geo_ID_col, years, months):
    IDs = list(geodf[geo_ID_col])

    time = list(it.product(IDs, years, months))
    inter_df = pd.DataFrame(time, columns = [geo_ID_col, 'Year', 'Month']).set_index(geo_ID_col, drop = True)
    inter_df['datetime'] = pd.to_datetime(['{}-{}-01'.format(y,m) for y, m in list(zip(inter_df.Year, inter_df.Month))])

    new_df = geodf.join(inter_df, on = geo_ID_col)
    return(new_df)

def ipc_prep(ipc_files):

    ipc_0 = [i for i in ipc_files if re.match(r'^.*\d\d\d\d10.*$', i)]
    ipc_1 = [i for i in ipc_files if re.match(r'^.*\d\d\d\d((01)|(12)|(02)).*$', i)]
    ipc_2 = [i for i in ipc_files if re.match(r'^.*\d\d\d\d(04).*$', i)]
    ipc_3 = [i for i in ipc_files if re.match(r'^.*\d\d\d\d((06)|(07)).*$', i)]

    ipc_2.append(ipc_1[-1])
    ipc_1.pop()

    ipc_0_years = [int(fname.split('_')[1][:4]) for fname in ipc_0]
    ipc_1_years = np.copy(ipc_0_years)
    ipc_2_years = [2014, 2018]
    ipc_3_years = [2014, 2015, 2016, 2017]

    ipc_file_list = [ipc_0, ipc_1, ipc_2, ipc_3]
    ipc_years = [ipc_0_years, ipc_1_years, ipc_2_years, ipc_3_years]

    return(ipc_file_list, ipc_years)

def build_price_feats(df, price_data_file, price_col):
    df_copy = df.copy()

    price_data = pd.read_csv(price_data_file)
    price_months = [np.arange(6,10), np.arange(10,13), np.arange(1,4), np.arange(4,6)]

    price_cols = ['mean_staple_price_{}'.format(i) for i in range(4)]
    df_copy[price_cols] = None

    for i, row in df.iterrows():
        country = row['Name']
        year = row['Year']

        price_sub = price_data.loc[(price_data.country == country) & (price_data.year == year)]
        mean_prices = []

        for j in range(4):
            mean_prices.append(price_sub.loc[(price_sub.month.isin(price_months[j]))][price_col].mean())

        df_copy.loc[(df_copy.Name == country) & (df_copy.Year == year), price_cols] = mean_prices

    return(df_copy)
