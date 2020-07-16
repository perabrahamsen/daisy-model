#! /usr/bin/env python3

import sys
sys.path.append('/home/xvs108/PyDaisy')

import numpy as np
import pandas as pd
from pydaisy.Daisy import *
import datetime

table = pd.read_csv ("table.csv")

def gen_fields (table, climate, block, crop):
    for field in ["N", "DM_mean", "DM_median", "DM_10"]:
        narrow = table[(table["climate"]==climate)
                       & (table["block"]==block)
                       & (table["crop"]==crop)]
        T0 = narrow[narrow["treatment"]=="T0"][field].values[0]
        T6 = narrow[narrow["treatment"]=="T6"][field].values[0]
        T6R10 = narrow[narrow["treatment"]=="T6R10"][field].values[0]
        yield [T0, 100 * T6 / T0, 100 * T6R10 / T0]

def gen_rows (table):
    for climate in ["CPH", "NF"]:
        for block in ["B3", "B2"]:
            for crop in ["SB", "CC", "WW"]:
                row = list (gen_fields (table, climate, block, crop))
                foo = list (zip (*row))
                bar = [climate, block, crop] + np.concatenate (foo).tolist ()
                yield (bar)

harvest = pd.DataFrame (list (gen_rows (table)), columns=[
    'climate', 'block', 'crop',
    "N", "DM_mean", "DM_median", "DM_10",
    "N_T6", "DM_mean_T6", "DM_median_T6", "DM_10_T6",
    "N_T6R10", "DM_mean_T6R10", "DM_median_T6R10", "DM_10_T6R10"])
harvest.to_csv ("yield.csv", index=False)

print ("Done")
