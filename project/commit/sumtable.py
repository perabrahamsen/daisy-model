#! /usr/bin/env python3

import sys
sys.path.append('/home/xvs108/PyDaisy')

import numpy as np
import pandas as pd
from pydaisy.Daisy import *
import datetime

table = pd.read_csv ("table.csv")

def gen_fields (table, climate, block, crop, fields):
    for field in fields:
        narrow = table[(table["climate"]==climate)
                       & (table["block"]==block)
                       & (table["crop"]==crop)]
        T0 = narrow[narrow["treatment"]=="T0"][field].values[0]
        T6 = narrow[narrow["treatment"]=="T6"][field].values[0]
        T6R10 = narrow[narrow["treatment"]=="T6R10"][field].values[0]
        yield [T0, T6 - T0, T6R10 - T0]

def gen_rows (table, fields):
    for climate in ["CPH", "NF"]:
        for block in ["B3", "B2"]:
            for crop in ["SB", "CC", "WW"]:
                row = list (gen_fields (table, climate, block, crop, fields))
                foo = list (zip (*row))
                bar = [climate, block, crop] + np.concatenate (foo).tolist ()
                yield (bar)

harvest_fields = ["N", "DM_mean", "DM_median", "DM_10"]
harvest = pd.DataFrame (list (gen_rows (table, harvest_fields)), columns=[
    'climate', 'block', 'crop',
    "N", "DM_mean", "DM_median", "DM_10",
    "N_T6", "DM_mean_T6", "DM_median_T6", "DM_10_T6",
    "N_T6R10", "DM_mean_T6R10", "DM_median_T6R10", "DM_10_T6R10"])
harvest.to_csv ("yield.csv", index=False)

stress_fields = ["WStress", "NStress"]
stress = pd.DataFrame (list (gen_rows (table, stress_fields)), columns=[
    'climate', 'block', 'crop',
    "WStress", "NStress", 
    "WStress_T6", "NStress_T6", 
    "WStress_T6R10", "NStress_T6R10"])
stress.to_csv ("stress.csv", index=False)

loss_fields = ["Air", "Drain", "Deep", "Soil"]
loss = pd.DataFrame (list (gen_rows (table, loss_fields)), columns=[
    'climate', 'block', 'crop',
    "Air", "Drain", "Deep", "Soil",
    "Air_T6", "Drain_T6", "Deep_T6", "Soil_T6",
    "Air_T6R10", "Drain_T6R10", "Deep_T6R10", "Soil_T6R10"])
loss.to_csv ("loss.csv", index=False)

print ("Done")
