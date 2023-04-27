#! /usr/bin/env python3

import numpy as np
import pandas as pd

climate_order = ["PRESENT", "WARMER", "WETWARM"]
crop_order = ["SB", "SBI", "SBIJB1", "SBIJB4", "WW", "WWI", "WWIJB1", "WWIJB4"]
soil_order = ["CONTROL", "BIO60", "BIO70", "BIO80", "BIO90", "BIO100",
              "RZ", "LESS", "JB4", "HIGH"]

def read_file (tag, fil, entries, result):
    print ("Reading", tag)
    data = pd.read_csv (fil)

    #Sorting
    data["climate_order"] = data["Climate"].apply (lambda x: climate_order.index (x) * 10000)
    data["crop_order"] = data["Crop"].apply (lambda x: crop_order.index (x) * 100)
    data["soil_order"] = data["Soil"].apply (lambda x: soil_order.index (x))
    data["Order"] = data["climate_order"]+ data["crop_order"]+ data["soil_order"]
    data.sort_values ("Order", inplace=True)
    data.index = data["Climate"] + "-" + data["Crop"] + "-" + data["Soil"];
    
    # Narrow
    table = data[(data["Climate"]=="PRESENT") &
                 (data["Crop"].isin (["SB", "SBI", "WW", "WWI"])) &
                 (data["Soil"].isin(["CONTROL", "BIO60", "BIO80", "BIO100"]))]

    # Extracting
    for e in entries:
        result[tag + " " + e + " AVG"] = table[(table["What"]=="Average")][e]
        result[tag + " " + e + " SD"] = table[(table["What"]=="STDEV")][e]
        result[tag + " " + e + " SE"] = table[(table["What"]=="STERR")][e]
        
sum = pd.DataFrame ()
read_file ("Yield", "sum_yield.csv",
           ["DM [Mg/ha]", "N [kg/ha]"], sum);
read_file ("Nitrogen", "sum_field_nitrogen.csv",
           ["Matrix-Leaching [kg N/ha]"], sum);
read_file ("Water 4-7", "sum_water_usage_4-7.csv", ["ETa [mm]"], sum);
read_file ("Water 5-7", "sum_water_usage_5-7.csv", ["ETa [mm]"], sum);

print ("Writing")

sum.to_csv ("cpe.csv")

print ("Done")
