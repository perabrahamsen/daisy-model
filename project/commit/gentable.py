#! /usr/bin/env python3

import sys
sys.path.append('/home/xvs108/PyDaisy')

import numpy as np
import pandas as pd
from pydaisy.Daisy import *
import datetime

# Harvest file may contain entries from the warmup period.
start_sim = datetime.datetime(3001, 1, 1)

def parse_harvest (scn, crop):
    dlf = DaisyDlf(f"scn/{scn}/harvest.dlf")
    data = dlf.Data[start_sim:]
    data = data[data["crop"]==crop]
    N = data["sorg_N"].mean ()
    DM_mean = data["sorg_DM"].mean ()
    DM_median = data["sorg_DM"].median ()
    DM_10 = data["sorg_DM"].quantile (0.1)
    WStress = data["WStress"].mean ()
    NStress = data["WStress"].mean ()
    return [N, DM_mean, DM_median, DM_10, WStress, NStress]

harvest_names =  ['N', 'DM_mean', 'DM_median', 'DM_10', 'WStress', 'NStress']

def parse_nitrogen (scn):
    dlf = DaisyDlf(f"scn/{scn}/field_nitrogen.dlf")
    data = dlf.Data[start_sim:]

    MinSurfaceFertilizer = data["Min-Surface-Fertilizer"].mean ()
    MinSoilFertilizer = data["Min-Soil-Fertilizer"].mean ()
    OrgFertilizer = data["Org-Fertilizer"].mean ()
    Deposition = data["Deposition"].mean ()
    Fixated = data["Fixated"].mean ()
    Seed = data["Seed"].mean ()
    
    inp = (MinSurfaceFertilizer +
           MinSoilFertilizer +
           OrgFertilizer +
           Deposition +
           Fixated +
           Seed)
    
    N2ONitrification = data["N2O-Nitrification"].mean ()
    Denitrification = data["Denitrification"].mean ()
    MatrixLeaching = data["Matrix-Leaching"].mean ()
    BioporeLeaching = data["Biopore-Leaching"].mean ()
    SoilDrain = data["Soil-Drain"].mean ()
    BioporeDrain = data["Biopore-Drain"].mean ()
    SurfaceLoss = data["Surface-Loss"].mean ()
    Harvest = data["Harvest"].mean ()

    out = (N2ONitrification +
           Denitrification +
           MatrixLeaching +
           BioporeLeaching +
           SoilDrain +
           BioporeDrain +
           SurfaceLoss +
           Harvest)

    Soil = out - inp

    Air = N2ONitrification + Denitrification
    Drain = SoilDrain + BioporeDrain
    Deep = MatrixLeaching + BioporeLeaching

    return [Air, Drain, Deep, Soil]

nitrogen_names = ['Air', 'Drain', 'Deep', 'Soil']

def gen_row ():
    for climate in ["CPH", "NF"]:
        for crop in ["SB", "CC", "WW"]:
            for block in ["B2", "B3"]:
                for treatment in ["T0", "T6", "T6R10", "T6R1"]:
                    meta = [climate, crop, treatment, block]
                    clm = "" if climate == "CPH" else "NF"
                    scn = f"{clm}{crop}{block}{treatment}"
                    harvest = parse_harvest (scn, "SB" if crop == "CC" else crop)
                    nitrogen = parse_nitrogen (scn)
                    yield meta + harvest + nitrogen

meta_names = ['climate', 'crop', 'treatment', 'block']
column_names = meta_names + harvest_names + nitrogen_names

table = pd.DataFrame (gen_row (), columns=column_names)
table.to_csv ("table.csv", index=False)
print ("Done")

