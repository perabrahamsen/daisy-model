#! /usr/bin/env python3

imgext = ".png"
figlib = "/mnt/c/Temp/fig/"
#figlib = "fig/"

import sys
sys.path.append('/home/xvs108/PyDaisy')

import numpy as np
import pandas as pd
from matplotlib import pyplot as plt
from datetime import date

from pydaisy.Daisy import *

def plot_gw_log (file, c, label):
    dlf = DaisyDlf(file)
    data = dlf.Data
    plt.plot (data["table_low"], '-', c=c, label=label)

def plot_gw ():
    gw_man_dlf = DaisyDlf(f"data/COMMITManual.ddf")
    man = gw_man_dlf.Data
    plt.xlabel ('Year')
    plt.xlim ([date(2017, 10, 1), date(2018, 10, 1)])
    plt.ylim ([-200, 0])
    plt.ylabel ('cm')
    plt.title ('gw')
    #plt.plot (man["Block1"].multiply (0.1), 'yx', label="B1 manual")
    #plt.plot (man["Block2"].multiply (0.1),
    #          'bx', label="B2 manual", c="darkblue")
    #plt.plot (man["Block3"].multiply (0.1), 'rx',
    #          label="B3 manual", c="darkred")
    #plt.plot (man["Block4"].multiply (0.1), 'gx',
    #          label="B4 manual")

    gw_auto_dlf = DaisyDlf(f"data/COMMITAuto.ddf")
    auto = gw_auto_dlf.Data

    plt.plot (auto["Block2_groundwater_level"].multiply (0.1),
              'b-', label="B2 auto")
    plt.plot (auto["Block3_groundwater_level"].multiply (0.1),
              'r-', label="B3 auto")

    plot_gw_log ("log/B2T0/groundwater.dlf", c="lavender", label="B2T0 sim")
#    plot_gw_log ("log/B2T6/groundwater.dlf", c="slateblue", label="B2T6 sim")
    plot_gw_log ("log/B3T0/groundwater.dlf", c="mistyrose", label="B3T0 sim")
#    plot_gw_log ("log/B3T6/groundwater.dlf", c="lightcoral", label="B3T6 sim")
    plt.legend ()
    plt.show ()

crop_name = { 1: "no cover crop", 2: "cover crop" }
treatment_name = { 'A': "no compaction",
                   'B': "3 Mg wheel load (2010-2013)",
                   'C': "6 Mg wheel load (2010-2013)",
                   'D':	"8 ton wheel load (2010)" }
treatment_value = { 'A': 0,
                    'B': 3,
                    'C': 6,
                    'D': 8 }

block_color = { 1: "skyblue",
                2: "blue",
                3: "red",
                4: "cyan" }

block_hatch = { 2: "/",
                3: "\\" }

treatment_color = { 'A': "skyblue",
                    'B': "yellow",
                    'C': "red",
                    'D': "blue" }

treatment_offset = { 'A': 1,
                     'B': 2,
                     'C': 3,
                     'D': 4 }

treatment_offset_sim = { 'A': 2,
                         'C': 3 }

def plot_ww_by_t (crop, treatment):
    data = pd.read_csv ('data/Harvest-grain-ww.csv', sep=';')
    ct = data[(data["Crop"] == crop) & (data["Treatment"] == treatment)]
    year = ct["Year"]
    width = 1.0 / 7.5
    dm_fraction = 0.85
    for b in [1, 2, 3, 4]:
        label = f"B{b}"
        color = block_color[b]
        plt.bar (year - 0.5 + b * width, ct[label], width=width, label=label,
                 color=color)
    t = treatment_value[treatment]
    for b in [2, 3]:
        label = f"B{b}"
        color = block_color[b]
        file = f"log/B{b}T{t}/harvest.dlf"
        dlf = DaisyDlf(file)
        data = dlf.Data
        plt.bar (data.index.year - 0.5 + (b + 3.5)  * width,
                 data["sorg_DM"] * 10 / dm_fraction, width=width,
                 label=f"{label} Daisy", color=color, hatch="/")
    plt.xlabel ('Year')
    plt.xlim ([2009, 2019.5])
    #plt.ylim ([-200, 0])
    plt.ylabel ('hkg ww grain/ha')
    plt.legend (loc="lower left")
    title = "Grain wet weight " + crop_name[crop] + " " + treatment_name[treatment] + " (" + treatment + ")"
    plt.title (title)
    plt.show ()

def plot_ww_by_b (crop, block):
    data = pd.read_csv ('data/Harvest-grain-ww.csv', sep=';')
    cd = data[(data["Crop"] == crop)]
    width = 1.0 / 7.5
    dm_fraction = 0.85
    column = f"B{block}"
    for t in ['A', 'B', 'C', 'D']:
        color = treatment_color[t]
        offset = treatment_offset[t]
        ct=cd[(cd["Treatment"]==t)]
        year = ct["Year"]
        plt.bar (year - 0.5 + offset * width, ct[column], width=width, label=t,
                 color=color)
    for t in ['A', 'C']:
        color = treatment_color[t]
        treatment = treatment_value[t]
        file = f"log/B{block}T{treatment}/harvest.dlf"
        dlf = DaisyDlf(file)
        data = dlf.Data
        offset = treatment_offset_sim[t]
        plt.bar (data.index.year - 0.5 + (offset + 3.5)  * width,
                 data["sorg_DM"] * 10 / dm_fraction, width=width,
                 label=f"{treatment} Daisy", color=color, hatch="/")
    plt.xlabel ('Year')
    plt.xlim ([2009, 2019.5])
    #plt.ylim ([-200, 0])
    plt.ylabel ('hkg ww grain/ha')
    plt.legend (loc="lower left")
    title = "Grain wet weight " + crop_name[crop] + f" B{block}"
    plt.title (title)
    plt.show ()

def plot_DM (crop):
    data = pd.read_csv ('data/Harvest_DM_N.csv', sep=';')
    cd = data[(data["F1"] == crop)]
    width = 1.0 / 9.5
    column = "hkg DM/ha"
    for t in ['A', 'B', 'C', 'D']:
        ct = data[(data["F1"] == crop) & (data["F2"] == t)]
        color = treatment_color[t]
        offset = treatment_offset[t]
        year = ct["Year"]
        plt.bar (year - 0.5 + offset * width, ct[column], width=width, label=t,
                 color=color)
    for t in ['A', 'C']:
        color = treatment_color[t]
        treatment = treatment_value[t]
        for b in [2, 3]:
            file = f"log/B{b}T{treatment}/harvest.dlf"
            dlf = DaisyDlf(file)
            data = dlf.Data
            offset = treatment_offset_sim[t] + (b - 2) * 2
            hatch = block_hatch[b]
            plt.bar (data.index.year - 0.5 + (offset + 3.5)  * width,
                     data["sorg_DM"] * 10, width=width,
                     label=f"{t}{b} Daisy", color=color, hatch=hatch)
    plt.xlabel ('Year')
    #plt.xlim ([2009, 2019.5])
    #plt.ylim ([-200, 0])
    plt.ylabel ('hkg DM grain/ha')
    plt.legend (loc="lower left")
    title = "Grain dry matter " + crop_name[crop] + " field"
    plt.title (title)
    plt.show ()

def plot_N (crop):
    data = pd.read_csv ('data/Harvest_DM_N.csv', sep=';')
    cd = data[(data["F1"] == crop)]
    width = 1.0 / 9.5
    column = "kg N grain/ha"
    for t in ['A', 'B', 'C', 'D']:
        ct = data[(data["F1"] == crop) & (data["F2"] == t)]
        color = treatment_color[t]
        offset = treatment_offset[t]
        year = ct["Year"]
        plt.bar (year - 0.5 + offset * width, ct[column], width=width, label=t,
                 color=color)
    for t in ['A', 'C']:
        color = treatment_color[t]
        treatment = treatment_value[t]
        for b in [2, 3]:
            file = f"log/B{b}T{treatment}/harvest.dlf"
            dlf = DaisyDlf(file)
            data = dlf.Data
            offset = treatment_offset_sim[t] + (b - 2) * 2
            hatch = block_hatch[b]
            plt.bar (data.index.year - 0.5 + (offset + 3.5)  * width,
                     data["sorg_N"], width=width,
                     label=f"{t}{b} Daisy", color=color, hatch=hatch)
    plt.xlabel ('Year')
    #plt.xlim ([2009, 2019.5])
    #plt.ylim ([-200, 0])
    plt.ylabel ('kg N grain/ha')
    plt.legend (loc="lower left")
    title = "Grain N " + crop_name[crop] + " field"
    plt.title (title)
    plt.show ()



# plot_gw ()

#plot_ww_by_t (crop=1,treatment='A')
#plot_ww_by_t (crop=1,treatment='C')
#plot_ww_by_b (crop=1,block=2)
#plot_ww_by_b (crop=1,block=3)

#plot_DM (crop=1)
plot_N (crop=1)
