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

def plot_fig (fig):
    if suffix:
        plt.savefig (f"fig/{fig}.{suffix}")
        plt.clf ()
    else:
        plt.show ()

def plot_gw_log (file, c, label):
    dlf = DaisyDlf(file)
    data = dlf.Data
    plt.plot (data["table_low"], '-', c=c, label=label)

def plot_gw ():
    plt.figure ()
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
    plot_fig ("GW")

crop_name = { 1: "no cover crop", 2: "cover crop" }

crop_xlim = { 1: [2009.5, 2019.5],
              2: [2013.5, 2019.5] }

crop_short = { 1: "NOCC", 2: "CC" }

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
    plt.figure ()
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
    plt.xlim (crop_xlim[crop])
    #plt.ylim ([-200, 0])
    plt.ylabel ('hkg ww grain/ha')
    plt.legend (loc="lower left")
    title = "Grain wet weight " + crop_name[crop] + " " + treatment_name[treatment] + " (" + treatment + ")"
    plt.title (title)
    CC = crop_short[crop]
    plot_fig (f"WW_T{treatment}_{CC}.{suffix}")

def plot_ww_by_b (crop, block):
    plt.figure ()
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
    plt.xlim (crop_xlim[crop])
    #plt.ylim ([-200, 0])
    plt.ylabel ('hkg ww grain/ha')
    plt.legend (loc="lower left")
    title = "Grain wet weight " + crop_name[crop] + f" B{block}"
    plt.title (title)
    CC = crop_short[crop]
    plot_fig (f"WW_B{block}_{CC}")

def plot_DM (crop):
    plt.figure ()
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
    plt.xlim (crop_xlim[crop])
    #plt.ylim ([-200, 0])
    plt.ylabel ('hkg DM grain/ha')
    plt.legend (loc="lower left")
    title = "Grain dry matter " + crop_name[crop] + " field"
    plt.title (title)
    CC = crop_short[crop]
    plot_fig (f"DM_{CC}")

def plot_N (crop):
    plt.figure ()
    data = pd.read_csv ('data/Harvest_DM_N.csv', sep=';')
    cd = data[(data["F1"] == crop)]
    width = 1.0 / 9.5
    column = "kg N grain/ha"
    for t in ['A', 'B', 'C', 'D']:
        ct = data[(data["F1"] == crop) & (data["F2"] == t)]
        color = treatment_color[t]
        offset = treatment_offset[t]
        year= ct["Year"]
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
    plt.xlim (crop_xlim[crop])
    #plt.ylim ([-200, 0])
    plt.ylabel ('kg N grain/ha')
    plt.legend (loc="lower left")
    title = "Grain N " + crop_name[crop] + " field"
    plt.title (title)
    CC = crop_short[crop]
    plot_fig (f"N_{CC}")

def plot_straw (crop):
    plt.figure ()
    data = pd.read_csv ('data/Harvest_DM_N.csv', sep=';')
    cd = data[(data["F1"] == crop)]
    width = 1.0 / 9.5
    column = "hkg straw/ha"
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
                     (data["leaf_DM"] + data["stem_DM"]) * 10, width=width,
                     label=f"{t}{b} Daisy DM", color=color, hatch=hatch)
    plt.xlabel ('Year')
    plt.xlim (crop_xlim[crop])
    #plt.ylim ([-200, 0])
    plt.ylabel ('hkg straw/ha')
    plt.legend (loc="lower left")
    title = "Straw " + crop_name[crop] + " field"
    plt.title (title)
    CC = crop_short[crop]
    plot_fig (f"straw_{CC}")

ww_N2protein = 5.7
sb_N2protein = 6.25

def gen_p_by_t (treatment, crop, data):
    for b in range (1, 5):
        color = block_color[b]
        label = f"B{b}"
        entry = data[(data["F2"]==treatment)&(data["F1"]==crop)]
        value = float("nan") if len (entry) == 0 else entry[f"BLOK{b}"].array[0]
        yield (color, label, value)
        t = treatment_value[treatment]
        cc = "" if crop == 1 else "CC"
        harvest_file = f"log/B{b}T{t}{cc}/harvest.dlf"
        label = f"B{b}SIM"
        if (not os.path.isfile (harvest_file)):
            yield (color, label, 0)
            continue
        file = f"log/B{b}T{t}/harvest.dlf"
        dlf = DaisyDlf(file)
        dat = dlf.Data
        year = 2019
        entry = dat[(dat.index.year==year)&(dat["crop"]=="WW")]
        DM = entry["sorg_DM"].array[0]
        N = entry["sorg_N"].array[0]
        yield (color, label, ww_N2protein * N / DM / 10)

def plot_protein_by_treatment (treatment, crop):
    tt = treatment_name[treatment]
    cc = crop_name[crop]
    data = pd.read_csv ('data/Harvest_blok_protein_percent.csv', sep=';')
    (colors, labels, values) = zip (*gen_p_by_t (treatment, crop, data))
    
    plt.title (f"{tt}, {cc}")
    plt.bar (labels, values, color=colors)

def plot_protein_treatments (crop):
    fig, axs = plt.subplots (2, 2, sharex='all', sharey='all')
    #fig, axs = plt.subplots (2, 2)
    plt.suptitle ("Grain protein content, " + crop_name[crop])

    plt.sca(axs[0, 0])
    plt.ylabel ('%')
    plot_protein_by_treatment ('A', crop)
    plt.sca(axs[0, 1])
    plot_protein_by_treatment ('B', crop)
    plt.sca(axs[1, 0])
    plt.ylabel ('%')
    plot_protein_by_treatment ('C', crop)
    plt.sca(axs[1, 1])
    plot_protein_by_treatment ('D', crop)

    CC = crop_short[crop]
    plot_fig (f"protein_T_{CC}")

def gen_p_by_b (b, crop, data):
    color = block_color[b]
    for treatment in ['A', 'B', 'C', 'D']:
        t = treatment_value[treatment]
        label = f"T{t}"
        entry = data[(data["F2"]==treatment)&(data["F1"]==crop)]
        value = float("nan") if len (entry) == 0 else entry[f"BLOK{b}"].array[0]
        yield (color, label, value)
        cc = "" if crop == 1 else "CC"
        harvest_file = f"log/B{b}T{t}{cc}/harvest.dlf"
        label = f"T{t}SIM"
        if (not os.path.isfile (harvest_file)):
            yield (color, label, 0)
            continue
        file = f"log/B{b}T{t}/harvest.dlf"
        dlf = DaisyDlf(file)
        dat = dlf.Data
        year = 2019
        entry = dat[(dat.index.year==year)&(dat["crop"]=="WW")]
        DM = entry["sorg_DM"].array[0]
        N = entry["sorg_N"].array[0]
        yield (color, label, ww_N2protein * N / DM / 10)

def plot_protein_by_block (b, crop):
    #plt.figure ()
    cc = crop_name[crop]
    data = pd.read_csv ('data/Harvest_blok_protein_percent.csv', sep=';')
    (colors, labels, values) = zip (*gen_p_by_b (b, crop, data))
    
    plt.title (f"Block {b}")
    plt.bar (labels, values, color=colors)
    #CC = crop_short[crop]
    #plot_fig (f"protein_B{b}_{CC}")

def plot_protein_blocks (crop):
    fig, axs = plt.subplots (2, 2, sharex='all', sharey='all')
    #fig, axs = plt.subplots (2, 2)
    plt.suptitle ("Grain protein content, " + crop_name[crop])

    plt.sca(axs[0, 0])
    plt.ylabel ('%')
    plot_protein_by_block (1, crop)
    plt.sca(axs[0, 1])
    plot_protein_by_block (2, crop)
    plt.sca(axs[1, 0])
    plt.ylabel ('%')
    plot_protein_by_block (3, crop)
    plt.sca(axs[1, 1])
    plot_protein_by_block (4, crop)

    CC = crop_short[crop]
    plot_fig (f"protein_B_{CC}")

#suffix = None
#plot_protein_blocks (1)

def plot_all ():
    print ("Plotting...")
    print ("Plotting...GW")
    plot_gw ()
    for crop in [1, 2]:
        print ("Plotting...WW")
        plot_ww_by_t (crop=crop,treatment='A')
        plot_ww_by_t (crop=crop,treatment='C')
    
        plot_ww_by_b (crop=crop,block=2)
        plot_ww_by_b (crop=crop,block=3)

        
        print ("Plotting...harvest")
        plot_DM (crop=crop)
        plot_N (crop=crop)
        plot_straw (crop=crop)
    
        print ("Plotting...protein")
        plot_protein_treatments (crop)
        plot_protein_blocks (crop)

    print ("Plotting...done")

suffix = "svg"

plot_all ()
