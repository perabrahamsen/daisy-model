#! /usr/bin/env python3

import sys
sys.path.append('/home/xvs108/PyDaisy')

import numpy as np
import pandas as pd
from matplotlib import pyplot as plt
from matplotlib import dates as md
from matplotlib import gridspec

from pydaisy.Daisy import *

import datetime

def plot_fig (fig):
    if suffix:
        plt.savefig (f"fig/{fig}.{suffix}", dpi=600)
        plt.clf ()
    else:
        plt.show ()

def plot_gw_log (file, c, label):
    dlf = DaisyDlf(file)
    data = dlf.Data
    plt.plot (data["table_low"], ':', c=c, label=label)

def plot_gw ():
    plt.figure ()
    gw_man_dlf = DaisyDlf(f"data/COMMITManual.ddf")
    man = gw_man_dlf.Data
    plt.xlabel ('Year')
    plt.xlim ([datetime.date(2017, 10, 1), datetime.date(2018, 10, 1)])
    plt.ylim ([-200, 0])
    plt.ylabel ('cm')
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
              'b-', label="B2 obs")
    plt.plot (auto["Block3_groundwater_level"].multiply (0.1),
              'r-', label="B3 obs")

    plot_gw_log ("log/B2T0/groundwater.dlf", c="b", label="B2T0 sim")
    plot_gw_log ("log/B3T0/groundwater.dlf", c="r", label="B3T0 sim")
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

block_color = { 1: "palegreen",
                2: "blue",
                3: "red",
                4: "yellow" }

block_hatch = { 2: "/",
                3: "\\" }

treatment_color = { 'A': "blue",
                    'B': "yellow",
                    'C': "red",
                    'D': "palegreen" }

treatment_hatch = { 'A': "/",
                    'C': "\\" }

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
    cc = "" if crop == 1 else "CC"
    for b in [1, 2, 3, 4]:
        label = f"B{b}"
        color = block_color[b]
        plt.bar (year - 0.5 + b * width, ct[label], width=width,
                 label=label + " obs", color=color)
    t = treatment_value[treatment]
    for b in [2, 3]:
        label = f"B{b}"
        file = f"log/B{b}T{t}{cc}/harvest.dlf"
        dlf = DaisyDlf(file)
        data = dlf.Data
        plt.bar (data.index.year - 0.5 + (b + 3.5)  * width,
                 data["sorg_DM"] * 10 / dm_fraction, width=width,
                 label=f"{label} sim", color=block_color[b],
                 hatch=block_hatch[b])

def plot_ww_treatment ():
    fig, axs = plt.subplots (2, 2, sharey='all', sharex='col',
                            gridspec_kw={'width_ratios': [7.5, 4.48]})
    plt.sca(axs[0, 0])
    plt.title ("No cover crop")
    plt.ylabel ('hkg w.w./ha (0 Mg)')
    plt.ylim ([0, 125])
    plt.xlim ([2009.5, 2019.5])
    plot_ww_by_t (1, 'A')
    plt.sca(axs[0, 1])
    plt.xlim ([2013.5, 2019.5])
    plt.title ("Cover crop")
    plot_ww_by_t (2, 'A')
    plt.legend (loc='lower left', fontsize='xx-small')
    plt.sca(axs[1, 0])
    plt.ylabel ('hkg w.w./ha (6 Mg)')
    plt.xlabel ('Year')
    plot_ww_by_t (1, 'C')
    plt.sca(axs[1, 1])
    plt.xlabel ('Year')
    plot_ww_by_t (2, 'C')
    fig.tight_layout ()
    plot_fig (f"WW_by_T")

def plot_ww_by_b (crop, block):
    data = pd.read_csv ('data/Harvest-grain-ww.csv', sep=';')
    cd = data[(data["Crop"] == crop)]
    width = 1.0 / 7.5
    dm_fraction = 0.85
    column = f"B{block}"
    cc = "" if crop == 1 else "CC"
    for t in ['A', 'B', 'C', 'D']:
        color = treatment_color[t]
        offset = treatment_offset[t]
        ct=cd[(cd["Treatment"]==t)]
        year = ct["Year"]
        plt.bar (year - 0.5 + offset * width, ct[column], width=width,
                 label=str(treatment_value[t]) + " Mg obs",
                 color=color)
    for t in ['A', 'C']:
        treatment = treatment_value[t]
        file = f"log/B{block}T{treatment}{cc}/harvest.dlf"
        dlf = DaisyDlf(file)
        data = dlf.Data
        offset = treatment_offset_sim[t]
        plt.bar (data.index.year - 0.5 + (offset + 3.5)  * width,
                 data["sorg_DM"] * 10 / dm_fraction, width=width,
                 label=f"{treatment} Mg sim",
                 color=treatment_color[t],
                 hatch=treatment_hatch[t])

def plot_ww_block ():
    fig, axs = plt.subplots (2, 2, sharey='all', sharex='col',
                            gridspec_kw={'width_ratios': [7.5, 4.48]})
    plt.sca(axs[0, 0])
    plt.title ("No cover crop")
    plt.ylabel ('hkg w.w./ha (block 2)')
    plt.ylim ([0, 125])
    plt.xlim ([2009.5, 2019.5])
    plot_ww_by_b (1, 2)
    plt.sca(axs[0, 1])
    plt.xlim ([2013.5, 2019.5])
    plt.title ("Cover crop")
    plot_ww_by_b (2, 2)
    plt.legend (loc='lower left', fontsize='xx-small')
    plt.sca(axs[1, 0])
    plt.ylabel ('hkg w.w./ha (block 3)')
    plt.xlabel ('Year')
    plot_ww_by_b (1, 3)
    plt.sca(axs[1, 1])
    plt.xlabel ('Year')
    plot_ww_by_b (2, 3)
    fig.tight_layout ()
    plot_fig (f"WW_by_B")

def plot_DM (crop):
    data = pd.read_csv ('data/Harvest_DM_N.csv', sep=';')
    cd = data[(data["F1"] == crop)]
    width = 1.0 / 9.5
    column = "hkg DM/ha"
    cc = "" if crop == 1 else "CC"
    for t in ['A', 'B', 'C', 'D']:
        ct = data[(data["F1"] == crop) & (data["F2"] == t)]
        color = treatment_color[t]
        offset = treatment_offset[t]
        year = ct["Year"]
        plt.bar (year - 0.5 + offset * width, ct[column], width=width,
                 label=str (treatment_value[t]) + " Mg obs",
                 color=color)
    for t in ['A', 'C']:
        color = treatment_color[t]
        treatment = treatment_value[t]
        for b in [2, 3]:
            file = f"log/B{b}T{treatment}{cc}/harvest.dlf"
            dlf = DaisyDlf(file)
            data = dlf.Data
            offset = treatment_offset_sim[t] + (b - 2) * 2
            hatch = block_hatch[b]
            plt.bar (data.index.year - 0.5 + (offset + 3.5)  * width,
                     data["sorg_DM"] * 10, width=width,
                     label=str (treatment_value[t]) + " Mg sim B" + str(b),
                     color=color, hatch=hatch)

def plot_N (crop):
    data = pd.read_csv ('data/Harvest_DM_N.csv', sep=';')
    cd = data[(data["F1"] == crop)]
    width = 1.0 / 9.5
    column = "kg N grain/ha"
    cc = "" if crop == 1 else "CC"
    for t in ['A', 'B', 'C', 'D']:
        ct = data[(data["F1"] == crop) & (data["F2"] == t)]
        color = treatment_color[t]
        offset = treatment_offset[t]
        year= ct["Year"]
        plt.bar (year - 0.5 + offset * width, ct[column], width=width,
                 label=str (treatment_value[t]) + " Mg obs",
                 color=color)
    for t in ['A', 'C']:
        color = treatment_color[t]
        treatment = treatment_value[t]
        for b in [2, 3]:
            file = f"log/B{b}T{treatment}{cc}/harvest.dlf"
            dlf = DaisyDlf(file)
            data = dlf.Data
            offset = treatment_offset_sim[t] + (b - 2) * 2
            hatch = block_hatch[b]
            plt.bar (data.index.year - 0.5 + (offset + 3.5)  * width,
                     data["sorg_N"], width=width,
                     label=str (treatment_value[t]) + " Mg sim B" + str(b),
                     color=color, hatch=hatch)

def plot_straw (crop):
    data = pd.read_csv ('data/Harvest_DM_N.csv', sep=';')
    cd = data[(data["F1"] == crop)]
    width = 1.0 / 9.5
    column = "hkg straw/ha"
    cc = "" if crop == 1 else "CC"
    for t in ['A', 'B', 'C', 'D']:
        ct = data[(data["F1"] == crop) & (data["F2"] == t)]
        color = treatment_color[t]
        offset = treatment_offset[t]
        year = ct["Year"]
        plt.bar (year - 0.5 + offset * width, ct[column], width=width,
                 label=str (treatment_value[t]) + " Mg obs",
                 color=color)
    for t in ['A', 'C']:
        color = treatment_color[t]
        treatment = treatment_value[t]
        for b in [2, 3]:
            file = f"log/B{b}T{treatment}{cc}/harvest.dlf"
            dlf = DaisyDlf(file)
            data = dlf.Data
            offset = treatment_offset_sim[t] + (b - 2) * 2
            hatch = block_hatch[b]
            plt.bar (data.index.year - 0.5 + (offset + 3.5)  * width,
                     (data["leaf_DM"] + data["stem_DM"]) * 10, width=width,
                     label=str (treatment_value[t]) + " Mg sim B" + str(b),
                     color=color, hatch=hatch)

def plot_field ():
    fig, axs = plt.subplots (3, 2, sharey='row', sharex='col',
                             figsize=[6.4, 6.8],
                             gridspec_kw={'width_ratios': [7.5, 4.48]})
    plt.sca(axs[0, 0])
    plt.title ("No cover crop")
    plt.ylabel ('kg N grain/ha')
    plt.ylim ([0, 150])
    plt.xlim ([2009.5, 2019.5])
    plot_N (1)
    plt.sca(axs[0, 1])
    plt.xlim ([2013.5, 2019.5])
    plt.title ("Cover crop")
    plot_N (2)
    plt.sca(axs[1, 0])
    plt.ylim ([0, 90])
    plt.ylabel ('hkg DM grain/ha')
    plot_DM (1)
    plt.sca(axs[1, 1])
    plot_DM (2)
    plt.sca(axs[2, 0])
    plt.ylim ([0, 90])
    plt.ylabel ('hkg DM straw/ha')
    plt.xlabel ('Year')
    plot_straw (1)
    plt.legend (loc='upper left', fontsize='small', ncol=2)
    plt.sca(axs[2, 1])
    plt.xlabel ('Year')
    plot_straw (2)
    fig.tight_layout ()
    plot_fig (f"field")

ww_N2protein = 5.7
sb_N2protein = 6.25

def plot_protein_by_treatment (treatment, crop):
    cc = crop_name[crop]
    data = pd.read_csv ('data/Harvest_blok_protein_percent.csv', sep=';')
    for b in range (1, 5):
        color = block_color[b]
        label = f"B{b} obs"
        entry = data[(data["F2"]==treatment)&(data["F1"]==crop)]
        value = float("nan") if len (entry) == 0 else entry[f"BLOK{b}"].array[0]
        plt.bar (label, value, color=color)
        t = treatment_value[treatment]
        cc = "" if crop == 1 else "CC"
        file = f"log/B{b}T{t}{cc}/harvest.dlf"
        label = f"B{b} sim"
        value = 0
        hatch = ' '
        if (os.path.isfile (file)):
            dlf = DaisyDlf(file)
            dat = dlf.Data
            year = 2019
            entry = dat[(dat.index.year==year)&(dat["crop"]=="WW")]
            DM = entry["sorg_DM"].array[0]
            N = entry["sorg_N"].array[0]
            value = ww_N2protein * N / DM / 10
            hatch = hatch=block_hatch[b]
        plt.bar (label, value, color=color, hatch=hatch )

def plot_protein_treatments ():
    fig, axs = plt.subplots (4, 2, sharex='all', sharey='all',
                             figsize=[6.4, 6.8])
    plt.sca(axs[0, 0])
    plt.title ("No cover crop")
    plt.ylabel ('% (0 Mg)')
    plot_protein_by_treatment ('A', 1)
    plt.sca(axs[0, 1])
    plt.title ("Cover crop")
    plot_protein_by_treatment ('A', 2)
    plt.sca(axs[1, 0])
    plt.ylabel ('% (3 Mg)')
    plot_protein_by_treatment ('B', 1)
    plt.sca(axs[1, 1])
    plot_protein_by_treatment ('B', 2)
    plt.sca(axs[2, 0])
    plt.ylabel ('% (6 Mg)')
    plot_protein_by_treatment ('C', 1)
    plt.sca(axs[2, 1])
    plot_protein_by_treatment ('C', 2)
    plt.sca(axs[3, 0])
    plt.xticks (rotation=90)
    plt.ylabel ('% (8 Mg)')
    plot_protein_by_treatment ('D', 1)
    plt.sca(axs[3, 1])
    plt.xticks (rotation=90)
    plot_protein_by_treatment ('D', 2)
    fig.tight_layout ()
    plot_fig (f"protein_by_T")

def plot_protein_by_block (b, crop):
    cc = crop_name[crop]
    data = pd.read_csv ('data/Harvest_blok_protein_percent.csv', sep=';')
    for treatment in ['A', 'B', 'C', 'D']:
        t = treatment_value[treatment]
        color = treatment_color[treatment]
        label = f"{t} Mg obs"
        entry = data[(data["F2"]==treatment)&(data["F1"]==crop)]
        value = float("nan") if len (entry) == 0 else entry[f"BLOK{b}"].array[0]
        plt.bar (label, value, color=color)
        cc = "" if crop == 1 else "CC"
        file = f"log/B{b}T{t}{cc}/harvest.dlf"
        label = f"{t} Mg sim"
        value = 0
        hatch = ' '
        if (os.path.isfile (file)):
            dlf = DaisyDlf(file)
            dat = dlf.Data
            year = 2019
            entry = dat[(dat.index.year==year)&(dat["crop"]=="WW")]
            DM = entry["sorg_DM"].array[0]
            N = entry["sorg_N"].array[0]
            value = ww_N2protein * N / DM / 10
            hatch = hatch=treatment_hatch[treatment]
        plt.bar (label, value, color=color, hatch=hatch )

def plot_protein_blocks ():
    fig, axs = plt.subplots (4, 2, sharex='all', sharey='all',
                             figsize=[6.4, 6.8])
    plt.sca(axs[0, 0])
    plt.title ("No cover crop")
    plt.ylabel ('% (block 1)')
    plot_protein_by_block (1, 1)
    plt.sca(axs[0, 1])
    plt.title ("Cover crop")
    plot_protein_by_block (1, 2)
    plt.sca(axs[1, 0])
    plt.ylabel ('% (block 2)')
    plot_protein_by_block (2, 1)
    plt.sca(axs[1, 1])
    plot_protein_by_block (2, 2)
    plt.sca(axs[2, 0])
    plt.ylabel ('% (block 3)')
    plot_protein_by_block (3, 1)
    plt.sca(axs[2, 1])
    plot_protein_by_block (3, 2)
    plt.sca(axs[3, 0])
    plt.xticks (rotation=90)
    plt.ylabel ('% (block 4)')
    plot_protein_by_block (4, 1)
    plt.sca(axs[3, 1])
    plt.xticks (rotation=90)
    plot_protein_by_block (4, 2)
    fig.tight_layout ()
    plot_fig (f"protein_by_B")

def plot_LAI_year (sim, obs, year):
    plt.gca ().xaxis.set_major_locator (md.DayLocator (1))
    begin = datetime.date (year, 3, 1)
    end = datetime.date (year, 9, 1)
    plt.xlim([begin, end])
    plt.ylim([0, 6])
    plt.title (year)
    if (year % 2 == 0):
        plt.ylabel ("LAI")
    if (year > 2015):
        plt.gca ().xaxis.set_major_formatter (md.DateFormatter ('%m'))
        plt.xlabel ("Month")
    else:
        plt.gca ().xaxis.set_major_formatter (md.DateFormatter (''))
    obs = obs[obs["Date"].dt.year == year]
    sim = sim.loc[begin:end]
    plt.plot (obs["Date"], obs["LAI"], label="RVI")
    plt.plot (sim["LAI"], label="Daisy")
    if (year == 2010):
        plt.legend ()

def plot_LAI ():
    plt.figure ()
    dlf = DaisyDlf("log/B3T0/crop.dlf")
    sim = dlf.Data
    sim = sim[sim["LAI"] > 0.0]
    obs = pd.read_csv ('data/LAI-BBCH.csv', parse_dates=["Date"])
    fig, axs = plt.subplots (4, 2, sharey=True,figsize=(5, 7.5),dpi=60)
    years = range (obs["Date"].dt.year.min (), obs["Date"].dt.year.max () + 1)
    for ax, year in zip (axs.reshape (-1), years):
        plt.sca(ax)
        plot_LAI_year (sim, obs, year)
    fig.tight_layout ()
    plot_fig ("LAI")

def plot_all ():
    print ("Plotting...")
    print ("Plotting...GW")
    plot_gw ()
    print ("Plotting...LAI")
    plot_LAI ()
    print ("Plotting...WW Treatment")
    plot_ww_treatment ()
    print ("Plotting...WW Block")
    plot_ww_block ()
    print ("Plotting...Field")
    plot_field ()
    print ("Plotting...protein Treatment")
    plot_protein_treatments ()
    print ("Plotting...protein Block")
    plot_protein_blocks ()

    print ("Plotting...done")

#suffix = "png"

plot_all ()
#plot_field ()
print ("Done")
