#! /usr/bin/env python3

imgext = ".png"
figlib = "/mnt/c/Temp/fig/"
#figlib = "fig/"

import sys
sys.path.append('/home/xvs108/PyDaisy')

import numpy as np
import pandas as pd
from matplotlib import pyplot as plt

from pydaisy.Daisy import *

def add_sorg (B, T, offset, color):
    dlf = DaisyDlf(f"log/B{B}T{T}/harvest.dlf")
    harvest = dlf.Data
    year = harvest.index.map (lambda x: x.year)
    sorg = harvest.sorg_DM
    plt.bar (year + offset,  sorg, label=f"B{B}T{T}", width=0.2, color=color)

def plot_sim_yield (where=None):
    plt.xlabel ('Year')
    plt.ylabel ('Mg DM/ha')
    plt.title ('Yield (simulated)')
    add_sorg (2, 0, -0.3, "darkred")
    add_sorg (2, 6, -0.1, "red")
    add_sorg (3, 0, +0.1, "darkblue")
    add_sorg (3, 6, +0.3, "blue")
    plt.legend (loc=3)
    if where:
        plt.savefig (figlib + where + imgext)
    else:
        plt.show ()
    plt.clf ()
    
def plot_2018_yield (where=None):
    harvest = pd.read_csv ('data/harvest.csv', parse_dates=['Date'])
    label = harvest.apply (lambda x: str (x.Treatment) + " " + ("CC" if x.Catchcrop else ""), axis=1)
    cmap = { 0:"red", 3:"yellow", 6:"green", 8:"blue" }
    colors = harvest.apply (lambda x: cmap[x.Treatment], axis=1)
    sorg = harvest.Yield / 10
    lower = (harvest.Yield - harvest.Lower) / 10
    upper = (harvest.Upper - harvest.Yield) / 10
    plt.title ('2018 Yield (observed)')
    plt.ylabel ('Mg DM/ha')
    plt.xlabel ('Treatment')
    plt.bar (label, sorg, color=colors, yerr=[lower, upper], capsize=3)
    if where:
        plt.savefig (figlib + where + imgext)
    else:
        plt.show ()
    plt.clf ()

def frange (start, stop, step):
    val = start
    while val <= stop:
        yield val
        val += step
    
def add_scn (B, T, col, color):
    dlf = DaisyDlf(f"scn/B{B}T{T}/harvest.dlf")
    dim = dlf.column_units[col]
    harvest = dlf.Data
    harvest = harvest[3:] # drop warmup period
    ColData = harvest.loc[:,col].sort_values ()
    num = len (ColData)
    step = 100 / num
    ColData.index = frange (step, 100, step)
    plt.plot (ColData, color=color, label=f"B{B}T{T}")
    return [ num, dim ]

def plot_scn (col, dim=None, title=None, where=None):
    if title == None:
        title = col
    [num, d] = add_scn (2, 0, col, "darkred")
    [num, d] = add_scn (2, 6, col, "red")
    [num, d] = add_scn (3, 0, col, "darkblue")
    [num, d] = add_scn (3, 6, col, "blue")
    if dim == None:
        dim = d
    plt.title (f'{title} frequency {num} simulations')
    plt.legend ()
    plt.ylabel (dim)
    plt.xlabel ('%')
    if where:
        plt.savefig (figlib + where + imgext)
    else:
        plt.show ()
    plt.clf ()

def add_N (B, T, col, color):
    dlf = DaisyDlf(f"scn/B{B}T{T}/field_nitrogen.dlf")
    N = dlf.Data
    N = N[1:] # drop init values
    N = N.loc[:,col].sort_values ()
    num = len (N)
    dim = dlf.column_units[col]
    step = 100 / num
    N.index = frange (step, 100, step)
    plt.plot (N, color=color, label=f"B{B}T{T}")
    return [ num, dim ]

def plot_N (col, dim=None, title=None, where=None):
    if title == None:
        title = col
    
    [num, d] = add_N (2, 0, col, "darkred")
    [num, d] = add_N (2, 6, col, "red")
    [num, d] = add_N (3, 0, col, "darkblue")
    [num, d] = add_N (3, 6, col, "blue")
    if dim == None:
        dim = d
    plt.title (f'{title} frequency {num} simulations')
    plt.legend ()
    if dim:
        plt.ylabel (dim)
    plt.xlabel ('%')
    if where:
        plt.savefig (figlib + where + imgext)
    else:
        plt.show ()
    plt.clf ()

plot_sim_yield (where='sim_yield')
plot_2018_yield (where='2018_yield')

plot_scn ('sorg_DM', where='sorg_DM')
plot_scn ('sorg_N', where='sorg_N')
plot_scn ('WStress', where='WStress')
plot_scn ('NStress', where='NStress')

plot_N ('Denitrification', where='Denit')
