#! /usr/bin/env python3

import sys
sys.path.append('/home/xvs108/PyDaisy')

# Cygwin python3 bug workaround
import codecs
try:
    codecs.lookup('mbcs')
except LookupError:
    ascii = codecs.lookup('ascii')
    func = lambda name, enc=ascii: {True: enc}.get(name=='mbcs')
    codecs.register(func)

import numpy as np
import pandas as pd

from pydaisy.Daisy import *

# Read measured data
bbch = pd.read_csv ('bbch.csv', parse_dates=['Date'], dayfirst=True)

def find_ds (site, crop):
    Site = site.capitalize ()
    dlf = DaisyDlf(f"log/{site}-{crop}.dlf")
    ds = dlf.Data
    for index, row in bbch.iterrows ():
        if row['Site'] == Site:
            date = row['Date']
            try:
                val_bbch = row['Primary']
                val_ds=ds['DS'][date]
                print (f"{val_bbch:.0f},{val_ds},{date:%Y-%m-%d},{Site},{crop}")
            except:
                dummy=1

print ("BBCH,DS,Date,Site,Crop")
find_ds ("tylstrup", "ww")
find_ds ("silstrup", "ww")
find_ds ("estrup", "ww")
find_ds ("faardrup", "ww")
find_ds ("tylstrup", "sb")
find_ds ("jyndevad", "sb")
find_ds ("silstrup", "sb")
find_ds ("estrup", "sb")
find_ds ("faardrup", "sb")
