#! /usr/bin/env python3
import codecs
try:
    codecs.lookup('mbcs')
except LookupError:
    ascii = codecs.lookup('ascii')
    func = lambda name, enc=ascii: {True: enc}.get(name=='mbcs')
    codecs.register(func)

import numpy as np
import pandas as pd

def gen_sow ():
    sow = pd.read_csv ('sow.csv', parse_dates=['Date'], dayfirst=True,)
    print (';;; Sow actions')
    site = ''
    for index, row in sow.iterrows ():
        if site != row['Site']:
            if site != '':
                print (')')
            print ('')
            site = row['Site']
            print ('(defaction "Sow ' + site + '" activity')

        time = row['Date']
        seed = row['Seed']
        print ('  (at ' + str (time.year)
               + ' ' + str (time.month)
               + ' ' + str (time.day)
               + ' (do (sow "PLAP '
               + row['Crop'] + '"'
               + (' (seed ' + str (seed) + ' [kg w.w./ha])'
                  if not np.isnan (seed)
                  else '')
               + ')))')
    print (')')

def gen_harvest ():
    harvest = pd.read_csv ('harvest.csv', parse_dates=['Date'], dayfirst=True,
                           keep_default_na=False)
    print (';;; Harvest actions')
    site = ''
    for index, row in harvest.iterrows ():
        if site != row['Site']:
            if site != '':
                print (')')
            print ('')
            site = row['Site']
            print ('(defaction "Harvest ' + site + '" activity')

        time = row['Date']
        removed = row['Removed']
        is_removed = (removed == 'Removed' or removed == '')
        print ('  (at ' + str (time.year)
               + ' ' + str (time.month)
               + ' ' + str (time.day)
               + ' (do (harvest "PLAP '
               + row['Crop']
               + '" (stub ' + str (row['Stubble']) + ' [cm])'
               + (' (stem 0) (leaf 0)' if not is_removed else '')
               + ')))'
               + (' ;; ' + removed if removed != '' else ''))
    print (')')

def gen_tillage ():
    harvest = pd.read_csv ('tillage.csv', parse_dates=['Date'], dayfirst=True,
                           keep_default_na=False)
    print (';;; Tillage actions')
    site = ''
    for index, row in harvest.iterrows ():
        if site != row['Site']:
            if site != '':
                print (')')
            print ('')
            site = row['Site']
            print ('(defaction "Tillage ' + site + '" activity')

        time = row['Date']
        action = row['Action']
        depth = row['Depth']
        if action != '':
            print ('  (at ' + str (time.year)
                   + ' ' + str (time.month)
                   + ' ' + str (time.day)
                   + ' (do (' + action
                   + ('' if depth == ''
                      else
                      (' (depth -' + str (depth) + ' [cm])'
                       if action == 'plowing'
                       else ' -' + str (depth) + ' [cm]'))
                   + ')))')
    print (')')

print (";;; plap_gen.dai --- Automatically generated, don't edit.")
print ('')
gen_sow ()
print ('')
gen_harvest ()
print ('')
gen_tillage ()
print ('')
print (';;; plap_gen.dai ends here.')
