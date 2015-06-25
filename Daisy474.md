# Changes #

## New `area` column parameter ##

When you log multiple columns to the same log file, the values will be
weighted with this parameter.  Example: If you have one column
representing 1 hectar with a percolation of 6 mm/d, and another column
representing an area of 4 hectars with a percolation of 1 mm/d, the
total percolation will be

```
  1 [ha] * 6 [mm/d] + 4 [ha] * 1 [mm/d]
 -------------------------------------- = 2 [mm/d]
            1 [ha] + 4 [ha]
```

The parameter does not affect simulations with only a single columns,
nor log files where you have specified the column (with the "column"
paramater).

## Allow using base names for logged chemicals more places ##

Specifically, biopores.

## More units and expressions ##

## New `accumulate` plot parameter ##

## Changes to `dk-soil.dai` ##

The "Jyndevad Ap" and "Jyndevad C" now parameterised for Brooks and
Corey hydraulic model.

## New `pipes` tertiary model ##

This works like `pipe` groundwater model, but can be combined with any
lower boundary.

## New `K_average_horizontal` parameter to the `Mollerup` uzrect model ##