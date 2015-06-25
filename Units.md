# User defined units #

You can define your own units.  The most useful way to do this is to derive from `SIfactor`, like this:
```
  (defunit [mph] SIfactor
    "Miles per hour.
  A speed unit in common use in the USA."
    (length 1)
    (time -1)   
    (factor 0.44704))
```
The factor is the number you should multiply with to convert to SI base
units, in this case `[m/s]`.

The new unit can be used for any parameter that accepts the same SI
base.  For example, since irrigation is usually specified in `[mm/h]`,
which has the same SI base, you can use the newly defined `[mph]` instead.
```
  (irrigate_overhead 1e-6 [mph])
```
You can also derive from an existing unit, like this:
```
  (defunit [mph] [mm/h]
    "Miles per hour.
  A speed unit in common use in USA."
    (factor 0.44704))
```
This saves you from specifying the dimensions (`length` and `time`).
However, note that `factor` remains the same.  It should still
specify how to convert to the SI base units `[m/s]`.

See the reference manual for a full list of SI dimensions and base units.

You can also define your own base and derived units, independently of the eight SI
defined dimensions.
```
  (defunit EUR base "European currency.")
  
  (defunit EUcent factor
    "European cents."
    (base EUR)
    (factor 0.01))
```
However, there is little point in doing this, as none of the existing
Daisy code expects these units.

# Caveat #

When converting between units, only the 8 SI dimensions are considered.  This mean that Daisy will happily convert `[kg N/ha]` to `[g C/m^2]`, and worse, that Daisy will consider the convertion factor between `[kg NO3]` and `[kg NO3-N]` to be 1.0, as both have the same SI base `[kg]`.