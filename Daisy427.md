# Changes #

## New `every` condition ##

This one match the simulation at a user specified interval.  It takes the same parameters as the `timestep` parameter top level Daisy parameter.

```
  (every (years 0) (days 1) (hours 2) (minutes 3) (seconds 4))
```

## The optional `step` parameter removed from the ..`ly` conditions ##

This affects conditions `hourly`, `daily`, `weekly`, `monthly` and `yearly` conditions.  They now always matches once at the last time step of the interval they are named after.

## New `minutely` and `secondly` conditions ##

These are true at the end of each minute and each second, respectively.

## Surface chemicals now decompose ##

The two new parameters `surface_decompose_rate` and `surface_decompose_halftime` determine how fast they decompose.  If they are both unspecified, the `decompose_rate` (or `decompose_halftime`) parameters are used instead.  No abiotic factors currently affects the surface decomposition, not is any decompose products produced (yet).

Set `surface_decompose_rate` to 0 in order to get the old behavior.