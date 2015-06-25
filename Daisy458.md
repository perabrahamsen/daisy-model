# Changes #

## OpenMI interface doesn't work ##

Fixed in 4.60.

## Major internal restructuring ##

A lot of code has been modified, which potentially could mean many new
bugs.

## The individual `AOM`, `SMB` and `SOM` pools are now components ##

If you specify individual pools, you need to precede the parameters
with the keyword `component`, like this:

```
   (defam slurry organic
     (description "Average based on numbers provided by Torben Bonde, Danish
   Environmental Protection Agency, approximately 1991.
   Added by <sha@kvl.dk>, 2000.")
     (volatilization 0.15 [])
     (om (component (initial_fraction 0.72 [])
                    (C_per_N 100 [(g C/cm^3)/(g N/cm^3)])
                    (turnover_rate 2.0e-4 [h^-1])
                    (efficiency 0.60 0.60 [])
                    (fractions 0.0 1.0 0.0 []))
         (component (initial_fraction 0.18 [])
                    (turnover_rate 2.0e-3 [h^-1])
                    (efficiency 0.60 0.60 [])
                    (fractions 0.0 1.0 0.0 []))
         (component (C_per_N 11 [(g C/cm^3)/(g N/cm^3)])
                    (turnover_rate 1.0 [h^-1])
                    (efficiency 1.0 [])
                    (fractions 0.0 0.0 1.0 []))))
```

You may want to use some of the build-in parameterizations, which can
be found in the reference manual.

## The vernalization crop submodule has been made into a component ##

This means you have to specify `default` before the vernalization
parameters in the crop parametrization, like this:

```
   (Vernal default (DSLim 0.33) (TaLim 5.00) (TaSum -50.0))
```

The crop parameterizations distributed with Daisy has been updated.

## Water transport model failures more discrete ##

The number of times the water (or solute) models failed to find a
solutions is now reported only at the end of the simulation.  You can
still find them in the daisy.log file, and the new "Failure" log
paramterization will also include information about when the failures
occured in an easy parsable format.

## Sample taastrup.dwf extended and renamed to dk-taastrup.dwf ##

In now contains weather data until 2008-10-31.  Note that the level of
quality control of the newer data has been reduced.

## New dk-taastrup-hourly.dwf sample file ##

This file contains hourly weather data from Taastrup for the period
1995-03-24 to 2008-10-31.  The data is detailed enough to feed the
more advanced photosynthesis and svat models in Daisy, but the quality
control performed on the data is not impressive.

## New genweather.dai sample setup file ##

This file demostrates how to aggregate and distribute weather data.

## The `Field nitrogen` and `Field chemical` logs now includes tertiary domain ##

The content of the biopores is now included in the balance for these
two log parameterizations.

## **New `secondary` parameter to the `equilibrium` reaction ##**

The `secondary` parameter is a flag indicating that the equilibrium
reaction takes place in the secondar domain rather than the primary domain.

## New `colloid` parameter to the `equilibrium` reaction ##

The value of the `colloid` parameter should be a name of chemical to
use instead of the soil for the `rho_b` scope variable.  The idea is
to use it in situation where a solute may bind to colloids instead of
to imoobile soil.  See `test-colloids.dai` for an example.

## Colloids work ##

The `colloid` chemical and the `colloids` chemistry have been removed
from the `chemistry.dai` file, and moved to the `test-colloids.dai`
file instead.  The later now contains an example with both colloids
and a colloid bound pesticide

## Bug fixes ##