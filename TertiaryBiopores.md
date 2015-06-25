# The `biopores` model #

As explained in SoilDomains, the tertiary domain constitutes pores large enough for the capillary forces to be negligible.  In the `biopores` model allows the user to divide the tertiary domain up in a number of classes, where the biopores in each class are considered to have similar physical properties.

Here is an example with two biopore classes.
```
  (Tertiary (biopores (classes (matrix (height_start 0 [cm])
                                       (height_end -145 [cm])
                                       (density 100 [m^-2])
                                       (diameter 10 [mm])
                                       (K_wall_relative 1 [%]))
                               (drain  (height_start -5 [cm])
                                       (height_end -100 [cm])
                                       (diameter 10 [mm])
                                       (density 100 [m^-2])
                                       (pipe_position -100 [cm])))))
```
As you see, the main, and only mandatory, parameter is `classes`.  There are more parameters, which you can find in the reference manual.  The value of the `classes` parameter is a list of `biopore` model parameterizations.  Here the two parameterizations are of two different models, the `matrix` model for biopores that end in the matrix, and the `drain` model for biopores that end in drain pipes.  We could easily have multiple parameterizations of each model, for example to model that biopores start in different depths.

# The `biopore` component #

There are some parameters that are shared by both biopore models, notable where they start (`height_start`) and ends (`height_end`), their diameter, and how common they are (`density`).

The last has a specific twist, as the density may vary with the _x_ dimension if you have a two dimensional setup.  For example, if you have the drain pipe located at _x = 0_ cm, you may want to decrease the density of biopores ending in the drain pipe as a function of _x_.  One way to do this is with the `plf` function.
```
  (density (plf x (range [m^-2])
                  (domain [cm])
                  (points (20 10)
                          (30 0))))
```
Here, we specify that there are 10 biopores (of the given class) per square meter in the area from 0 to 20 cm from the drain pipes (at _x = 0_), no biopores when you are more than 30 cm from the drain pipes, and linear interpolation in between.  That means there will be a density of 5 biopores per square meter at 25 cm from the drain pipe.

Look in the `number` component of the reference manual for alternatives to the `plf` function.

## The `matrix` model ##

The `matrix` model furthermore adds resistances for water leaving the biopore to the primary domain (K\_wall\_relative).  The parameter denote the conductivity of the biopore wall, compare to the surrounding matrix.  The wall thickness is assumed to be 10% of the biopore radius.

## The `drain` model ##

The `matrix` model needs to be told the height of the drain pipe (`pipe_position`).  This might seem redundant as you most likely have already told Daisy where the drain pipe is located, but is needed as it is possible to have setups with multiple drain pipes.

# Implementation #

You can read about the C++ implementation of the `biopores` model in CodeTertiaryBiopores.