# Changes #

## Many speedups ##

## The default weather model will now distribute daily rain better ##

Rain intensity below the value of 'minimum\_precipitation' parameter
will be applied beginning at midnight at the rate specified by the
parameter, followed dry weather for the rest of the day.

The default value (0.83 mm/h) is the average hourly rain intensity at
the Taastrup agrometeorological station.

The effect of is that transpiration and soil evaporation increase, and
evaporation from ponded and intercepted water decreases.

## Most management actions are now instantaneous ##

They used to take one timestep (1 hour).

For example

```
  (wait (at 1987 4 5 1))
  (fertilize (pig_slurry (first_year_utilization 100 [%]))
             (equivalent_weight 100 [kg N/ha]))
  (sow "Grass")
  (sow "Spring Barley")
```

will now both fertilize and sow the two crops during the same hour, it
used to take three hours to complete all three actions.

This includes irrigation, even when a irrigation duration is
specified.  For example

```
  (wait (at 1987 6 5 1))
  (irrigate_overhead 2 [mm/h] (hours 3)
                     (solute (NH4 1 [g N/l])(NO3 2 [g N/l])))
  (fertilize (mineral (weight 80.0 [kg N/ha])
                      (NH4_fraction 0.5 [])))
```

will start the irrigation and then immediately fertilize in the first
hour.  The irrigation will then continue for two more hours, without
need for further manager involvement.

This affects the `activity` action model.  The change was made to make
management more independent of the selected timestep, and because the
old semantics could be problematic to get right.

## New `EpFacWet` canopy parameter ##

Specifies `K_c` for wet surface.  By default it is identical to `EpFac`.

## The `FAO_PM` reference evapotranpiration model used actual albedo ##

Not it used the reference value.

## The `FAO_PM` model will now use wet surface reference ##

## New `biopores` tertiary model parameter `active_msg` ##

Set this to `cell` (old style), `range` (new default) or `none` to control
message when biopores are activated or deactivated.

## New hydraulic model `B_C_inverse` ##

This is an version of the Burdine/Campbell model where the `b` and
`h_b` parameters are found from the soil wilting point and field
capacity.

The `b` and `h_b` parameter values will be written to the daisy.log
file.

```
(defhorizon Ap FAO3
  "Andeby top soil."
  (clay 8.0 [%]) (silt 10.5 [%]) (sand 81.5 [%])
  (humus 1.12 [%]) (dry_bulk_density 1.5 [g/cm^3])
  (C_per_N 11.0 [g C/g N])
  (hydraulic B_C_inverse 
             (K_at_h -100.0 [cm] 1e-6 [m/s])       
             (Theta_wp 0.03 [])
             (Theta_fc 0.2 [])
             (Theta_sat 0.4 [])))
```

## New `sorption` reaction model ##

Faster than `equilibrium`, more flexible than `adsorption`.

## The `aperture` secondary model take new `use_secondary` parameter ##

Set this to false to make `aperture` only affect hydraulic
conductivity, without introducing a secondary domain for solute
transport.

## Fix output for gnuplot 4.4 ##

## Chemicals may now decompose when part of the litter pack ##

## Bug fixes ##