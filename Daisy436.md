# Changes #

## The `SeedN` `CrpN` parameter removed ##

It has been marked obsolete for some time.  Use the `NCrop` Production parameter instead.

## New `Seed` parameter in the `default` crop model ##

The `Seed` parameter govern initial growth.  The default model for initial growth is based on a forced LAI function as before, and thus take over some parameters from the Canopy submodel.  This means crop parameterizations needs a small change.

Before:
```
 (Canopy (SpLAI  0.020 [(m^2/m^2)/(g DM/m^2)])
         (SpLAIfac (0.0 100) (1.0 100))
         (DSLAI05 0.5)
         (LeafAIMod (1.5 1.1)(2.0 0.3))))
```
Now:
```
 (Seed LAI
       (DSLAI05 0.5)
       (SpLAIfac (0.0 100) (1.0 100)))
 (Canopy (SpLAI  0.020 [(m^2/m^2)/(g DM/m^2)])
         (LeafAIMod (1.5 1.1)(2.0 0.3)))
```

Note that two additional changes has been made to this model:

1) The `LeafAIMod` parameter is taken into account when deciding when to switch from the forced LAI model to the leaf mass based model, which will avoid mysterious jumps in LAI when switching model.

2) The initial LAI is now treated like other kinds of forced LAI, which mean the initial LAI can be found in the `ForcedCAI` log variable, and the mass based CAI can be found in the `SimCAI` log variable.  The LAI actually used can, as always, be found in the `CAI` log variable.

## New `release` seed model ##

The `LAI` seed model suffers from two drawbacks:

1) It does not take the amount of seed applied when sowing into account, except indirectly through the value of the `DSLAI5` parameter.

2) The model becomes very sensitive to low radiation or stress during the initial growth phase, as not enough dry matter may be produced to be self sustainable.

The alternative `release` model is based on a first order release of assimilate from the seeds, which naturally scale with the amount of seeds applied, and ensures a minimum amount of dry matter for leaves and roots that doesn't depend on the weather and soil conditions.

```
  (defcrop "Spring Barley; Seed" "Spring Barley" 
    "A spring barley with initial growth governed by seeds."
    ;; We remove the old initial guess on seed N.  A negative number for
    ;; NCrop will force Daisy to calculate a new number based on seed
    ;;amount and N concentration.
    (Prod (NCrop -1.0))
    ;; Here we select the 
    (Seed release
          ;; We release 30% of the carbon per day.  It is first order,
          ;; so the total amount will be decreasing over time.
          (rate 0.3 [d^-1])
          ;; 15% of the seeds are water.
          (DM_fraction 0.85 [])
          ;; Almost half of the seeds is available as carbon assimilate.
          (C_fraction 0.4676 [])
          ;; The seeds also contain some nitrogen to get the crop started.
          (N_fraction 1.8 [%]))
    ;; The initial leaves are very thin.  Note that LeafAIMod is often
    ;; used for modifying old leaves, if so, you need to append the
    ;; original value for LeafAIMod here. 
    (Canopy (LeafAIMod (0.0 2.0) (0.4 1.0))))

  ;; When we sow the modified crop, we need to specify the amount applied.
  (sow ("Spring Barley; Seed" (weight 180 [kg w.w./ha])))
```