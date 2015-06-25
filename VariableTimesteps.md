You can now allow Daisy to temporarily decrease timesteps in order to achieve more stable results.  You do this by setting the parameter `minimal_timesteps`, like this:

```
  (minimal_timestep (microseconds 1))
```

One microsecond is the smallest possible timestep.  The `timestep` parameter specifies the largest timestep.  There is no hard upper limit here, but many models will likely fail to work with timesteps larger than one hour.

By default, `minimal_timestep` is set to the same value as `timestep`, thus forcing the models to use fixed timesteps.

This global variable timestep affects all Daisy processes, unlike the variable timesteps internal to the water movement and solute transport processes.

## Drain component and biopore tertiary model may limit timestep ##

If you specify drain pipes with the `Drain` column parameter, or specify macropores by setting the `Tertiary` parameter to the `biopores` model, the timestep may be limited by the source/sink terms.  Daisy will calculate an initial estimate for the source/sink terms, and try to limit the timesteps so that the water change in each numerical cell
stay "reasonable" within the timestep.

What is reasonable is determined by the `max_sink_change` parameter in SoilWater.  It is given as percentage of the difference in water content between wilting and saturation point.

```
  (SoilWater (max_sink_change 1 [%]))
```
The default value is 10%.

## Chemistry may also limit timestep ##

This is again based on the matrix water sink term from the `drain` component and `biopore` tertiary model.

You can specify acceptable loss relative to the total content (including sorbed forms), the solute content alone, or the content of the secondary domain (if applicable).  The parameters, together with their default values, are specified like this.

```
  (Chemistry original
             (max_sink_total 50 [%])
             (max_sink_solute 90 [%])
             (max_sink_secondary 150 [%])
             (min_sink_total 1 [%]))
```

The last parameter, `min_sink_total` overwrites the other, meaning that 1% of the total content of a cell will always be allowed to escape through the sinks.