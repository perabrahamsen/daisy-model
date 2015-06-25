# Changes #

## FAO\_PM pet model: `use_wet` now default true ##

Should lead to more evapotranspiration from wet surfaces.

## Reduced memory use during initialization by orders of magnitude ##

## Dimension change in log files ##

The dimension of most flux variables in the standard log files has
changed so it no longer contain the log timestep.  For example,
logging "Field water" weekly would have the unit for fluxes be mm/w,
while logging monthly would have the same units be mm/m.  Now it will
be mm.  The actual values will be the same, the interpretation is
"amount of water since last we logged".

The advantages of this new interpretation are that 1) We avoid weird
units, 2) it works with intervals that have no nice symbol (like
biweekly), and 3) it works with variable timesteps.

Technically, internally in Daisy most flux unit are per time
(independent of the actual timestep used by Daisy).  When a log file
parametrization specifies (handle sum) this value will be multiplied
with the length of the timestep.  What happens now is that the unit is
also multiplied be the unit of the timestep [h](h.md).

If you really want the dimension of flux variables to be per time,
specify (handle average) instead, which will divide the value and unit
by the total time between log entries in hours, returning to the
original unit.  You can then tell Daisy to convert this to another
timestep by specifying `dimension` explicitly.

## Variable timesteps. ##

Yo can now allow Daisy to temporarily decrease timesteps in order to
achieve more stable results.  You do this by setting the parameter
`minimal_timesteps`, like this:

```
  (minimal_timestep (microseconds 1))
```

One microsecond is the smallest possible timestep.  The `timestep`
parameter specifies the largest timestep.  There is no hard upper limit
here, but many models will likely fail to work with timesteps larger
than one hour.

By default, `minimal_timestep` is set to the same value as `timestep`,
thus forcing the models to use fixed timesteps.

This variable timestep affects all Daisy processes, unlike the already
existing variable timesteps internal to the water movement and solute
transport processes.

## Drain component and biopore tertiary model may limit timestep ##

If you specify drain pipes with the `Drain` column parameter, or
specify macropores by setting the Tertiary parameter to the `biopores`
model, the timestep may be limited by the source/sink terms.  Daisy
will calculate an initial estimate for the source/sink terms, and try
to limit the timesteps so that the water change in each numerical cell
stay "reasonable" within the timestep.

What is reasonable is determined by the `max_sink_change` parameter in
SoilWater.  It is given as percentage of the difference in water
content between wilting and saturation point.

```
  (SoilWater (max_sink_change 1 [%]))
```
The default value is 10%.

## Chemistry may also limit timestep ##

This is again based on the matrix water sink term from the drain
component and biopore tertiary model.

You can specify acceptable loss relative to the total content
(including sorbed forms), the solute content alone, or the content of
the secondary domain (if applicable).  The parameters, together with
their default values, are specified like this.

```
  (Chemistry original
             (max_sink_total 50 [%])
             (max_sink_solute 90 [%])
             (max_sink_secondary 150 [%])
             (min_sink_total 1 [%]))
```

The last parameter, `min_sink_total` overwrites the other, meaning
that 1% of the total content of a cell will always be allowed to
escape through the sinks.

## New `microsecond` resolution ##

Daisy can now track time on microsecond basis if so desired.  You can
specify microseconds all the same places where you used to be able to
specify seconds.  For example, to get microseconds in log files, specify

```
  (log_time_columns year month mday hour minute second microsecond)
```

## New `Time` log ##

This log will provide information about the timestep.

## The `none` chemistry is now build in ##

So there is no longer any reason to load `chemistry.dai` just in order
to disable chemistry.

## Biopore wall resistance change ##

The R\_primary and R\_secondary parameters are gone, instead we have
K\_wall\_relative, which denote the conductivity of the biopore wall
relative to the surrounding matrix.

## Water movement on the x surface with 2D simulations ##

The water movement on surface within the system being simulated is now
controlled by a new `Surface` parameter `LocalDetentionCapacity`.  If the
average ponding on the surface is above this threshold, all part of
the surface will have the same ponding.  Otherwise, if the ponding
locally is above this threshold, excess water will be distributed
evenly to the rest of the surface (limited by the threshold).

## Tertiary flux below surface now approximated ##

The `biopores` tertiary model didn't use to calculate a flux below
surface.  It still doesn't but it find an approximate value based on a
mass balance that ignores the storage capacity of the biopores.  The
values will likely be very much of for individual timesteps, but
averaged over a long period (long enough that the storage capacity
become an insignificant part of the mass balance), the approximation
should work well.

## New 2D plot facilities ##

### Content ###

Assuming `soil_water_content.dlf` list water content in all cells, the
following small setup file will generate a `daisy.gnuplot` file, which
will show a 2D representation on the screen when feed into the gnuplot
program.

```
  (defgnuplot sample soil
    (file "soil_water_content.dlf")
    (when 2000 5 28))
  
  (defprogram showme gnuplot
    (graph sample))
  
  (run showme)
```

The `type` parameter allows you to select between `block`, `smooth`,
`surface`, and `contour` plots, and the `top`, `bottom`, `left` and
`right` parameters allows you to zoom into a rectangular subset of the
soil..  More information about generating plots can be found in the
tutorial, and more details on the `soil` model can be found in the
reference manual.

### Flux ###

You can now plot horizontal flux as a function of depth, or vertical
flux as a function of the x-position.  Here is an example that plot
horizontal flux at two different distances from the drain.

```
   (defxysource hor50 flux
     "Horizontal flux at x=50."
     (file "matrix-water-flux.dlf")
     (x 50 [cm])
     (when 1999 9 28))

   (defxysource hor150 flux
     "Horizontal flux at x=150."
     (file "matrix-water-flux.dlf")
     (x 150 [cm])
     (when 1999 9 28))

   (defgnuplot plothor xy
     "Horizontal flux at x=50 and x=150."
     (source hor50 hor150))

   (defprogram showit gnuplot
     (graph plothor))

   (run showit)
```