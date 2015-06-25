# More flexible weather data handling #

## The `simple` and `none` weather models are gone ##

The new `const` model (see later) can sometimes be used as
replacement.

## The `default` weather model is rewritten ##

There are probably many new bugs, and the error messages are likely
less informative.  However, it is more flexible as described below.

### More fine grained weather data available weather files ###

You can now specify `Minute`, `Second`, and even `Microsecond` in the
weather files.  If you have set `minimal_timestep`, Daisy will try to
make the timesteps match the weather data.

You can also specify time with a `Date` (or `time`) column.  The
format is one of

```
  yyyy-mm-dd
  yyyy-mm-ddThh
  yyyy-mm-ddThh:mm
  yyyy-mm-ddThh:mm:ss
  yyyy-mm-ddThh:mm:ss.uuuuuu
```

depending on the desired resolution.

All weather data describe the preceding time step, except for daily
data which describe the specified date.  So
```
  1999-02-05
```
will specify the weather for the fifth of February, 1999, while
```
  1999-02-05T04
```
will specify the weather for the the hour between 3 and 4 the same day
(assuming hourly data).

### Keywords, columns, and parameters interchangeable ###

All numeric data about the weather files can be specified as keywords,
columns, or parameters.  For example, to run with a constant vapour
pressure of 1000 Pascal, you can specify
```
  VapPres: 1000 Pa
```
in the weather file header.  Or alternatively, as a parameter:
```
  (weather default "dk-taastrup.dwf"
    (VapPres 1000 [Pa]))
```
Doing the later will overwrite any value specified in `dk-taastrup.dwf`.

Data that are not simple numbers (like `Surface` or `PrecipCorrect`) can
be specified as keywords or parameters.

### Single number allowed for `PrecipCorrect`  and `PrecipScale` ###

If you specify a single number, it is assumed to represent the whole year.

## New `table` model ##

The new `table` model differs from the `default` model on three points.

  1. The file name is specified with an explicit `file` parameter.  E.g.
```
  (weather table (file "dk-taastrup.dwf"))
```
  1. The `missing\_years' parameter is not supported.
  1. The `table` model can be used with the `combine` and `time` models, explained below.

## New `const` model ##

This is identical to the `table` model, except that the file
parameters is not accepted.  This means weather data must be specified
as parameters.  The value will therefore be constant, except the
radiation which will still follow a day cycle based on relative
extraterrestrial radiation.

## New `combine` model ##

The `combine` model will allow you to combine multiple sources of
weather data.  It has a single parameter `entry`, which is a list of
weather sources entries.  Each entry have `begin`, `end`, `use`, and
`source` parameters.  The `begin` and `end` parameters specify the
time interval where we should use this particular source.  The `use`
parameter specified the data we should extract from this particular
source.  The particular value `Any` specifies that we should use all
the data provided by the source.  Finally, `source` specifies the
source.  This can be any weather model but `default`.  For each type
of weather data, Daisy will examine each entry in sequence, and use
the first match.  If the `begin` and `end` parameters are unspecified,
the entry will cover from the start to the end of the simulation.  The
default value for `use` is `Any`.

Fo example, we might want to use weather data from the file
`dk-taastrup.dwf`, except for precipitation during the summer, where
we instead use data from file `dk-taastrup-hourly.dwf`.
```
  (weather combine
           (entry ((begin 2006 6 1 0)
                   (end   2006 9 1 0)
                   (use Precip)
                   (source table (file "dk-taastrup-hourly.dwf")))
                  ((source table (file "dk-taastrup.dwf")))))
```

## New `time` model ##

The time model allows you to use weather data from a different period.
E.g.
```
   (weather time (from 2002) (to 2006)
            (source table (file "dk-taastrup.dwf")))
```
will use four years old weather data in the simulation.  Note that all
weather data is mapped, so 2001 data will be used for 2005, and 2003
data for 2007.  It can be combined with the `combine` model to map
only a selected period.  For example
```
  (weather combine
           (entry ((begin 2006 1 1 0)
                   (end   2007 1 1 0)
                   (source time (from 2002) (to 2006)
                           (source table (file "dk-taastrup.dwf"))))
                  ((source table (file "dk-taastrup.dwf")))))
```
will use data from `dk-taastrup.dwf`, with the exception that 2002
data will be used for 2006.

Note that while dates until the 28 of February will be an exact match
for the two specified years, other dates may be one day off due to
leap years.

If you don't want to map year to year, you can specify `offset`
instead of `from` and `to`.
```
  (weather time (source table (file "dk-taastrup.dwf"))
           (offset (days 1) (hours 0) (minutes 0) (seconds 0) (microseconds 0)))
```
This will use yesterdays weather data.  So will this
```
  (weather time (source table (file "dk-taastrup.dwf"))
           (offset (days 1)))
```
as all the `offset` parameters default to zero.  There is no `year`
parameter to offset, since that is not a well defined length of time.