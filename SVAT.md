# The `svat` component #

The **Soil Vegetation and ATmosphere** component is repsonsible for maintaining energy balance in the area between the soil and the free atmosphere.   In Daisy, it is located in the default Bioclimate module.  In Daisy, it is responsible for calculating temperatures for canopy (both dark and light leaves), suggesting transpiration (which may be modified if the root system is unable to deliver), and can optionally calculate a water stress that will limit crop production.  The individual models may calculate further fluxes, as well as resistances between the components of the system, but these will not be used outside the model.

## The `none` model ##

The **none** model will say that the canopy and soil surface both have the temperature of free air (above the canopy) read from the weather module.   No resistances will be calculated.  transpiration will be based on potential evapotranspiration further limited by available soil water.  No stress will be suggested, the root model will calculate its own stress.

This model is enables by default, but can be enabled explicitly by adding

```
  (Bioclimate original (svat none))
```

to the column definition.


## The `PMSW` model ##

The **Penman-Monteith Shuttleworth-Wallace** model was written primarily by [Peter van der Keur](PeterVanDerKeur.md) for the [RS-Model](RSModel.md) [EU](EU.md) project.

The model should only be used when you have hourly weather data, inclusive wind speed and humidity.

The model will deliver temperature for the leaves (not partitioned in dark and light) and canopy air (a hypothetical point in the middle of the canopy), as well production stress based on estimated canopy resistances.  It will not actually change the transpiration, only the production.

```
  (Bioclimate original (svat PMSW))
```

The `PMSW` log parametrization will give you lots of information about the internal state of the SVAT system.

## The `SSOC` model ##

The **Sun Shade Open Canopy** model.

The model should only be used when you have hourly weather data, inclusive wind speed and humidity.  Also, the crops should be based on the FCC3 or or FCC4 photosynthesis models.

```
  (Bioclimate original
    (svat SSOC)
    (raddist sun-shade))
```

Sunlit and dark leaves will be given separate temperatures, and transpiration will be adjusted to match energy balance.

The `SSOC` log parametrization will give you lots of information about the internal state of the SVAT system.

The development was funded by the [SAFIR](SAFIR.md) project, where you can also find more detailed documentation.