# Introduction #

This page describes the code for implementing tertiary transport in Daisy.  Tertiary transport here is the transport of water and solutes in "large" macropores, as described in the [soil domains](SoilDomains.md) page.  The "large" macropores are macropores where the capillary forces no longer dominates, typically biopores as made by earthworms or old root channels.  Their diameter are typically in the millimeter range.

# The tertiary component #

The interface to the tertiary component is the `Tertiary` class in [tertiary.h](http://code.google.com/p/daisy-model/source/browse/trunk/tertiary.h).

## The interface to water ##

```
  virtual void tick (const Geometry&, const Soil&, const SoilHeat&,
                     const double dt, SoilWater&, Surface&, Treelog&) = 0;
```

This function have two purposes, first it should extract any water from the surface that should go to the biopores, and the second is the interaction with the matrix water if that is not handled by the small timesteps.

## The interface to solutes ##

```
  virtual void solute (const Geometry&, const SoilWater&, 
                       const std::map<size_t, double>& J_tertiary,
                       const double dt,
                       Chemical&, Treelog&) = 0;
```

This function will be called once for each chemical that is tracked by Daisy.  The `J_tertiary` parameter contains information about how much of the chemical enter directly to the biopores from the surface.  It is implemented as a map of edge -> flux.  When implementing a model you should loop over all the map entries, to be sure all the fluxes are added to the biopore domain.

## Other functions ##

```
  virtual bool has_macropores () = 0;
```

This should return true, unless you are implementing a model with no tertiary transport.  It is used by the heuristics for discretization of the soil.

The `check`, `initialize`, and `output` functions have their usual meanings.

# Models #

## The none model ##

The none model does nothing, and is implemented by the `TertiaryNone` class in [tertiary.C](http://code.google.com/p/daisy-model/source/browse/trunk/tertiary.C).

## The old model ##

The old model is implemented by the `TertiaryOld` class in [tertiary\_old.C](http://code.google.com/p/daisy-model/source/browse/trunk/tertiary_old.C).  It delegates water transport to the macro component, and solute transport the mactrans component.  It only works with the vertical movement component, for which it is default.  It does not support small timesteps.

## The biopores model ##

The biopores model is implemented by the `TertiaryBiopores` class in [tertiary\_biopores.C](http://code.google.com/p/daisy-model/source/browse/trunk/tertiary_biopores.C).  It implements both the small and large timestep interfaces, using multiple inheritance.  The biopores model divides the tertiary domain into a number of classes, each represented by a biopore model.

It is described in more details in [for programmers](CodeTertiaryBiopores.md) and [model users](TertiaryBiopores.md).