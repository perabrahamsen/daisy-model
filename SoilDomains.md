# Primary, secondary and tertiary transport #

Intra-aggregate pores constitute the primary domain, and inter-aggregate pores the secondary domain.  Both the primary and secondary domain are dominated by capillary forces.  Pores large enough for the capillary forces to become insignificant constitute the tertiary domain.  The primary and secondary domain together also constitute the soil matrix.

In practice, this means we can use Richard's Equation for the soil matrix.  The convection-dispersion equation assumes equilibrium between pore sizes, which we doesn't believe is a good assumption between the primary and secondary domains.  Therefore, solute transport in each of those domains are considered separate, with an exchange rate between them.

For the tertiary domain we cannot even use Richard's Equation.

# Secondary Domain #

The horizon parameter `secondary_domain` is used for enabling solute transport in the secondary domain.  The default is that no secondary domain exists.

# Tertiary Domain #

The tertiary transport mechanism is specified by `Tertiary` movement parameter.  The default value for 1D simulations is `old`, which work as it used to, that is macropore flow for horizons with more than 5% clay.  You can also specify the old macropore model directly, like this:

> (Tertiary old (macro none))

which has the same effect as

> (Tertiary none)

The `macro` parameter used to be located under `SoilWater` under the same name, and was then moved to `Movement`.

The `old` tertiary model only works with the `vertical` movement model, therefore `none` is the default value for 2D simulations for now.

New models for tertiary transport are under active construction, the first one being the [biopores model](TertiaryBiopores.md).

See CodeTertiary for information on the C++ implementation of tertiary transport.