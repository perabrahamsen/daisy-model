# Introduction #

The biopores [tertiary model](CodeTertiary.md) is implemented by the `TertiaryBiopores` class in [tertiary\_biopores.C](http://code.google.com/p/daisy-model/source/browse/trunk/tertiary_biopores.C).  It implements both the [small](CodeTertsmall.md) and large timestep interfaces, using multiple inheritance.

# Biopore classes #

The biopores model divides the [tertiary domain](SoilDomains.md) into a number of classes, each represented by a biopore model.  The biopores instantly transport water, either to the bottom of the biopore (where it might slowly leak back to the matrix domain), or to drain pipes where it is lost from the system.  The main parameter is `classes`, which contains both parameters and state for each biopore class.

```
  const auto_vector<Biopore*> classes; // List of biopore classes.
```

Each class is an instance of the biopore component, for which two models exists.  The **matrix** model for biopores that end in the matrix, and the **drain** model for biopores that ends in drain pipes.  Much of the work of the biopores tertiary model is delegated to the individual biopore classes.

# Active cells #

An important concept in the biopores model is **active and inactive cells**.  The idea is that cells are marked active once the matrix potential surpass `pressure_initiate` and stay active until it becomes lower than `pressure_end`.  While the cell is active, water can (but doesn't have to, the biopores might be full) move from the matrix domain to the tertiary domain.  The state is stored in the `active` state variable, and updated by the `update_active` member function.

```
  const double pressure_initiate;// Pressure needed to init pref.flow [cm]
  const double pressure_end;     // Pressure after pref.flow has been init [cm]
  std::vector<bool> active;      // Biopore activity 
  void update_active (const std::vector<double>& h);
```

The `update_active` function is also part of the [small timestep interface](CodeTertsmall.md).

# Surface infiltration #

Water may infiltrate through the surface if the pond is higher than `pond_max` at the beginning of the timestep, and the biopores reaches the soil surface..

```
  const double pond_max;         // Pond height before activating pref.flow [mm]
  double capacity (const Geometry&, size_t e, double dt); // Max flux.
  void infiltrate (const Geometry&, size_t e, double amount, double dt);
```

Related to this is the two functions `capacity` and `infiltrate`.  The first simply calculate how much "free space" there is in the tertiary domain, by accumulating the capacity of individual classes.  The second actually divide the incoming water among the classes.  The division is done relatively to the numeric biopore density, so that a class with twice as many biopores will receive twice as much water, at first independently of the size of the biopores.  However, care is also taken that biopore classes doesn't receive more than their capacity (which depend on the size), and that the extra water is divided among the remaining classes by the same rules.  Biopores that end in drain pipes have an infinite storage capacity in the model, but the infiltration is still limited by how fast the biopores can transport the water, which depends on the density and diameter.

# Small timestep interface #

The main function in the small timestep interface may be `find_implicit_water`.  The purpose of this function is to find the solution to
```
  h3_next = h3_prev + S (h3_next, h) * dt 
```
for `h3_next`, where `h3_next` is the tertiary water content at the end of the timestep, `h3_prev` is the tertiary water status at the start of the timestep, `h` is the matrix water content (assumed to be constant during the timestep), `dt` is the length of the timestep, and `S (h_next, h)` is the matrix sink term expected assuming `h3_next` and `h`.

We cannot solve this directly, so we use an iterative solution, where we start with an initial guess for `h3_next`, and then calculate a new guess for `h3_next` based on the right side of the equation.  We repeat the process until either the new and old guess is sufficiently close, or we have used too many iteration.

## State ##

The only state we care about is the water content in the biopores, and only the matrix biopore model has any water.  The states of the biopores are stored in an [auto\_vector](CodeMemutils.md), and the `get_state` and `set_state` functions simply delegates to biopores stored in `classes`.

An additional `converge` function allows the user to check if two states are sufficiently close.  All also delegates the real work to the biopore classes.

## Helper functions ##

The `find_implicit_water` function is build upon the state functions described above, and two helper functions.

```
  void update_biopores (const Geometry& geo, 
                        const Soil& soil,  
                        const SoilHeat& soil_heat, 
                        const std::vector<double>& h,
                        const double dt);
```

The `update_biopores` function calls `update_matrix_sink`, which does two things.  First, it calculates an matrix sink term called for all cells.  This sink term is stored in a log variable named `S`.  Secondly, it calculates how much water should be added to the tertiary domain based on `S` and `dt`.

```
  void update_water ();
```

The `update_water` function is delegated to the biopore classes.  It takes the water to be removed from the matrix via `S` term calculated by ```update_matrix_sink``, and adds it to the tertiary domain.  Or rather, so it does for biopore classes that ends in the matrix, it does nothing for biopore classes that ends in drains, as the water removed by these biopores are considered lost to the system.

The big trick to `find_implicit_water` is that the biopore state is reset the state of the beginning of the timestep, between the calls to `update_biopore` and `update_water`.  This ensures that the new guess for `h3_next` is based on the value of `h3_prev`, plus the sink we got based on the previous guess for `h3_next`.

## Matrix sink ##

The `matrix_sink` function call `add_matrix_sink` for all biopore classes, which will add the `S` term calculated by `update_matrix_sink` to either `S_matrix` or `S_drain`, depending on the biopore model.

# Large timestep interface to water #

The `tick` functions will calculate infiltration from surface for all cell near the top, using the `capacity` and `infiltrate` functions. The `tick` function then has to calculate the interaction between the matrix and tertiary domains.  Since this interaction is often far to fast to do for a large timestep, the `tick` function will cheat and pretend the timestep is smaller.  The effect of this is that tertiary transport will be much slower than what our understanding of the physics dictates.  But it will still bypass the matrix, and thus preserve one important aspect.

First, `find_implicit_water` is called repeatedly with decreasing values of `ddt`, until we wind a timestep small enough to get a solution.

Then the resulting matrix sink term is examined, and `ddt` is further adjusted so that the amount of water removed from an active cell will never put the pressure below `pressure_end`.  Note that the cell will continue to be active, so if additional water is added through matrix flow, it may be removed in the next large timestep.

Finally, `find_implicit_water` is called again with the adjusted value of `ddt` to find the final tertiary state, and the resulting source terms.