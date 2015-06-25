Daisy is a mechanistic simulation model of the physical and biological processes in an agricultural field.  It traces the fate of water, energy, carbon, nitrogen, and pesticides, both above and below ground.  The model is able to predict production, environmental impact in the form of leaching, and change in soil (carbon) quality over time.  Another common use is as an upper boundary for a groundwater model.

The input to Daisy is daily or hourly weather data (at least precipitation, global radiation, and temperature, much more can be used if available), management information (sow/harvest, tillage operations, as well as data and amounts of irrigation, fertilizer and pesticide applications), and finally soil quality (texture, humus content).

The timescale go from hourly fluxes to changes in soil carbon pools over centuries.

The default one dimensional Daisy transport model assumes homogeneous fields, with no significant horizontal transport.  An optional two dimensional transport model exists is included, and has been used for simulating row crops and drain pipes.  To model larger areas, Daisy should be coupled with a GIS system.

The input to Daisy is through text files.  Output is also text files, with the exception of simple progress messages during the simulation.  The code can be accesses from a command line utility, a simple GUI (Qt based), or from another program through C, C++, C# or [OpenMI](OpenMI.md) interfaces.

Daisy is developed by members of the [Agrohydrology](http://plen.ku.dk/english/research/env_chem_phys/agrohydrology/) research group at the [Section for Environmental Chemistry and Physics](http://plen.ku.dk/english/research/env_chem_phys/) at the [Department for Plant and Environmental Sciences](http://plen.ku.dk/) at the [Faculty of Science](http://www.science.ku.dk/) at [University of Copenhagen](http://www.ku.dk/).  The primary responsible scientist is [Søren Hansen](SorenHansen.md).

See [the Daisy bibliography](Literature.md) for additional (mostly external) references.