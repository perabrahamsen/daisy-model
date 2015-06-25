# Introduction #

The basic idea behind partial rootzone drying (PRD) is that by keeping part of the rootzone dry, the plant will generate water stress hormones (ABA), which will cause the leaf stomata to close, which will reduce both transpiration and production.  For low concentrations, the reductions in transpiration will be much larger than the reduction in production, making it desirable in situation where water supply for irrigation is limited.
In practice, one divide the root zone for each plant in two side, and keep one side dry and the other wet, by selective irrigation.  The two sides must be switched during the irrigation period, to keep the effect working.

To support this in Daisy, a number of new models have been introduced:

  * Farquhar-Ball photosynthesis.
  * ABA effect on photosynthesis.
  * ABA production in the soil.
  * A 2D description of the soil.
  * A [2D description of the root distribution](GP2D.md).
  * A [SVAT](SVAT.md) model linking transpiration to photosynthesis.
  * Initial growth should be [fueled by applied seeds](Daisy436.md).

In Daisy version 4.31, a setup file named "test-prd.dai" can be found in the "sample" directory.  The file demonstrates how to enable all these models in order to simulate a PRD experiment.

This work was funded by the [SAFIR](SAFIR.md) project, where the work has been documented.

# Long terms goals #

These requires addition funding:

  1. The user interface for specifying PRD should be simplified.
  1. Better ABA effect models.