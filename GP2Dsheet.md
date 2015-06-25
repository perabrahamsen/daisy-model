The Excel sheet contains an implementation of the [GP2D](GP2D.md) root density model for row crops.

[Fetch Excel sheet here.](http://daisy-model.googlecode.com/files/GP2Dv0.xls)

The physical model parameters are marked yellow. By entering them, the sheet will calculate the mathematical model parameters, marked green. Furthermore, it will update two graphs, both showing root density as a function of horizontal distance for a row at different depths. The top graph shows only the density of roots from the row itself, while the bottom graph shows the total density from all rows.

The main input parameters are the root zone geometry, defined by a `depth` and `width`. The `depth` is the vertical distance from the soil surface where the root density has fallen to `DensRootTip`, and the `width` is the horizontal distance between the two points at each side of the row where the root density has fallen to `DensRootTip`. The `row distance` is the distance between two rows.  The root `dry matter` is the total dry mass of the roots. Finally, `SpRootLength` is the factor for converting root dry matter to root length.

The blue cells at the right side of the sheet are intended for measured values. The pink cell is the coefficient of determination. You can either manually adjust the yellow input parameters to get a better match, or you can use the Solver function in excel. We suggest you optimize `depth`, `width`, and `dry matter`.