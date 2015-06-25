Daisy includes a 2D root density model for row crops, based on Gerwitz and Page (1977). The model is quite simple, where Gerwitz and Pages showed a exponential decrease in root density with depth
```
    L(z) = L0 * exp(-a * z)
```
the 2D extension postulates a similar decrease with horizontal distance from the row
```
    L(x, z) = L00 * exp(-ax * |x|) * exp(-az * z)
```
where `L` is the root density, `x` is horizontal position with zero at the row, `z` is depth below surface, and `L0`, `L00`, `a`, `ax`, and `az` are constants.

Daisy will calculate the constants given information row geometry, specific root length, root density at the edge of the root zone, the width and depth of the root zone, and the total root mass. The root mass and the size of the root zone are dynamic in the model, the rest are static parameters. This is described in a [scientific article](GP2Dpaper.md).

These calculations are also available in static form in a [Excel sheet](GP2Dsheet.md).  This sheet can be used for either estimating the spatial distribution of root density from the root zone size and total root mass, or alternatively, for estimating root zone size and total root mass from measurements of root density.

Alternatively, you can use Daisy to calculate a static solution. You can find an example in the text file '[test-GP2D.dai](http://code.google.com/p/daisy-model/source/browse/trunk/sample/test-GP2D.dai)' in the 'sample' directory distributed together with Daisy.  The file should be self explaining. You run the example by dragging the file to the daisyw executable in the bin directory.  Or more comfortable from TextPad. Using Daisy additionally allows you to specify a soil limit for the root penetration.

Daisy can also perform inverse modeling based on root density measurements. Again, a slightly more advanced model including soil limited root penetration is performed. Additionally, Daisy will calculate parameters for an 1D model, and perform an F-test to see if the 2D model is significantly better. The file '[rootdata.ddf](http://code.google.com/p/daisy-model/source/browse/trunk/sample/rootdata.ddf)' shows the format expected for the file with root density measurments, and '[rootmatch.dai](http://code.google.com/p/daisy-model/source/browse/trunk/sample/rootmatch.dai)' file contains an self explained example. Both can be found in the 'sample' directory. Running the example will produce the following output:
  * A comparison of the measured and modeled root densities for the 2D model.
  * A comparison of the measured and modeled root densities for the 1D model.
  * The coefficient of determination for the 2D model.
  * The estimated parameters for the 2D model.
  * The coefficient of determination for the 1D model.
  * The estimated parameters for the 1D model.
  * The values for the F test.