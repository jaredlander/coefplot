# Version 1.2.5
Added `coefpath` to plot the coefficient path resulting from a `glmnet` object.
`coefplot` now displays the coeffcients from a linear `xgboost` model.

# Version 1.2.4
Patched to accommodate changes to ggplot2.

# Version 1.2.3
Can run coefplot on a data.frame that is properly setup like on resulting from coefplot(..., plot=FALSE).

# Version 1.2.2
Support for glmnet models.  Added tests.

# Version 1.2.1
In mulitplot there is now an option to reverse the order of the legend so it matches the ordering in the plot.

# Version 1.2.0
Major update.  The codebase has been refactored to run faster and be easily updated for improvements and adding new models.
New arguments (predictors and coefficients) for specifying coefficients to be plotted both by indicating the specific coefficients (including specific factor levels) or by specifying the predictors that created coefficients.  This is particularly important for factor variables and interactions.
New argument (newNames) for giving new names to coefficients.
Mulitplot can be used to plot Andy Gelman's secret weapon by setting secret.weapon to TRUE and selecting just one coefficient in coefficients.  For a vertical secret weapon set by to "Model" and horizontal to FALSE.
Faceting a single model no longer works.


#Version 1.1.9
Refactoring of code to make new models easier to add.
For now this means certain functionality will be lost, such as the shortening of coefficient names, plot a factor variable numerically.

# Version 1.1.8
Minor changes to plotting to reflect change in gpplot2_0.9.2.

# Version 1.1.7
Thanks to Felipe Carrillo I have fixed a bug in multiplot.  Previously, if multiple models with the same formula but different data.frames were inputed then they would all have the same name (even if specified with the "names" argument) and only one model would be plotted.  This now works as expected, plotting all the models regardless of identical formulas.

# Version 1.1.6
Made change to reshape2::melt so that variable.name and value.name behave properly with new ggplot2

# Version 1.1.5
Fixed glitch that didn't plot inner CI for single plots or multipane multiplots

# Version 1.1.4
Added functionality for rxLogit

# Version 1.1.2
multiplot can plot in a single pane or in facets
Changed sort argument to use match.arg

# Version 1.1.1
Adjusting the S3 dispatch for coefplot.rxLinMod

# Version 1.1
Added multiplot to plot numerous models at once
Moved a lot of code in coefplot.lm to their own functions

# Version 1.0
First build
Methods for plotting coefficients from lm, glm and rxLinMod