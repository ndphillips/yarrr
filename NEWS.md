# yarrr 0.1.5

* Minor changes to `theme = 1` in `pirateplot()`. Changed default value of `cap.beans` to `TRUE`

* Added color mixing arguments `mix.col` and `mix.p` to `piratepal()`. These allow you to mix the default palettes with a specified color (e.g.; `"white"`)

* Added the option to specify data in `pirateplot()` as a list of numeric vectors, or as a numeric dataframe or matrix without specifying a formula. Each column / element will be taken as a new group.

* New palettes in `piratepal()`: `decision`.

* Fixed bug in `sortx` in `pirateplot()`. Sorting data by functions (e.g. `sortx = "mean"`) should now work.

* Added `gl` argument to `pirateplot()` to specify locations of gridlines (e.g.; `gl = seq(0, 10, 1)`)

* Added `cex.names` argument to control size of bean names (currently this was controlled by `cex.lab`, which now controls the size of the axis names.)

# yarrr 0.1.4

* Some minor changes to default plotting parameters that I think make the default plots look a bit nicer.

* Added `cap.beans` argument to `pirateplot()`. When `cap.beans = TRUE`, beans will be cut at the maximum and minimum values of the data.

* Added `cap.beans` argument to `pirateplot()`. When `cap.beans = TRUE`, beans will be cut at the maximum and minimum values of the data.

* Added two new `inf.method` values: `sd` for standard deviation, and `se` for standard error

# yarrr 0.1.2

* Minor updates to themes. Added `theme = 3`

* You can now assign a pirateplot to a variable to return summary statistics.

# yarrr 0.1.0

* Added a `NEWS.md` file to track changes to the package.

* Re-structured code generating colors and opacities in `pirateplot()` to make future updates easier.

* Added `quant`, `quant.length` and `quant.width` arguments that add horizontal lines for specified quantiles to each bean (thanks @pat-s)

* Added several new arguments (e.g.;  `bean.fill.col` for customising pirateplots

* Improved theme support (now under `theme` rather than `theme.o`)


# yarrr 0.0.7

* `pirateplot()` can now handle up to 3 IVs!  
Example: `pirateplot(age ~ sex + headband + favorite.pirate, data = pirates)`.   
Levels of the third IV are shown in separate plots in a grid.

Minor and Bug-fixes

- The `inf.p` parameter in `pirateplot()` was prevously not being passed to the Bayesian HDIs, rendering all inference bands to be the default of 95% (thanks to Roman Pahl for catching this). This has now been fixed.

# yarrr 0.0.6

* Added `hdi.band` argument to `pirateplot()`. Setting `hdi.band = "tight"` will constrain inference bands to bean densities.
* Minor changes to gridlines when specifying `gl.col`.



