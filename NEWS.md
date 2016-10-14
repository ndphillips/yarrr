# yarrr 0.0.9

* Added a `NEWS.md` file to track changes to the package.

* Added `quant`, `quant.length` and `quant.width` arguments that add horizontal lines for specified quantiles to each bean (thanks @pat-s)

# yarrr 0.0.7

* `pirateplot()` can now handle up to 3 IVs!  
Example: `pirateplot(age ~ sex + headband + favorite.pirate, data = pirates)`.   
Levels of the third IV are shown in separate plots in a grid.

Minor and Bug-fixes

- The `inf.p` parameter in `pirateplot()` was prevously not being passed to the Bayesian HDIs, rendering all inference bands to be the default of 95% (thanks to Roman Pahl for catching this). 
This has now been fixed.

# yarrr 0.0.6

* Added `hdi.band` argument to `pirateplot()`. Setting `hdi.band = "tight"` will constrain inference bands to bean densities.
* Minor changes to gridlines when specifying `gl.col`.



