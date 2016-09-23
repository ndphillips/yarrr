# yarrr
YaRrr Package

The `yarrr` package contains a mixture of data, functions and tutorials supporting the e-book YaRrr! The Pirate's Guide to R (www.thepiratesguidetor.com). 

To install the package from github, run the following code

```
install("devtools") # Install the devtools package to get the install_github function
devtools::install_github("ndphillips/yarrr", build_vignette = T) # Install the yarrr package
library("yarrr") # Load the package
```


Here are the most important parts of the package:

## pirateplot()

![Pirateplot Example](http://nathanieldphillips.com/wp-content/uploads/2016/08/ppExample.png)

The `pirateplot` function creates a pirateplot, a transparent (both literally and figuratively) plot for displaying continuous data as a function of 1 (or 2) discrete variables. Unlike traditional plots, like barplots and boxplots, the pirateplot shows both raw data (jittered points), descriptive statistics (line and/or bar), and inferential statistics (95% Bayesian Highest Density Intervals or Confidence Intervals), in one plot. While the default plot shows all these elements, the user can easily customize the transparency of each element using additional arguments. 

For example, here is a pirateplot of the weight of chickens (from the `ChickWeight` dataset)

`yarrr::pirateplot(formula = weight ~ Time, data = ChickWeight)`

![ChickenWeight pirateplot](http://nathanieldphillips.com/wp-content/uploads/2016/08/chickenplot.png)

See http://rpubs.com/yarrr/pirateplot for details.


## Updates

0.0.7

- `pirateplot()` can now handle up to 3 IVs! (e.g.; `pirateplot(age ~ sex + headband + favorite.pirate, data = pirates)`. Levels of the third IV are shown in separate plots in a grid.

Bug-fixes

- The `inf.p` parameter in `pirateplot()` was prevously not being passed to the Bayesian HDIs, rendering all inference bands to be the default of 95% (thanks to Roman Pahl for catching this). This has now been fixed

0.0.6

- Added `hdi.band` argument to `pirateplot()`. Setting `hdi.band = "tight"` will constrain inference bands to bean densities
- Minor changes to gridlines when specifying `gl.col`
