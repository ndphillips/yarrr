
<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Build Status](https://travis-ci.org/ndphillips/yarrr.svg?branch=master)](https://travis-ci.org/ndphillips/yarrr) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/yarrr)](http://cran.r-project.org/package=yarr) [![Rdoc](http://www.rdocumentation.org/badges/version/yarrr)](http://www.rdocumentation.org/packages/yarrr) [![Downloads](http://cranlogs.r-pkg.org/badges/oddsratio?color=brightgreen)](http://www.r-pkg.org/pkg/oddsratio)

yarrr
=====

YaRrr Package

The `yarrr` package contains a mixture of data, functions and tutorials supporting the e-book YaRrr! The Pirate's Guide to R (www.thepiratesguidetor.com).

To install the (stable) version from CRAN, run the following code

``` r
install.packages("yarrr") # install yarrr
library("yarrr") # load yarrr
yarrr.guide() # run main package guide
```

Here are the most important parts of the package:

pirateplot()
------------

![Pirateplot Example](http://nathanieldphillips.com/wp-content/uploads/2016/08/ppExample.png)

The `pirateplot` function creates a pirateplot, a transparent (both literally and figuratively) plot for displaying continuous data as a function of 1, 2, or 3 discrete variables. Unlike traditional plots, like barplots and boxplots, the pirateplot shows both raw data (jittered points), descriptive statistics (line and/or bar), and inferential statistics (95% Bayesian Highest Density Intervals or Confidence Intervals), in one plot. While the default plot shows all these elements, the user can easily customize the transparency of each element using additional arguments.

For example, here is a pirateplot of the weight of chickens (from the `ChickWeight` dataset)

``` r
yarrr::pirateplot(formula = weight ~ Time, data = ChickWeight)
```

![ChickenWeight pirateplot](http://nathanieldphillips.com/wp-content/uploads/2016/08/chickenplot.png)

See <http://rpubs.com/yarrr/pirateplot> for details.
