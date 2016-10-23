
<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Build Status](https://travis-ci.org/ndphillips/yarrr.svg?branch=master)](https://travis-ci.org/ndphillips/yarrr) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/yarrr)](https://CRAN.R-project.org/package=yarrr) [![Rdoc](http://www.rdocumentation.org/badges/version/yarrr)](http://www.rdocumentation.org/packages/yarrr) [![Downloads](http://cranlogs.r-pkg.org/badges/yarrr?color=brightgreen)](http://www.r-pkg.org/pkg/yarrr)

yarrr
=====

YaRrr Package

The `yarrr` package contains a mixture of data, functions and tutorials supporting the e-book "YaRrr! The Pirate's Guide to R" (www.thepiratesguidetor.com).

To install yarrr (0.1.1) from CRAN, run the following code

``` r
install.packages("yarrr") # install yarrr 0.1.1
library("yarrr") # load yarrr
yarrr.guide() # run main package guide
```

Here are the most important parts of the package:

pirateplot()
------------

![Pirateplot Example](http://nathanieldphillips.com/wp-content/uploads/2016/08/ppExample.png)

The `pirateplot` function creates a pirateplot, a transparent (both literally and figuratively) plot for displaying continuous data as a function of 1, 2, or 3 discrete variables. Unlike traditional plots, like barplots and boxplots, the pirateplot shows both raw data (jittered points), descriptive statistics (line and/or bar), and inferential statistics (95% Bayesian Highest Density Intervals or Confidence Intervals), in one plot. While the default plot shows all these elements, the user can easily customize the transparency of each element using additional arguments.

For example, here is a default pirateplot of the weight of chickens (from the `ChickWeight` dataset)

``` r
pirateplot(formula = weight ~ Time, 
          data = ChickWeight,
          theme = 2,
          main = "pirateplot of Chicken Weights",
          back.col = gray(.97),
          gl.col = gray(.5))

```

![ChickenWeight pirateplot](https://dl.dropboxusercontent.com/u/7618380/chickenplot.png)

Here's an alternative pirateplot of the same data using `theme = 2` and some additional arguments

``` r
pirateplot(formula = weight ~ Time, 
           data = ChickWeight,
           main = "Chicken Weights",
           theme = 2,
           back.col = gray(.97),
           gl.col = gray(.5))
```

![ChickenWeight pirateplot](https://dl.dropboxusercontent.com/u/7618380/chickentheme2.png)

See `?pirateplot` or <https://cran.r-project.org/web/packages/yarrr/vignettes/pirateplot.html> for more details
