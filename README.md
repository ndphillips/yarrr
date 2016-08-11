# yarrr
YaRrr Package

The `yarrr` package contains a mixture of data, functions and tutorials supporting the e-book YaRrr! The Pirate's Guide to R (www.thepiratesguidetor.com). 

To install the package from github, run the following code

`install("devtools") # Install the devtools package to get the install_github function`
`install_github("ndphillips/yarrr", build_vignette = T) # Install the yarrr package`
`library("yarrr")` # Load the package


Here are the most important parts of the package:

## pirateplot()




![Pirateplot Example](http://nathanieldphillips.com/wp-content/uploads/2016/08/ppExample.png)

The `pirateplot` function creates a pirateplot, a transparent (both literally and figuratively) plot for displaying continuous data as a function of 1 (or 2) discrete variables. Unlike traditional plots, like barplots and boxplots, the pirateplot shows both raw data (jittered points), descriptive statistics (line and/or bar), and inferential statistics (95% Bayesian Highest Density Intervals or Confidence Intervals), in one plot. While the default plot shows all these elements, the user can easily customize the transparency of each element using additional arguments. See http://rpubs.com/yarrr/pirateplot for details.


