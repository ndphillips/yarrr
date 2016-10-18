#' pirateplot
#'
#' The pirateplot function creates an RDI (Raw data, Descriptive and Inferential statistic) plot showing the relationship between 1 - 3 categorical independent variables and 1 continuous dependent variable. The plot shows all raw data (shown as points), smoothed densities (shown as beans), and inferential statistics such as 95% Bayesian Highest Density Intervals (shown as bands).
#'
#' @param formula formula. A formula in the form \code{y ~ x1 + x2 + x3} indicating the vertical response variable (y) and up to three independent varaibles
#' @param data dataframe. A dataframe containing the variables specified in formula.
#' @param pal string. The color palette of the plot. Can be a single color, a vector of colors, or the name of a palette in the piratepal() function (e.g.; "basel", "google", "southpark"). To see all the palettes, run \code{piratepal(palette = "all", action = "show")}
#' @param point.col,bar.f.col,bean.b.col,bean.f.col,inf.f.col,inf.b.col,avg.line.col,bar.b.col,quant.col,point.bg string. Vectors of colors specifying the colors of the plotting elements. This will override values in the palette. f stands for filling, b stands for border.
#' @param theme integer. An integer in the set 0, 1, 2 specifying a theme (that is, new default values for opacities and colors). \code{theme = 0} turns off all opacities which can then be individually specified individually.
#' @param bar.f.o,point.o,inf.f.o,inf.b.o,avg.line.o,bean.b.o,bean.f.o,bar.b.o numeric. A number between 0 and 1 indicating how opaque to make the bars, points, inference band, average line, and beans respectively. These values override whatever is in the specified theme
#' @param avg.line.fun function. A function that determines how average lines and bar heights are determined (default is mean).
#' @param gl.col,back.col string. The color of the horizontal gridlines and plotting background.
#' @param point.cex,point.pch,point.lwd numeric.  The size, pch type, and line width of raw data points.
#' @param bean.lwd,bean.lty,inf.lwd,avg.line.lwd,bar.lwd numeric. Vectors of numbers customizing the look of beans and lines.
#' @param width.min,width.max numeric. The minimum/maximum width of the beans.
#' @param cut.min,cut.max numeric. Optional minimum and maximum values of the beans.
#' @param inf string. A string indicating what types of inference bands to calculate. "ci" means frequentist confidence intervals, "hdi" means Bayesian Highest Density Intervals (HDI), "iqr" means interquartile range.
#' @param inf.band string. Either \code{"wide"} to indicate a fixed width band, or \code{"tight"} to indicate a band constrained by the bean
#' @param inf.p numeric. A number between 0 and 1 indicating the level of confidence to use in calculating inferences for either confidence intervals or HDIs. The default is 0.95
#' @param hdi.iter integer. Number of iterations to run when calculating the HDI. Larger values lead to better estimates, but can be more time consuming.
#' @param bw,adjust Arguments passed to density calculations for beans (see ?density)
#' @param jitter.val numeric. Amount of jitter added to points horizontally. Defaults to 0.05.
#' @param at integer. Locations of the beans. Especially helpful when adding beans to an existing plot with add = T
#' @param sortx string. How to sort the x values. Can be "sequential" (as they are found in the original dataframe), "alphabetical", or a string indicating a function (i.e.; "mean")
#' @param add logical. Whether to add the pirateplot to an existing plotting space or not.
#' @param evidence logical. Should Bayesian evidence be shown? (currently ignored)
#' @param quant.length,quant.lwd numeric. Specifies line lengths/widths of \code{quant}.
#' @param family a font family (Not currently in use)
#' @param cex.lab,cex.axis Size of the labels and axes.
#' @param gl.lwd,gl.lty Customization for grid lines.
#' @param bty,xlim,ylim,xlab,ylab,main,yaxt,xaxt General plotting arguments
#' @param quant numeric. Adds horizontal lines representing custom quantiles.
#' @param bar.b.lwd,line.fun,inf.o,bean.o,inf.col,theme.o depricated arguments
#' @param ... other arguments passed on to the plot function (e.g.; main, xlab, ylab, ylim, cex.axis, cex.main, cex.lab)
#' @keywords plot
#' @importFrom BayesFactor ttestBF
#' @importFrom grDevices col2rgb gray rgb
#' @importFrom graphics abline axis layout mtext par plot points polygon rasterImage rect segments text
#' @importFrom stats density model.frame optimize rnorm t.test qbeta sd quantile
#' @importFrom utils vignette
#' @export
#' @examples
#'
#'
#'# Default pirateplot of weight by Time
#'pirateplot(formula = weight ~ Time,
#'           data = ChickWeight)
#'
#'# Black and white version
#'pirateplot(formula = weight ~ Time,
#'           data = ChickWeight,
#'           main = "Chicken weights by Time",
#'           pal = gray(.2)) # Dark gray palette
#'
#'# Now using theme 2
#'pirateplot(formula = weight ~ Time,
#'           data = ChickWeight,
#'           main = "Chicken weight by time",
#'           theme = 2, # Turn off all elements
#'           gl.col = "gray") # gray gridlines
#'
#'
#'# Start with theme 2, but then customise!
#'pirateplot(formula = weight ~ Time,
#'           data = ChickWeight,
#'           theme = 2, # theme 2
#'           pal = "xmen", # xmen palette
#'           main = "Chicken weights by Time",
#'           point.o = .4, # Add points
#'           point.col = "black",
#'           point.bg = "white",
#'           point.pch = 21,
#'           bean.f.o = .2, # Turn down bean filling
#'           inf.f.o = .8, # Turn up inf filling
#'           gl.col = "gray", # gridlines
#'           gl.lwd = c(.5, 0)) # turn off minor grid lines
#'
#'# Theme 2 with 2 IVs
#'pirateplot(formula = len ~ dose + supp,
#'           data = ToothGrowth,
#'           theme = 2,
#'           main = "Guinea pig tooth length by supplement",
#'           point.pch = 16,  # Point specifications...
#'           point.col = "black",
#'           point.o = .7,
#'           inf.f.o = .9, # inference band opacity
#'           gl.col = "gray")
#'
#'
#'# Build everything from scratch with theme 0
#'#  And use 3 IVs
#'pirateplot(formula = height ~ headband + eyepatch + sex,
#'           data = pirates,
#'           pal = gray(.1), # Dark gray palette
#'           theme = 0, # Start from scratch
#'           inf.f.o = .7, # Band opacity
#'           inf.f.col = piratepal("basel"), # Add color to bands
#'           point.o = .1, # Point opacity
#'           avg.line.o = .8, # Average line opacity
#'           gl.col = gray(.6), # Gridline specifications
#'           gl.lty = 1,
#'           gl.lwd = c(.5, 0))
#'
#'   # See the vignette for more details
#'  vignette("pirateplot", package = "yarrr")
#'
#'

pirateplot <- function(
  formula = NULL,
  data = NULL,
  avg.line.fun = mean,
  pal = "basel",
  back.col = NULL,
  point.cex = NULL,
  point.pch = 16,
  point.lwd = 1,
  jitter.val = .03,
  theme = 1,
  bean.b.o = NULL,
  bean.f.o = NULL,
  point.o = NULL,
  bar.f.o = NULL,
  bar.b.o = NULL,
  inf.f.o = NULL,
  inf.b.o = NULL,
  avg.line.o = NULL,
  gl.col = NULL,
  point.col = NULL,
  point.bg = NULL,
  bar.f.col = NULL,
  bean.b.col = NULL,
  bean.f.col = NULL,
  inf.f.col = NULL,
  inf.b.col = NULL,
  avg.line.col = NULL,
  bar.b.col = NULL,
  quant.col = NULL,
  avg.line.lwd = 4,
  bean.lwd = 1,
  bean.lty = 1,
  inf.lwd = 1,
  bar.lwd = 1,
  at = NULL,
  bw = "nrd0",
  adjust = 1,
  add = FALSE,
  sortx = "alphabetical",
  cex.lab = NULL,
  cex.axis = 1,
  quant = NULL,
  quant.length = NULL,
  quant.lwd = NULL,
  bty = "n",
  evidence = FALSE,
  family = NULL,
  inf = "hdi",
  inf.p = .95,
  hdi.iter = 1e3,
  inf.band = "wide",
  cut.min = NULL,
  cut.max = NULL,
  width.min = .3,
  width.max = NA,
  ylim = NULL,
  xlim = NULL,
  xlab = NULL,
  ylab = NULL,
  main = NULL,
  yaxt = NULL,
  xaxt = NULL,
  gl.lwd = NULL,
  gl.lty = NULL,
  bar.b.lwd = NULL,
  line.fun = NULL,
  inf.o = NULL,
  bean.o = NULL,
  inf.col = NULL,
  theme.o = NULL,
  ...
) {

# -----
#  SETUP
# ------

# Check for depricated arguments
{
if(is.null(bar.b.lwd) == FALSE) {

  message("bar.b.lwd is depricated. Use bar.lwd instead")

  bar.lwd <- bar.b.lwd
}

if(is.null(line.fun) == FALSE) {

  message("line.fun is depricated. Use avg.line.fun instead")

  avg.line.fun <- line.fun

}

if(is.null(inf.o) == FALSE) {

  message("inf.o is depricated. Use inf.f.o instead")

  inf.f.o <- inf.o

}

if(is.null(bean.o) == FALSE) {

  message("bean.o is depricated. Use bean.b.o instead")

  bean.b.o <- bean.o

}

if(is.null(inf.col) == FALSE) {

  message("inf.col is depricated. Use inf.f.col instead")

  inf.f.col <- inf.col

}

if(is.null(theme.o) == FALSE) {

  message("theme.o is depricated. Use theme instead")

  theme <- theme.o

}
}

# Look for missing critical inputs
{
if(is.null(data)) {stop("You must specify a dataframe in the data argument!")}
if(is.null(formula) | class(formula) != "formula") {stop("You must specify a valid formula in the formula argument!")}
}

# Reshape dataframe to include relevant variables
{
data <- model.frame(formula = formula,
                      data = data)

dv.name <- names(data)[1]
dv.v <- data[,1]
}

# GET IV INFORMATION
{
n.iv <- ncol(data) - 1
if(n.iv > 3) {stop("Currently only 1, 2, or 3 IVs are supported in pirateplot(). Please reduce.")}

# selection.mtx dictates which values are in each sub-plot

if(n.iv %in% 1:2) {

selection.mtx <- matrix(TRUE, nrow = nrow(data), ncol = 1)

}

if(n.iv == 3) {

  iv3.levels <- sort(unique(data[,4]))
  selection.mtx <- matrix(unlist(lapply(iv3.levels, FUN = function(x) {data[,4] == x})), nrow = nrow(data), ncol = length(iv3.levels), byrow = F)

}

n.subplots <- ncol(selection.mtx)




# Loop over subplots (only relevant when there is a third IV)
if(n.subplots == 2) {par(mfrow = c(1, 2))}
if(n.subplots == 3) {par(mfrow = c(1, 3))}
if(n.subplots == 4) {par(mfrow = c(2, 2))}
if(n.subplots %in% c(5, 6)) {par(mfrow = c(2, 3))}
if(n.subplots > 7) {par(mfrow = c(ceiling(sqrt(n.subplots)), ceiling(sqrt(n.subplots))))}

}

# Loop over subplots
for(subplot.i in 1:n.subplots) {

# Select data for current subplot
data.i <- data[selection.mtx[,subplot.i],]

# Remove potential iv3 column
data.i <- data.i[,1:min(ncol(data.i), 3)]

# Determine levels of each IV
{
if(substr(sortx, 1, 1) == "a") {

  iv.levels <- lapply(2:ncol(data.i),
                      FUN = function(x) {sort(unique(data.i[,x]))})

}

if(substr(sortx, 1, 1) == "s") {

  iv.levels <- lapply(2:ncol(data.i), FUN = function(x) {unique(data.i[,x])})

}

iv.lengths <- sapply(1:length(iv.levels), FUN = function(x) {length(iv.levels[[x]])})
iv.names <- names(data.i)[2:ncol(data.i)]
subplot.n.iv <- length(iv.levels)
}

# Set up bean info
{
bean.mtx <- expand.grid(iv.levels)
names(bean.mtx) <- names(data.i)[2:ncol(data.i)]
n.beans <- nrow(bean.mtx)
bean.mtx$bean.num <- 1:nrow(bean.mtx)

# Determine bean x locations

if(is.null(at)) {

bean.loc <- 1:n.beans

group.spacing <- 1

if(subplot.n.iv == 2) {

  bean.loc <- bean.loc + rep(group.spacing * (0:(iv.lengths[2] - 1)),
                             each = iv.lengths[1])
}

}

if(!is.null(at)) {

 bean.loc <- rep(at, length.out = n.beans)

}

bean.mtx$x.loc <- bean.loc
data.i <- merge(data.i, bean.mtx)

}

# COLORS AND TRANSPARENCIES

# Set number of colors to number of levels of the first IV
n.cols <- iv.lengths[1]

# DEFINE THEMES
{
if((theme %in% 0:2) == FALSE) {

  print("theme must be an integer between 0 and 2. I'll set it to 1 for now.")
  theme <- 1

}

if(theme == 0) {

  if(is.null(point.o)) {point.o <- 0}
  if(is.null(bean.b.o)) {bean.b.o <- 0}
  if(is.null(bean.f.o)) {bean.f.o <- 0}
  if(is.null(inf.f.o)) {inf.f.o <- 0}
  if(is.null(inf.b.o)) {inf.b.o <- 0}
  if(is.null(avg.line.o)) {avg.line.o <- 0}
  if(is.null(bar.f.o)) {bar.f.o <- 0}
  if(is.null(bar.b.o)) {bar.b.o <- 0}
  if(is.null(point.cex)) {point.cex <- 1}

}

if(theme == 1) {

  if(is.null(point.o)) {point.o <- .3}
  if(is.null(bean.b.o)) {bean.b.o <- 0}
  if(is.null(bean.f.o)) {bean.f.o <- .5}
  if(is.null(inf.f.o))  {inf.f.o <- .5}
  if(is.null(inf.b.o)) {inf.b.o <- 1}
  if(is.null(avg.line.o))  {avg.line.o <- 1}
  if(is.null(bar.f.o))  {bar.f.o <- 0}
  if(is.null(bar.b.o))  {bar.b.o <- 0}

  if(is.null(inf.f.col)) {inf.f.col <- "white"}
  if(is.null(inf.b.col)) {inf.b.col <-  "black"}
  if(is.null(avg.line.col)) {avg.line.col <- "black"}
  if(is.null(point.col)) {point.col <- "black"}
  if(is.null(point.cex)) {point.cex <- .5}

  if(is.null(back.col)) {back.col <- gray(.97)}
  if(is.null(gl.col)) {gl.col <- gray(.5)}

}

if(theme == 2) {

  if(is.null(point.o)) {point.o <- .2}
  if(is.null(bean.b.o)) {bean.b.o <- .2}
  if(is.null(bean.f.o)) {bean.f.o <- 0}
  if(is.null(inf.f.o)) {inf.f.o <- .8}
  if(is.null(inf.b.o)) {inf.b.o <- .8}
  if(is.null(avg.line.o)) {avg.line.o <- 1}
  if(is.null(bar.f.o)) {bar.f.o <- .1}
  if(is.null(bar.b.o)) {bar.b.o <- 0}
  if(is.null(point.cex)) {point.cex <- 1}
}

}

# DEFINE OPACITIES
{

opac.df <- data.frame(
  point.o = rep(NA, n.beans),
  bean.b.o = rep(NA, n.beans),
  bean.f.o = rep(NA, n.beans),
  inf.f.o = rep(NA, n.beans),
  inf.b.o = rep(NA, n.beans),
  avg.line.o = rep(NA, n.beans),
  bar.f.o = rep(NA, n.beans),
  bar.b.o = rep(NA, n.beans)
)
rownames(opac.df) <- 1:n.beans




# If opacity values are specified, update them.
if(is.null(point.o) == FALSE) {opac.df$point.o <- rep(point.o, length.out = n.beans)}
if(is.null(bean.b.o) == FALSE) {opac.df$bean.b.o <- rep(bean.b.o, length.out = n.beans)}
if(is.null(bean.f.o) == FALSE) {opac.df$bean.f.o <- rep(bean.f.o, length.out = n.beans)}
if(is.null(inf.f.o) == FALSE) {opac.df$inf.f.o <- rep(inf.f.o, length.out = n.beans)}
if(is.null(inf.b.o) == FALSE) {opac.df$inf.b.o <- rep(inf.b.o, length.out = n.beans)}
if(is.null(avg.line.o) == FALSE) {opac.df$avg.line.o <- rep(avg.line.o, length.out = n.beans)}
if(is.null(bar.f.o) == FALSE) {opac.df$bar.f.o <- rep(bar.f.o, length.out = n.beans)}
if(is.null(bar.b.o) == FALSE) {opac.df$bar.b.o <- rep(bar.b.o, length.out = n.beans)}

}

# DEFINE COLORS
{
colors.df <- data.frame(
  point.col = rep(NA, n.beans),
  point.bg = rep(NA, n.beans),
  bean.b.col = rep(NA, n.beans),
  bean.f.col = rep(NA, n.beans),
  inf.f.col = rep(NA, n.beans),
  inf.b.col = rep(NA, n.beans),
  avg.line.col = rep(NA, n.beans),
  bar.f.col = rep(NA, n.beans),
  bar.b.col = rep(NA, n.beans),
  quant.col = rep(NA, n.beans)
)
rownames(colors.df) <- 1:n.beans


# If palette is in piratepal()...
if(mean(pal %in% piratepal("names")) == 1) {

colors.df$point.col <- rep(piratepal(palette = pal, length.out = n.cols), length.out = n.beans)
colors.df$point.bg <- rep(piratepal(palette = pal, length.out = n.cols), length.out = n.beans)
colors.df$bean.b.col <- rep(piratepal(palette = pal, length.out = n.cols), length.out = n.beans)
colors.df$bean.f.col <- rep(piratepal(palette = pal, length.out = n.cols), length.out = n.beans)
colors.df$inf.f.col <- rep(piratepal(palette = pal, length.out = n.cols), length.out = n.beans)
colors.df$inf.b.col <- rep(piratepal(palette = pal, length.out = n.cols), length.out = n.beans)
colors.df$avg.line.col <- rep(piratepal(palette = pal, length.out = n.cols), length.out = n.beans)
colors.df$bar.f.col <- rep(piratepal(palette = pal, length.out = n.cols), length.out = n.beans)
colors.df$bar.b.col <- rep(piratepal(palette = pal, length.out = n.cols), length.out = n.beans)
colors.df$quant.col <- rep(piratepal(palette = pal, length.out = n.cols), length.out = n.beans)

}

# If palette is NOT in piratepal()...

if(mean(pal %in% piratepal("names")) != 1) {

colors.df$point.col <- rep(pal, length.out = n.beans)
colors.df$point.bg <- rep(pal, length.out = n.beans)
colors.df$bean.b.col <- rep(pal, length.out = n.beans)
colors.df$bean.f.col <- rep("white", length.out = n.beans)
colors.df$inf.f.col <- rep(pal, length.out = n.beans)
colors.df$inf.b.col <- rep(pal, length.out = n.beans)
colors.df$avg.line.col <- rep(pal, length.out = n.beans)
colors.df$bar.f.col <- rep(pal, length.out = n.beans)
colors.df$bar.b.col <- rep(pal, length.out = n.beans)
colors.df$quant.col <- rep(pal, length.out = n.beans)

}

# Apply specified colors

if(is.null(point.col) == FALSE) {colors.df$point.col <- rep(point.col, length.out = n.beans)}
if(is.null(point.bg) == FALSE) {colors.df$point.bg <- rep(point.bg, length.out = n.beans)}
if(is.null(bean.b.col) == FALSE) {colors.df$bean.b.col <- rep(bean.b.col, length.out = n.beans)}
if(is.null(bean.f.col) == FALSE) {colors.df$bean.f.col <- rep(bean.f.col, length.out = n.beans)}
if(is.null(inf.f.col) == FALSE) {colors.df$inf.f.col <- rep(inf.f.col, length.out = n.beans)}
if(is.null(inf.b.col) == FALSE) {colors.df$inf.b.col <- rep(inf.b.col, length.out = n.beans)}
if(is.null(avg.line.col) == FALSE) {colors.df$avg.line.col <- rep(avg.line.col, length.out = n.beans)}
if(is.null(bar.f.col) == FALSE) {colors.df$bar.f.col <- rep(bar.f.col, length.out = n.beans)}
if(is.null(bar.b.col) == FALSE) {colors.df$bar.b.col <- rep(bar.b.col, length.out = n.beans)}
if(is.null(quant.col) == FALSE) {colors.df$quant.col <- rep(quant.col, length.out = n.beans)}

}

# SETUP PLOTTING SPACE
{

# Determine margins
if(n.subplots == 1) {
  par(mar = c(5, 4, 4, 1) + .1)
  add.yaxt <- TRUE
  }
if(n.subplots %in% c(2, 3)) {

  if(subplot.i == 1) {

    par(mar = c(5, 4, 4, 1) + .1)
    add.yaxt <- TRUE

    }
  if(subplot.i > 1) {

    par(mar = c(5, 2, 4, 1) + .1)

    add.yaxt <- FALSE
    }


}
if(n.subplots == c(4)) {

  if(subplot.i %in% c(1, 3)) {

    par(mar = c(5, 4, 4, 1) + .1)} else {
    par(mar = c(5, 2, 4, 1) + .1)

  }

}
if(n.subplots %in% c(5, 6)) {

  if(subplot.i %in% c(1, 4)) {

    par(mar = c(5, 4, 4, 1) + .1)} else {
      par(mar = c(5, 2, 4, 1) + .1)

    }

}
if(n.subplots > 6) {

  if(subplot.i %in% seq(ceiling(sqrt(n.subplots)) + 1,
                        n.subplots + 1,
                        length.out = ceiling(sqrt(n.subplots)))) {

    par(mar = c(5, 4, 4, 1) + .1)} else {

      par(mar = c(5, 2, 4, 1) + .1)

    }

}

# Determine y limits (y axis limits)
# y axis breaks (y.levels)


if(is.null(ylim) == TRUE) {

  # Determine best step size

  steps.p <- c(
              seq(1e-3, 1e-2, 1e-3),
              seq(1e-4, 1e-3, 1e-3),
              seq(1e-5, 1e-4, 1e-4),
              seq(1e-6, 1e-5, 1e-5),
              seq(1e-7, 1e-6, 1e-6),
              seq(1e-8, 1e-7, 1e-7),
              seq(1e-9, 1e-8, 1e-8),
              1/2, 1/5, 1/10, 1/25, 1/50, 1/100,
               1, 2, 5, 10, 25, 50, 100,
               seq(1e2, 1e3, 1e2),
               seq(1e3, 1e4, 1e3),
               seq(1e4, 1e5, 1e4),
               seq(1e5, 1e6, 1e5),
               seq(1e6, 1e7, 1e6),
               seq(1e7, 1e8, 1e7),
               seq(1e8, 1e9, 1e8)
              )


  range <- max(dv.v) - min(dv.v)

  steps.p.m <- range / steps.p
  best.step.size <- min(steps.p[which(abs(steps.p.m - 10) == min(abs(steps.p.m - 10)))])

  plot.min <- floor(min(dv.v) / best.step.size) * best.step.size
  plot.max <- ceiling(max(dv.v) / best.step.size) * best.step.size
  plot.height <- plot.max - plot.min

  ylim <- c(plot.min, plot.max)
  y.levels <- seq(plot.min, plot.max, by = best.step.size)

}

if(is.null(ylim) == FALSE) {

  steps.p <- c(
    seq(1e-3, 1e-2, 1e-3),
    seq(1e-4, 1e-3, 1e-3),
    seq(1e-5, 1e-4, 1e-4),
    seq(1e-6, 1e-5, 1e-5),
    seq(1e-7, 1e-6, 1e-6),
    seq(1e-8, 1e-7, 1e-7),
    seq(1e-9, 1e-8, 1e-8),
    1/2, 1/5, 1/10, 1/25, 1/50, 1/100,
    1, 2, 5, 10, 25, 50, 100,
    seq(1e2, 1e3, 1e2),
    seq(1e3, 1e4, 1e3),
    seq(1e4, 1e5, 1e4),
    seq(1e5, 1e6, 1e5),
    seq(1e6, 1e7, 1e6),
    seq(1e7, 1e8, 1e7),
    seq(1e8, 1e9, 1e8)
  )

  range <- ylim[2] - ylim[1]

  steps.p.m <- range / steps.p
  best.step.size <- min(steps.p[which(abs(steps.p.m - 10) == min(abs(steps.p.m - 10)))])

  plot.min <- floor(ylim[1] / best.step.size) * best.step.size
  plot.max <- ceiling((max(dv.v) - plot.min)/  best.step.size) * best.step.size

  y.levels <- seq(ylim[1], ylim[2], by = best.step.size)

}

if(is.null(xlim)) {xlim <- c(min(bean.loc) - .5, max(bean.loc) + .5)}

# Determine x and y labels

if(subplot.n.iv == 1 & is.null(xlab)) {my.xlab <- iv.names[1]}
if(subplot.n.iv == 1 & is.null(xlab) == F) {my.xlab <- xlab}

if(subplot.n.iv > 1) {my.xlab <- ""}

if(is.null(ylab)) {ylab <- dv.name}
}

# PLOTTING SPACE
if(add == F) {

  plot(1,
       xlim = xlim,
       ylim = ylim,
       type = "n",
       xaxt = "n",
       yaxt = "n",
       xlab = my.xlab,
       ylab = ylab,
       main = main,
       yaxt = yaxt,
       bty = bty,
       ...
  )


# Add title for iv3

if(n.iv > 2) {

  top.text <- paste(names(data)[4], " = ", iv3.levels[subplot.i], sep = "")
  mtext(text = top.text, side = 3)

}

# Y-AXIS
{
if(is.null(yaxt)) {

axis(side = 2,
     at = y.levels,
     labels = prettyNum(y.levels, big.mark = ","),
     las = 1,
     lwd = 1,
     lwd.ticks = 1,
     cex.axis = cex.axis)

}
}

# BACKGROUND
{

if(is.null(back.col) == FALSE) {

rect(xleft = par("usr")[1],
     ybottom = par("usr")[3],
     xright = par("usr")[2],
     ytop = par("usr")[4],
     col = back.col,
     border = NA)

}
}

# GRIDLINES
{
if(is.null(gl.col) == F) {

  if(is.null(gl.lwd)) {gl.lwd <- c(1, .5)}
  if(is.null(gl.lty)) {gl.lty <- 3}

  abline(h = seq(min(y.levels), max(y.levels), length.out = length(y.levels) * 2 - 1),
         lwd = gl.lwd,
         col = gl.col,
         lty = gl.lty)
}
}
}

# PIRATEPLOT ELEMENTS
{

if(is.na(width.max)) {

  if(subplot.n.iv == 1) {width.max <- .45}
  if(subplot.n.iv == 2) {width.max <- .5}

}

bean.lwd <- rep(bean.lwd, length.out = n.beans)
bean.lty <- rep(bean.lty, length.out = n.beans)
inf.lwd <- rep(inf.lwd, length.out = n.beans)
avg.line.lwd <- rep(avg.line.lwd, length.out = n.beans)
bar.lwd <- rep(bar.lwd, length.out = n.beans)

# Loop over beans
for (bean.i in 1:n.beans) {

dv.i <- data.i[data.i$bean.num == bean.i, dv.name]

if(is.logical(dv.i)) {dv.i <- as.numeric(dv.i)}

x.loc.i <- bean.mtx$x.loc[bean.i]

# CALCULATE DENSITIES

if(length(dv.i) > 3) {  # only if n > 5

  dens.i <- density(dv.i, bw, adjust)

  dens.y.i <- dens.i$y
  dens.x.i <- dens.i$x

  # Rescale density according to width.max and width.min

  if(max(dens.y.i) < width.min) {

    dens.y.i <- dens.y.i / max(dens.y.i) * width.min

  }

  if(max(dens.y.i) > width.max) {

    dens.y.i <- dens.y.i / max(dens.y.i) * width.max

  }

  # adjust to cut.min and cut.max

  dens.x.plot.i <- dens.x.i
  dens.y.plot.i <- dens.y.i

  if(is.null(cut.min) == F) {

    dens.x.plot.i <- dens.x.i[dens.x.i > cut.min]
    dens.y.plot.i <- dens.y.i[dens.x.i > cut.min]

  }


  if(is.null(cut.max) == F) {

    dens.x.plot.i <- dens.x.i[dens.x.i < cut.max]
    dens.y.plot.i <- dens.y.i[dens.x.i < cut.max]

  }
}

# BAR
{
rect(xleft = x.loc.i - width.max,
     ybottom = 0,
     xright = x.loc.i + width.max,
     ytop = avg.line.fun(dv.i),
     col = transparent(colors.df$bar.f.col[bean.i], trans.val = 1 - opac.df$bar.f.o[bean.i]),
     border = transparent(colors.df$bar.b.col[bean.i], trans.val = 1 - opac.df$bar.b.o[bean.i]),
     lwd = bar.b.lwd[bean.i]
)
}

# BEAN
{

if(length(setdiff(dv.i, c(0, 1))) > 0 & length(dv.i) > 3) {

  polygon(c(x.loc.i - dens.y.plot.i[1:(length(dens.x.plot.i))],
            x.loc.i + rev(dens.y.plot.i[1:(length(dens.x.plot.i))])),
          c(dens.x.plot.i[1:(length(dens.x.plot.i))],
            rev(dens.x.plot.i[1:(length(dens.x.plot.i))])),
          col = transparent(colors.df$bean.f.col[bean.i],
                            trans.val = 1 - opac.df$bean.f.o[bean.i]),
          border = transparent(colors.df$bean.b.col[bean.i],
                               trans.val = 1 - opac.df$bean.b.o[bean.i]),
          lwd = bean.lwd[bean.i], lty = bean.lty[bean.i]
  )

}

}

# POINTS
{

# 1-color points
if((point.pch %in% 21:25) == FALSE) {
  points(x = rep(x.loc.i, length(dv.i)) + rnorm(length(dv.i), mean = 0, sd = jitter.val),
         y = dv.i,
         pch = point.pch,
         col = transparent(colors.df$point.col[bean.i],
                           trans.val = 1 - opac.df$point.o[bean.i]),
         cex = point.cex,
         lwd = point.lwd
  )
}

# 2-color points
if(point.pch %in% 21:25) {
points(x = rep(x.loc.i, length(dv.i)) + rnorm(length(dv.i), mean = 0, sd = jitter.val),
       y = dv.i,
       pch = point.pch,
       col = transparent(colors.df$point.col[bean.i],
                         trans.val = 1 - opac.df$point.o[bean.i]),
       bg = transparent(colors.df$point.bg[bean.i],
                         trans.val = 1 - opac.df$point.o[bean.i]),
       cex = point.cex,
       lwd = point.lwd
)
}

}

# QUANTILES
if (!is.null(quant)) {

  # set default line length if length is not given manually
  if (is.null(quant.length)) {
    quant.length <- c(rep(0.65, length(quant)))
  } else {quant.length <- rep(quant.length, length.out = length(quant))}
  if (is.null(quant.lwd)) {
    quant.lwd <- c(rep(0.75, length(quant)))
  } else {quant.lwd <- rep(quant.lwd, length.out = length(quant))}

  for (i in 1:length(quant)) {

    # draw lines
    segments(x.loc.i + (quant.length[i] - width.max), # left end
             quantile(dv.i, probs = quant[i]),
             x.loc.i - (quant.length[i] - width.max), # right end
             quantile(dv.i, probs = quant[i]),
             col =  colors.df$quant.col[bean.i],
             lwd = quant.lwd[i],
             lend = 3
    )
  }
}

# INFERENCE BAND
{
if(length(dv.i) > 3 & sd(dv.i) > 0) {

if(length(dv.i) <= 3) {

  message(paste("Note: Group ", bean.i, " had too few observations (", length(dv.i), ") for an inference band", sep = ""))
  message(paste("Note: Group", bean.i, "had no variance, so no inference band :("))

  }

# Binary data.i

if(length(setdiff(dv.i, c(0, 1))) == 0) {

if(inf == "hdi") {

  # Calculate HDI from beta(Success + 1, Failure + 1)
inf.lb <- qbeta(.025, shape1 = sum(dv.i) + 1, shape2 = sum(dv.i == 0) + 1)
inf.ub <- qbeta(.975, shape1 = sum(dv.i) + 1, shape2 = sum(dv.i == 0) + 1)

}

if(inf == "ci") {

  # Calculate 95% CI with Normal distribution approximation to binomial
  inf.lb <- mean(dv.i) - 1.96 * sqrt(mean(dv.i) * (1 - mean(dv.i)) / length(dv.i)) - .5 / length(dv.i)
  inf.ub <- mean(dv.i) + 1.96 * sqrt(mean(dv.i) * (1 - mean(dv.i)) / length(dv.i)) + .5 / length(dv.i)

  if(inf.lb < 0) {inf.lb <- 0}
  if(inf.lb > 1) {inf.ub <- 1}

}
}

# Non-Binary data.i
if(length(setdiff(dv.i, c(0, 1))) > 0) {

if(inf == "hdi") {

ttest.bf <- BayesFactor::ttestBF(dv.i, posterior = T, iterations = hdi.iter, progress = F)
samples <- ttest.bf[,1]

# using the hdi function from Kruschke

inf.lb <- hdi(samples, credMass = inf.p)[1]
inf.ub <- hdi(samples, credMass = inf.p)[2]

}

if(inf == "iqr") {

  inf.lb <- quantile(dv.i, probs = .25)
  inf.ub <- quantile(dv.i, probs = .75)

}

if(inf == "ci") {

ci.i <- t.test(dv.i, conf.level = inf.p)$conf.int

inf.lb <- ci.i[1]
inf.ub <- ci.i[2]

}
}

dens.inf.x <- dens.x.i[dens.x.i >= inf.lb & dens.x.i <= inf.ub]
dens.inf.y <- dens.y.i[dens.x.i >= inf.lb & dens.x.i <= inf.ub]

# Draw inf band

if(inf.band == "wide") {

rect(x.loc.i - width.max * .8,
     inf.lb,
     x.loc.i + width.max * .8,
     inf.ub,
     col = transparent(colors.df$inf.f.col[bean.i],
                       trans.val = 1 - opac.df$inf.f.o[bean.i]),
     lwd = inf.lwd[bean.i],
     border = transparent(colors.df$inf.b.col[bean.i],
                          trans.val = 1 - opac.df$inf.b.o[bean.i])
)

}

if(inf.band == "tight") {

  polygon(c(x.loc.i - dens.inf.y[1:(length(dens.inf.x))],
            x.loc.i + rev(dens.inf.y[1:(length(dens.inf.x))])),
          c(dens.inf.x[1:(length(dens.inf.x))],
            rev(dens.inf.x[1:(length(dens.inf.x))])),
          col = transparent(colors.df$inf.f.col[bean.i],
                            trans.val = 1 - opac.df$inf.f.o[bean.i]),
          border = transparent(colors.df$inf.b.col[bean.i],
                               trans.val = 1 - colors.df$inf.b.o[bean.i]),
          lwd = bean.lwd[bean.i]
  )
}
}

}

# AVERAGE LINE
{

  if(inf.band == "wide") {
    segments(x0 = x.loc.i - width.max,
             y0 = avg.line.fun(dv.i),
             x1 = x.loc.i + width.max,
             y1 = avg.line.fun(dv.i),
             col = transparent(colors.df$avg.line.col[bean.i],
                               trans.val = 1 - opac.df$avg.line.o[bean.i]),
             lwd = avg.line.lwd[bean.i],
             lend = 3
    )
  }

  if(inf.band == "tight") {

    fun.loc <- which(abs(dens.x.i - avg.line.fun(dv.i)) == min(abs(dens.x.i - avg.line.fun(dv.i))))

    segments(x.loc.i - dens.y.i[fun.loc],
             avg.line.fun(dv.i),
             x.loc.i + dens.y.i[fun.loc],
             avg.line.fun(dv.i),
             col = transparent(colors.df$avg.line.col[bean.i],
                               trans.val = 1 - opac.df$avg.line.o[bean.i]),
             lwd = avg.line.lwd[bean.i],
             lend = 3
    )
  }

}

}

# Add bean names for IV 1

if(subplot.n.iv == 1) {line.t <- .5}
if(subplot.n.iv == 2) {line.t <- 2}

if(is.null(xaxt) == T) {

  if(is.null(cex.lab)) {

    cex.lab <- 1 / ((n.subplots - 1) * .1 + 1)

  }

  mtext(bean.mtx[,1],
        side = 1,
        at = bean.mtx$x.loc,
        line = line.t,
        cex = cex.lab)


  # Add names for IV 2

  if(subplot.n.iv == 2) {

    mtext(iv.names[1], side = 1, line = 2, at = par("usr")[1], adj = 1, cex = cex.lab)

    mtext(iv.names[2], side = 1, line = .5, at = par("usr")[1], adj = 1, cex = cex.lab)

    text.loc <- (iv.lengths[1] + 1) / 2 * (2 *(1:iv.lengths[2]) - 1)

    mtext(text = unique(bean.mtx[,2]),
          side = 1,
          line = .5,
          at = text.loc,
          cex = cex.lab
    )


  }

}

}

}

}
