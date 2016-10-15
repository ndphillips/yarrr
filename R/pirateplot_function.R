#' pirateplot
#'
#' The pirateplot function creates an RDI (Raw data, Descriptive and Inferential statistic) plot showing the relationship between 1 - 3 categorical independent variables and 1 continuous dependent variable. The plot shows all raw data (shown as points), smoothed densities (shown as beans), and inferential statistics such as 95% Bayesian Highest Density Intervals (shown as bands).
#'
#' @param formula formula. A formula in the form \code{y ~ x1 + x2 + x3} indicating the vertical response variable (y) and up to three independent varaibles
#' @param data dataframe. A dataframe containing the variables specified in formula.
#' @param pal string. The color palette of the plot. Can be a single color, a vector of colors, or the name of a palette in the piratepal() function (e.g.; "basel", "google", "southpark"). To see all the palettes, run \code{piratepal(palette = "all", action = "show")}
#' @param point.col,bar.col,bean.col,bean.fill.col,inf.col,inf.border.col,avg.line.col,bar.border.col,quant.col (string) An optional vector of colors specifying the colors of the plotting elements. This will override values in the palette.
#' @param theme.o integer. An integer in the set 0, 1 specifying an opacity theme (that is, specific values of bar.o, point.o, etc.). \code{theme = 0} turns off all opacities which can then be individually specified using bar.o, inf.o (etc.)
#' @param bar.o,point.o,inf.o,avg.line.o,bean.o,bean.fill.o,bar.border.o (numeric) A number between 0 and 1 indicating how opaque to make the bars, points, inference band, average line, and beans respectively. These values override whatever is in the specified theme
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
#' @param bar.border.lwd,line.fun depricated arguments
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
#' # pirateplot of chicken weights by diet
#' pirateplot(formula = weight ~ Diet,
#'            data = ChickWeight)
#'
#'
#' # Some customizations on ChickWeight
#'pirateplot(formula = weight ~ Time,
#'           data = ChickWeight,
#'           pal = gray(.1), # Dark gray palette
#'           quant = c(.1, .9),   # Add .10 and .90 quantiles
#'           bean.fill.col = "white", # Fill beans with white
#'           bean.fill.o = 1, # Full filling opacity
#'           back.col = "snow1", # Bit of color in the background
#'           gl.col = "black", # Black gridlines
#'           gl.lwd = c(.25, 0), # Just major gridlines
#'           gl.lty = 1) # solid gridlines
#'
#'
#'# More customizations now with 2 IVs on ToothGrowth
#'pirateplot(formula = len ~ dose + supp,
#'           data = ToothGrowth,
#'           pal = gray(.1), # Dark gray palette
#'           inf.col = piratepal("basel"), # add color to bands
#'           inf.o = .7, # Slightly transparent bands
#'           bar.o = 0, # Turn off main bars
#'           gl.col = gray(.6)) # mid-gray gridlines
#'
#'
#'   # See the vignette
#'  vignette("pirateplot", package = "yarrr")
#'
#'

pirateplot <- function(
  formula = NULL,
  data = NULL,
  avg.line.fun = mean,
  pal = "basel",
  back.col = gray(1),
  point.cex = 1,
  point.pch = 16,
  point.lwd = 1,
  jitter.val = .03,
  theme.o = 1,
  bean.o = NULL,
  bean.fill.o = NULL,
  point.o = NULL,
  bar.o = NULL,
  bar.border.o = NULL,
  inf.o = NULL,
  avg.line.o = NULL,
  gl.col = NULL,
  point.col = NULL,
  bar.col = NULL,
  bean.col = NULL,
  bean.fill.col = NULL,
  inf.col = NULL,
  inf.border.col = NULL,
  avg.line.col = NULL,
  bar.border.col = NULL,
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
  bar.border.lwd = NULL,
  line.fun = NULL,
  ...
) {



# -----
#  SETUP
# ------

# Check for depricated arguments

if(is.null(bar.border.lwd) == FALSE) {

  message("bar.border.lwd is depricated. Use bar.lwd intead")

  bar.lwd <- bar.border.lwd
}

if(is.null(line.fun) == FALSE) {

  message("line.fun is depricated. Use avg.line.fun intead")

  avg.line.fun <- line.fun

}


# Look for missing critical inputs

if(is.null(data)) {stop("You must specify a dataframe in the data argument!")}
if(is.null(formula) | class(formula) != "formula") {stop("You must specify a valid formula in the formula argument!")}

{

# Reshape dataframe to include relevant variables

data <- model.frame(formula = formula,
                      data = data)

dv.name <- names(data)[1]
dv.v <- data[,1]

# GET IV INFORMATION

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

}

# Loop over subplots (only relevant when there is a third IV)
if(n.subplots == 1) {par(mfrow = c(1, 1))}
if(n.subplots == 2) {par(mfrow = c(1, 2))}
if(n.subplots == 3) {par(mfrow = c(1, 3))}
if(n.subplots == 4) {par(mfrow = c(2, 2))}
if(n.subplots %in% c(5, 6)) {par(mfrow = c(2, 3))}
if(n.subplots > 7) {par(mfrow = c(ceiling(sqrt(n.subplots)), ceiling(sqrt(n.subplots))))}

for(subplot.i in 1:n.subplots) {

data.i <- data[selection.mtx[,subplot.i],]

# Remove potential iv3 column
data.i <- data.i[,1:min(ncol(data.i), 3)]

# Determine levels of each IV

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

# DEFINE OPACITIES
{

opac.df <- data.frame(
  point.o = rep(NA, n.beans),
  bean.o = rep(NA, n.beans),
  bean.fill.o = rep(NA, n.beans),
  inf.o = rep(NA, n.beans),
  avg.line.o = rep(NA, n.beans),
  bar.o = rep(NA, n.beans),
  bar.border.o = rep(NA, n.beans)
)
rownames(opac.df) <- 1:n.beans

if((theme.o %in% c(0, 1)) == FALSE) {

  print("theme.o must be either 0 or 1. I'll set it to 1 for now.")
  theme.o <- 1

}

if(theme.o == 1) {

 opac.df$point.o <- .2
 opac.df$bean.o <- .2
 opac.df$bean.fill.o <- 0
 opac.df$inf.o <- .8
 opac.df$avg.line.o <- 1
 opac.df$bar.o <- .1
 opac.df$bar.border.o <- 0

}

if(theme.o == 0) {

  opac.df$point.o <- 0
  opac.df$bean.o <- 0
  opac.df$bean.fill.o <- 0
  opac.df$inf.o <- 0
  opac.df$avg.line.o <- 0
  opac.df$bar.o <- 0
  opac.df$bar.border.o <- 0
}

# If opacity values are specified, update them.
if(is.null(point.o) == FALSE) {opac.df$point.o <- rep(point.o, length.out = n.beans)}
if(is.null(bean.o) == FALSE) {opac.df$point.o <- rep(bean.o, length.out = n.beans)}
if(is.null(bean.fill.o) == FALSE) {opac.df$bean.fill.o <- rep(bean.fill.o, length.out = n.beans)}
if(is.null(inf.o) == FALSE) {opac.df$inf.o <- rep(inf.o, length.out = n.beans)}
if(is.null(avg.line.o) == FALSE) {opac.df$avg.line.o <- rep(avg.line.o, length.out = n.beans)}
if(is.null(bar.o) == FALSE) {opac.df$bar.o <- rep(bar.o, length.out = n.beans)}
if(is.null(bar.border.o) == FALSE) {opac.df$bar.border.o <- rep(bar.border.o, length.out = n.beans)}

}

# DEFINE COLORS
{
colors.df <- data.frame(
  point.col = rep(NA, n.beans),
  bean.col = rep(NA, n.beans),
  bean.fill.col = rep(NA, n.beans),
  inf.col = rep(NA, n.beans),
  inf.border.col = rep(NA, n.beans),
  avg.line.col = rep(NA, n.beans),
  bar.col = rep(NA, n.beans),
  bar.border.col = rep(NA, n.beans),
  quant.col = rep(NA, n.beans)
)
rownames(colors.df) <- 1:n.beans


# If palette is in piratepal()...
if(mean(pal %in% piratepal("names")) == 1) {

colors.df$point.col <- rep(piratepal(palette = pal, length.out = n.cols), length.out = n.beans)
colors.df$bean.col <- rep(piratepal(palette = pal, length.out = n.cols), length.out = n.beans)
colors.df$bean.fill.col <- rep(piratepal(palette = pal, length.out = n.cols), length.out = n.beans)
colors.df$inf.col <- rep(piratepal(palette = pal, length.out = n.cols), length.out = n.beans)
colors.df$inf.border.col <- rep(piratepal(palette = pal, length.out = n.cols), length.out = n.beans)
colors.df$avg.line.col <- rep(piratepal(palette = pal, length.out = n.cols), length.out = n.beans)
colors.df$bar.col <- rep(piratepal(palette = pal, length.out = n.cols), length.out = n.beans)
colors.df$bar.border.col <- rep(piratepal(palette = pal, length.out = n.cols), length.out = n.beans)
colors.df$quant.col <- rep(piratepal(palette = pal, length.out = n.cols), length.out = n.beans)

}

# If palette is NOT in piratepal()...

if(mean(pal %in% piratepal("names")) != 1) {

colors.df$point.col <- rep(pal, length.out = n.beans)
colors.df$bean.col <- rep(pal, length.out = n.beans)
colors.df$bean.fill.col <- rep("white", length.out = n.beans)
colors.df$inf.col <- rep(pal, length.out = n.beans)
colors.df$inf.border.col <- rep(pal, length.out = n.beans)
colors.df$avg.line.col <- rep(pal, length.out = n.beans)
colors.df$bar.col <- rep(pal, length.out = n.beans)
colors.df$bar.border.col <- rep(pal, length.out = n.beans)
colors.df$quant.col <- rep(pal, length.out = n.beans)

}

# Apply specified colors

if(is.null(point.col) == FALSE) {colors.df$point.col <- rep(point.col, length.out = n.beans)}
if(is.null(bean.col) == FALSE) {colors.df$bean.col <- rep(bean.col, length.out = n.beans)}
if(is.null(bean.fill.col) == FALSE) {colors.df$bean.fill.col <- rep(bean.fill.col, length.out = n.beans)}
if(is.null(inf.col) == FALSE) {colors.df$inf.col <- rep(inf.col, length.out = n.beans)}
if(is.null(inf.border.col) == FALSE) {colors.df$inf.border.col <- rep(inf.border.col, length.out = n.beans)}
if(is.null(avg.line.col) == FALSE) {colors.df$line.col <- rep(avg.line.col, length.out = n.beans)}
if(is.null(bar.col) == FALSE) {colors.df$bar.col <- rep(bar.col, length.out = n.beans)}
if(is.null(bar.border.col) == FALSE) {colors.df$bar.border.col <- rep(bar.border.col, length.out = n.beans)}
if(is.null(quant.col) == FALSE) {colors.df$quant.col <- rep(quant.col, length.out = n.beans)}

}

# SETUP PLOTTING SPACE
{
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

#showtext::showtext.auto()

#par(mar = c(5, 4, 4, 1) + .1)

if(evidence == T) {layout(matrix(1:2, nrow = 2, ncol = 1), heights = c(5, 2), widths = 5)}


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

 # mtext(side = 1, text = my.xlab)
 # mtext(side = 1, text = my.xlab, family = family)


  # Add y-axis

  if(is.null(yaxt)) {

  axis(side = 2,
       at = y.levels,
       las = 1,
       lwd = 1,
       lwd.ticks = 1,
       cex.axis = cex.axis)

  }

  # Add background

    rect(-1e10, -1e10, 1e10, 1e10,
         col = back.col,
         border = NA)

  # Add gridlines

  if(is.null(gl.col) == F) {

    if(is.null(gl.lwd)) {gl.lwd <- c(1, .5)}
    if(is.null(gl.lty)) {gl.lty <- 3}

    abline(h = seq(min(y.levels), max(y.levels), length.out = length(y.levels) * 2 - 1),
           lwd = gl.lwd,
           col = gl.col,
           lty = gl.lty)
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

for (bean.i in 1:n.beans) {

  dv.i <- data.i[data.i$bean.num == bean.i, dv.name]

  if(is.logical(dv.i)) {dv.i <- as.numeric(dv.i)}

  fun.val <- avg.line.fun(dv.i)

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


# QUANTILES

if (!is.null(quant)) {

  # set default line length if length is not given manually

  if(is.null(quant.lwd) == FALSE) {quant.lwd <- rep(quant.lwd, length.out = length(quant))}
  if(is.null(quant.length) == FALSE) {quant.length <- rep(quant.length, length.out = length(quant))}


  if (is.null(quant.length)) {
    quant.length <- c(rep(0.65 * width.max, length(quant)))
  }
  if (is.null(quant.lwd)) {
    quant.lwd <- c(rep(1, length(quant)))
  }


  # init empty vector for loop
  stats.limit = c()

  for (i in 1:length(quant)) {

    # draw lines
    segments(x.loc.i + (quant.length[i] - width.max), # left end
             quantile(dv.i, probs = quant[i]),
             x.loc.i - (quant.length[i] - width.max), # right end
             quantile(dv.i, probs = quant[i]),
             col = avg.line.col[bean.i],
             lwd = quant.lwd[i],
             lend = 3
    )
  }
}

# BAR
{
rect(x.loc.i - width.max,
     0,
     x.loc.i + width.max,
     fun.val,
     col = transparent(colors.df$bar.col[bean.i], trans.val = 1 - opac.df$bar.o[bean.i]),
     border = transparent(colors.df$bar.border.col[bean.i], trans.val = 1 - opac.df$bar.border.o[bean.i]),
     lwd = bar.border.lwd[bean.i]
)
}


# BEAN
{

if(length(setdiff(dv.i, c(0, 1))) > 0 & length(dv.i) > 3) {

  polygon(c(x.loc.i - dens.y.plot.i[1:(length(dens.x.plot.i))],
            x.loc.i + rev(dens.y.plot.i[1:(length(dens.x.plot.i))])),
          c(dens.x.plot.i[1:(length(dens.x.plot.i))],
            rev(dens.x.plot.i[1:(length(dens.x.plot.i))])),
          col = transparent(colors.df$bean.fill.col[bean.i],
                            trans.val = 1 - opac.df$bean.fill.o[bean.i]),
          border = transparent(colors.df$bean.col[bean.i],
                               trans.val = 1 - opac.df$bean.o[bean.i]),
          lwd = bean.lwd[bean.i], lty = bean.lty[bean.i]
  )

}

}

# BAR
{
rect(x.loc.i - width.max,
     0,
     x.loc.i + width.max,
     fun.val,
     col = transparent(colors.df$bar.col[bean.i],
                       trans.val = 1 - opac.df$bar.o[bean.i]),
     border = transparent(colors.df$bar.border.col[bean.i],
                          trans.val = 1 - opac.df$bar.border.o[bean.i]),
     lwd = bar.lwd[bean.i]
)
}


# POINTS
{
points(x = rep(x.loc.i, length(dv.i)) + rnorm(length(dv.i), mean = 0, sd = jitter.val),
       y = dv.i,
       pch = point.pch,
       col = transparent(colors.df$point.col[bean.i],
                         trans.val = 1 - opac.df$point.o[bean.i]),
       cex = point.cex,
       lwd = point.lwd
)
}

# AVERAGE LINE
{

if(inf.band == "wide") {
segments(x.loc.i - width.max,
         fun.val,
         x.loc.i + width.max,
         fun.val,
         col = transparent(colors.df$avg.line.col[bean.i],
                           trans.val = 1 - opac.df$avg.line.o[bean.i]),
         lwd = avg.line.lwd[bean.i],
         lend = 3
)
}

if(inf.band == "tight") {

  fun.loc <- which(abs(dens.x.i - fun.val) == min(abs(dens.x.i - fun.val)))

  segments(x.loc.i - dens.y.i[fun.loc],
           fun.val,
           x.loc.i + dens.y.i[fun.loc],
           fun.val,
           col = transparent(colors.df$avg.line.col[bean.i],
                             trans.val = 1 - opac.df$avg.line.o[bean.i]),
           lwd = avg.line.lwd[bean.i],
           lend = 3
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
    quant.lwd <- c(rep(0.3, length(quant)))
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

# BAND
{
if(opac.df$bean.o[bean.i] > 0 & length(dv.i) > 3 & sd(dv.i) > 0) {

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
     col = transparent(colors.df$inf.col[bean.i],
                       trans.val = 1 - opac.df$inf.o[bean.i]),
     lwd = inf.lwd[bean.i],
     border = colors.df$inf.border.col[bean.i])

}

if(inf.band == "tight") {

  polygon(c(x.loc.i - dens.inf.y[1:(length(dens.inf.x))],
            x.loc.i + rev(dens.inf.y[1:(length(dens.inf.x))])),
          c(dens.inf.x[1:(length(dens.inf.x))],
            rev(dens.inf.x[1:(length(dens.inf.x))])),
          col = transparent(colors.df$inf.col[bean.i],
                            trans.val = 1 - opac.df$inf.o[bean.i]),
          border = transparent(colors.df$inf.border.col[bean.i], trans.val = 0),
          lwd = bean.lwd[bean.i]
  )
}
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

# ----
# EVIDENCE
# ----
#
# if(evidence) {
#
#   # Convert IVs to factors
#
# for(iv.i in 1:subplot.n.iv) {
#
# data[,iv.i] <- as.factor(data[,iv.i])
#
#   }
#
# bf <- BayesFactor::anovaBF(formula = formula, data = data[,1:(subplot.n.iv + 1)])
#
# bf.vec <- extractBF(bf)$bf
#
# par(mar = c(0, 0, 3, 0))
# plot(1, xlim = c(0, 1), ylim = c(0, 1), bty = "n", xlab = "", ylab = "", xaxt = "n", yaxt = "n", type = "n")
#
# add.bf.bar <- function(bf.val, x.loc, y.loc, add.labels = F) {
#
# rect(x.loc[1], y.loc[1], x.loc[2], y.loc[2], border = gray(.5, .5), col = "white")
#
# bf.scale <- c(0, 1 / 150, 1 / 20, 1 / 3, 1, 3, 20, 150, Inf)
# bf.labels <- c("Very Strong", "Strong", "Positive", "Weak", "Weak", "Positive", "Strong", "Very Strong")
#
# bf.i.label <- bf.labels[bf.val < bf.scale[2:length(bf.scale)] & bf.val > bf.scale[1:(length(bf.scale) - 1)]]
#
# if(bf.val > 1) {in.favor <- "alt"}
# if(bf.val < 1) {in.favor <- "null"}
#
# bf.col.fun <- circlize::colorRamp2(breaks = c(-1, 0, 1),
#                                  colors = c("red", "yellow", "blue"),
#                                  transparency = .7)
#
# bf.val.t <- bf.val
# if(bf.val > 150) {bf.val.t <- 150}
# if(bf.val < (1 / 150)) {bf.val.t <- 1 / 150}
#
# bf.val.t.log <- log(bf.val.t, base = 10)
#
# if(bf.val > 1) {
#
#  bf.val.scale <- bf.val.t.log / log(150, base = 10)
#  bf.yloc <- mean(y.loc) + bf.val.scale * (y.loc[2] - y.loc[1]) / 2
#
#  }
#
# if(bf.val < 1) {
#
#  bf.val.scale <- -1 * bf.val.t.log / log(1 / 150, base = 10)
#  bf.yloc <- mean(y.loc) + bf.val.scale * (y.loc[2] - y.loc[1]) / 2
#
#  }
#
#
#
#  rect(x.loc[1],
#       mean(y.loc),
#       x.loc[2],
#       bf.yloc,
#       col = bf.col.fun(bf.val.scale), border = NA)
#
# if(add.labels) {
#
#   # text(x = rep(x.loc[1], length(bf.scale)),
#   #      y = bf.l.scale / log(max(bf.scale), base = 10),
#   #      labels = bf.scale, adj = 1
#   #      )
#
#
#   text(mean(x.loc), min(y.loc) + .75 * diff(y.loc),
#        labels = paste("BF = ", round(bf.val.t, 1), sep = ""))
#
#
#   if(bf.val < 1) {concl.text <- paste(bf.i.label, " Evidence for\nNO effect", sep = "")}
#   if(bf.val > 1) {concl.text <- paste(bf.i.label, " Evidence for a\nTRUE effect", sep = "")}
#
#   text(mean(x.loc), min(y.loc) + .5 * diff(y.loc),
#        labels = concl.text)
#
# }
#
#
#
#
#  }
#
#
# if(subplot.n.iv == 1) {bar.loc.vec <- c(.5) ; bar.widths <- .1}
# if(subplot.n.iv == 2) {bar.loc.vec <- c(.3, .7) ; bar.widths <- .1}
#
# for(iv.i in 1:subplot.n.iv) {
#
# add.bf.bar(bf.vec[iv.i],
#            x.loc = c(bar.loc.vec[iv.i] - bar.widths / 2, bar.loc.vec[iv.i] + bar.widths / 2),
#            y.loc = c(.1, .9),
#            add.labels = T
# )
#
#   text(x = bar.loc.vec[iv.i], y = 1, labels = iv.names[iv.i])
#
#
# }
#
#
# }


  # reset parameters

  # par(mfrow = c(1, 1))
  # par(mar = c(5, 4, 4, 1) + .1)



