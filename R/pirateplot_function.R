#' pirateplot
#'
#' The pirateplot function creates an RDI plot (Raw data, Descriptive and Inferential statistic) pirate version of the fantastic beanplot function in the beanplot package. Just like a beanplot, pirateplot takes a discrete iv and a continuous dv, and creates a plot showing raw data, smoothed densities and central tendency. In addition, pirateplot adds the option for a 95% Highest Density Intervals (HDI), and has a few aesthetic differences preferred by pirates.
#'
#' @param formula (formula) A formula in the form y ~ x1 + x2 indicating the vertical response variable (y) and 1 or two independent varaibles
#' @param data (dataframe) Data which to perform the beanplot on. This data can consist of dataframes, vectors and/or formulas. For each formula, a dataset can be specified with data=[dataset], and a subset can be specified with subset=[subset]. If subset/data arguments are passed, but there are not enough subset/data arguments, they are reused. Additionally, na.action, drop.unused.levels and xlev can be passed to model.frame in the same way. Also, parameters for axis and title can be passed.
#' @param line.fun (function) A function that determines how average lines and bar heights are determined (default is mean).
#' @param pal (string) A string indicating the color palette of the plot. Can be a single color, a vector of colors, or the name of a palette in the piratepal() function (e.g.; "basel", "google", "southpark"). To see all the palettes, run piratepal(palette = "all", action = "show")
#' @param gl.col,back.col (string) An optional string indicating the color of the horizontal gridlines and plotting background.
#' @param point.cex,point.pch,point.lwd (numeric) Numbers indicating the size, pch type, and line width of raw data points.
#' @param width.min,width.max (numeric) The minimum and maximum width of a bean.
#' @param cut.min, cut.max (numeric) Optimal minimum and maximum values of the beans.
#' @param inf (string) A string indicating what types of inference lines to calculate. "ci" means frequentist confidence intervals, "hdi" means Bayesian Highest Density Intervals (HDI).
#' @param inf.p (numeric) A number between 0 and 1 indicating the level of confidence to use in calculating inferences for either confidence intervals or HDIs. The default is 0.95
#' @param theme.o (integer) An integer in the set 0, 1, 2, 3, specifying an opacity theme (that is, specific values of bar.o, point.o, etc.). You can override specific opacity values in a theme by specifying bar.o, inf.o (etc.)
#' @param bar.o,point.o,inf.o,line.o,bean.o (numeric) A number between 0 and 1 indicating how opaque to make the bars, points, inference line, average line, and beans respectively. These values override whatever is in the specified theme
#' @param point.col,bar.col,bean.border.col,bar.border.col,inf.col,average.line.col,bar.border.col (string) An optional vector of colors specifying the colors of the plotting elements. This will override values in the palette.
#' @param bean.lwd,inf.lwd,line.lwd,bar.border.lwd (numeric) A vector of numbers indicating the line widths of various elements.
#' @param hdi.iter (integer) An integer indicating how many iterations to run when calculating the HDI. Larger values lead to better estimates, but can be more time consuming.
#' @param bw (string) The smoothing bandwidth to use for the bean. (see ?density)
#' @param adjust (numeric) Adjustment for the bandwidth (see ?density)
#' @param jitter.val (numeric) A number indicaing how much to jitter the points horizontally. Defaults to 0.05.
#' @param at (numeric) An optional vector specifying the locations of the beans. Especially helpful when adding beans to an existing plot with add = T
#' @param sortx (logical) A logical value indicating whether or not to sort the unique values of the independent variables in the plot.
#' @param add (logical) A logical value indicating whether to add the pirateplot to an existing plotting space or not.
#' @param ... other arguments passed on to the plot function (e.g.; main, xlab, ylab, ylim, cex.axis, cex.main, cex.lab)
#' @keywords plot
#' @export
#' @examples
#'
#'
#'
#'# Pirateplots of the ChickWeight dataframe
#'
#'# Matrix of Pirate Plots of the ChickWeight dataframe
#'par(mfrow = c(4, 2))
#'
#'# Plot 1: Theme 1
#'
#'pirateplot(formula = weight ~ Diet,
#'           data = ChickWeight,
#'           main = "Theme 1",
#'           theme.o = 1
#'           )
#'
#'# Plot 2: Theme 1 + grayscale
#'
#'pirateplot(formula = weight ~ Diet,
#'           data = ChickWeight,
#'           main = "Theme 1 + grayscale",
#'           theme.o = 1,
#'           pal = "black"
#'           )
#'
#'
#'# Plot 3: Theme 2 + black points
#'
#'pirateplot(formula = weight ~ Diet,
#'           data = ChickWeight,
#'           main = "Theme 2",
#'           theme.o = 2,
#'           point.col = "black"
#'           )
#'
#'# Plot 4: Theme 2 + grayscale + gridlines
#'
#'pirateplot(formula = weight ~ Diet,
#'           data = ChickWeight,
#'           main = "Theme 2 + grayscale",
#'           pal = "black",
#'           theme.o = 2,
#'           point.o = .2,
#'           gl.col = gray(.9),
#'           point.pch = 16
#'           )
#'
#'
#'# Plot 5: Theme 3
#'
#'pirateplot(formula = weight ~ Diet,
#'           data = ChickWeight,
#'           main = "Theme 3"
#'           theme.o = 3
#')
#'
#'# Plot 6: Theme 3 + white on black
#'
#'pirateplot(formula = weight ~ Diet,
#'           data = ChickWeight,
#'           main = "Theme 3 + white on black",
#'           pal = "white",
#'           theme.o = 3,
#'           gl.col = gray(.7),
#'           back.col = gray(.2)
#'           )
#'
#'# Plot 7: Theme 0 - Fully customised
#'
#'pirateplot(formula = weight ~ Diet,
#'           data = ChickWeight,
#'           main = "Theme 0\nFully customized",
#'           pal = "google",
#'           point.o = .2,
#'           line.o = 1,
#'           theme.o = 0,
#'           line.lwd = 10,
#'           point.cex = 1.5,
#'           jitter.val = .1
#'           )
#'
#'
#'# Plot 8: Theme 0\nFully customised
#'
#'pirateplot(formula = weight ~ Diet,
#'           data = ChickWeight,
#'           main = "Theme 0\nFully customized",
#'           pal = "info2",
#'           point.o = .03,
#'           line.o = 0,
#'           bean.o = 1,
#'           theme.o = 0,
#'           back.col = transparent("steelblue4", .5),
#'           line.lwd = 10,
#'           yaxt = "n",   # no y-axis
#'           ylab = "",
#'           bty = "n",    # no plot border
#'           point.cex = 3,
#'           jitter.val = .00
#'           )
#'
#'par(mfrow = c(2, 2))
#'
#'# Matrix of plots with 2 IVs
#'
#'# Theme 1
#'
#'pirateplot(formula = weight ~ Diet + Time,
#'           data = subset(ChickWeight, Time < 10),
#'           theme.o = 1,
#'           gl.col = gray(.8),
#'           main = "Two IVs\nTheme 1, default palette"
#'           )
#'
#'# Theme 2
#'
#'pirateplot(formula = weight ~ Diet + Time,
#'           data = subset(ChickWeight, Time < 10),
#'           theme.o = 2,
#'           pal = "basel",
#'           main = "Two IVs\nTheme 2, Basel palette"
#'           )
#'
#'# Theme 3
#'
#'pirateplot(formula = weight ~ Diet + Time,
#'           data = subset(ChickWeight, Time < 10),
#'           theme.o = 3,
#'           pal = "ipod",
#'           main = "Two IVs\nTheme 3 (with slow to calculate HDIs), ipod palette"
#'           )
#'
#'# Theme 0 (fully customised)
#'
#'pirateplot(formula = weight ~ Diet + Time,
#'           data = subset(ChickWeight, Time < 10),
#'           theme.o = 0,
#'           bar.o = 0,
#'           point.o = .7,
#'           bean.o = .2,
#'           inf.o = 0,
#'           pal = "ipod",
#'           bar.border.col = gray(.5),
#'           main = "Two IVs\nTheme 0, ipod palette"
#'           )
#'
#'
#





pirateplot <- function(
  formula,
  data,
  line.fun = mean,
  pal = "appletv",
  back.col = gray(1),
  point.cex = 1,
  point.pch = 16,
  point.lwd = 1,
  cut.min = NULL,
  cut.max = NULL,
  width.min = .3,
  width.max = NA,
  bean.o = NULL,
  point.o = NULL,
  bar.o = NULL,
  inf.o = NULL,
  line.o = NULL,
  inf = "ci",
  inf.p = .95,
  theme.o = 1,
  hdi.iter = 1e3,
  jitter.val = .03,
  line.lwd = 4,
  bean.lwd = 1,
  inf.lwd = 1,
  bar.border.lwd = 1,
  gl.col = NULL,
  ylim = NULL,
  xlim = NULL,
  xlab = NULL,
  ylab = NULL,
  main = NULL,
  yaxt = NULL,
  point.col = NULL,
  bar.col = NULL,
  bean.border.col = NULL,
  inf.col = NULL,
  average.line.col = NULL,
  bar.border.col = NULL,
  at = NULL,
  bw = "nrd0",
  adjust = 1,
  add = F,
  sortx = T,
  y.levels = NULL,
  cex.lab = 1,
  cex.axis = 1,
  bty = "n",
  ...
) {

## TESTING
#
#
#   line.fun = mean
#   pal = "appletv"
#   back.col = gray(1)
#   point.cex = 1
#   point.pch = 16
#   point.lwd = 1
#   cut.min = NULL
#   cut.max = NULL
#   width.min = .3
#   width.max = NA
#   bean.o = NULL
#   point.o = NULL
#   bar.o = NULL
#   inf.o = NULL
#   line.o = NULL
#   inf = "hdi"
#   inf.p = .95
#   theme.o = 3
#   hdi.iter = 1e3
#   jitter.val = .03
#   line.lwd = 4
#   bean.lwd = 1
#   inf.lwd = 1
#   bar.border.lwd = 1
#   gl.col = NULL
#   ylim = NULL
#   xlim = NULL
#   xlab = NULL
#   ylab = NULL
#   main = NULL
#   yaxt = NULL
#   point.col = NULL
#   bar.col = NULL
#   bean.border.col = NULL
#   inf.col = NULL
#   average.line.col = NULL
#   bar.border.col = NULL
#   at = NULL
#   bw = "nrd0"
#   adjust = 1
#   add = F
#   sortx = T
#   y.levels = NULL
#   cex.lab = 1
#   cex.axis = 1
#
#
#   formula = weight ~ Diet
#   data = ChickWeight
#   main = "Theme 1"
#   theme.o = 3



  data.2 <- model.frame(formula = formula,
                        data = data)

  dv.name <- names(data.2)[1]
  dv.v <- data.2[,1]

  # Determine levels of each IV

  if(sortx == T) {

  iv.levels <- lapply(2:ncol(data.2), FUN = function(x) {unique(data.2[,x])})

  }

  if(sortx == F) {

    iv.levels <- lapply(2:ncol(data.2), FUN = function(x) {sort(unique(data.2[,x]))})

  }

  iv.lengths <- sapply(1:length(iv.levels), FUN = function(x) {length(iv.levels[[x]])})
  iv.names <- names(data.2)[2:ncol(data.2)]
  n.iv <- length(iv.levels)

  if(is.na(width.max)) {

  if(n.iv == 1) {width.max <- .45}
  if(n.iv == 2) {width.max <- .5}

  }

  # Set up bean info

  bean.mtx <- expand.grid(iv.levels)
  names(bean.mtx) <- names(data.2)[2:ncol(data.2)]
  n.beans <- nrow(bean.mtx)
  bean.mtx$bean.num <- 1:nrow(bean.mtx)

  # Determine bean x locations

  if(is.null(at)) {

  bean.loc <- 1:n.beans

  group.spacing <- 1

  if(n.iv == 2) {

    bean.loc <- bean.loc + rep(group.spacing * (0:(iv.lengths[2] - 1)), each = iv.lengths[1])

  }

  }

  if(!is.null(at)) {

   bean.loc <- rep(at, length.out = n.beans)

    }

  bean.mtx$x.loc <- bean.loc

  data.2 <- merge(data.2, bean.mtx)


  n.cols <- iv.lengths[1]

  # Determine opacity values

  if(theme.o == 1) {

    point.o <- ifelse(is.null(point.o), .3, point.o)
    bean.o <- ifelse(is.null(bean.o), .1, bean.o)
    inf.o <- ifelse(is.null(inf.o), 0, inf.o)
    line.o <- ifelse(is.null(line.o), .5, line.o)
    bar.o <- ifelse(is.null(bar.o), .5, bar.o)

  }

  if(theme.o == 2) {

    point.o <- ifelse(is.null(point.o), .8, point.o)
    bean.o <- ifelse(is.null(bean.o), .5, bean.o)
    inf.o <- ifelse(is.null(inf.o), 0, inf.o)
    line.o <- ifelse(is.null(line.o), .1, line.o)
    bar.o <- ifelse(is.null(bar.o), .1, bar.o)

  }

  if(theme.o == 3) {

    point.o <- ifelse(is.null(point.o), .2, point.o)
    bean.o <- ifelse(is.null(bean.o), .2, bean.o)
    inf.o <- ifelse(is.null(inf.o), .8, inf.o)
    line.o <- ifelse(is.null(line.o), 1, line.o)
    bar.o <- ifelse(is.null(bar.o), .1, bar.o)

  }

  if(theme.o == 0) {

    point.o <- ifelse(is.null(point.o), 0, point.o)
    bean.o <- ifelse(is.null(bean.o), 0, bean.o)
    inf.o <- ifelse(is.null(inf.o), 0, inf.o)
    line.o <- ifelse(is.null(line.o), 0, line.o)
    bar.o <- ifelse(is.null(bar.o), 0, bar.o)

  }

  # point.col = NULL,
  # bar.col = NULL,
  # bean.border.col = NULL,
  # inf.col = NULL,
  # average.line.col = NULL,
  # bar.border.col = NULL,



  # Get colors

  if(mean(pal %in% piratepal(action = "p")) == 1) {

    if (is.null(point.col)) {

      point.col <- piratepal(palette = pal,
                             length.out = n.cols,
                             trans = 1 - point.o)

      } else {

        point.col <- rep(transparent(point.col,
                                     trans.val = 1 - point.o),
                                     length.out = n.cols)
        }


    if (is.null(bean.border.col)) {

      bean.border.col <- piratepal(palette = pal,
                             length.out = n.cols,
                             trans = 1 - bean.o)

    } else {

      bean.border.col <- rep(transparent(bean.border.col,
                                   trans.val = 1 - bean.o),
                       length.out = n.cols)
    }


    if (is.null(inf.col)) {

      inf.col <- piratepal(palette = pal,
                                   length.out = n.cols,
                                   trans = 1 - inf.o)

    } else {

      inf.col <- rep(transparent(inf.col,
                                         trans.val = 1 - inf.o),
                             length.out = n.cols)
    }


    if (is.null(average.line.col)) {

      average.line.col <- piratepal(palette = pal,
                                length.out = n.cols,
                                trans = 1 - line.o)

    } else {

      average.line.col <- rep(transparent(average.line.col,
                                      trans.val = 1 - line.o),
                          length.out = n.cols)
    }

    if (is.null(bar.col)) {

      bar.col <- piratepal(palette = pal,
                                    length.out = n.cols,
                                    trans = 1 - bar.o)

    } else {

      bar.col <- rep(transparent(bar.col,
                                          trans.val = 1 - bar.o),
                              length.out = n.cols)
    }


  }

  if(mean(pal %in% piratepal(action = "p")) != 1) {

    if(length(pal) < n.cols) {pal <- rep(pal, n.cols)}

    if(is.null(point.col)) {

    point.col <- transparent(pal, trans.val = 1 - point.o)

    } else {

      point.col <- rep(transparent(point.col, trans.val = 1 - point.o),
                       length.out = n.cols)

    }


    if(is.null(bean.border.col)) {

      bean.border.col <- transparent(pal, trans.val = 1 - bean.o)

    } else {

      bean.border.col <- rep(transparent(bean.border.col, trans.val = 1 - bean.o),
                       length.out = n.cols)

    }


    if(is.null(inf.col)) {

      inf.col <- transparent(pal, trans.val = 1 - bean.o)

    } else {

      inf.col <- rep(transparent(inf.col, trans.val = 1 - inf.o),
                             length.out = n.cols)

    }


    if(is.null(average.line.col)) {

      average.line.col <- transparent(pal, trans.val = 1 - bean.o)

    } else {

      average.line.col <- rep(transparent(average.line.col, trans.val = 1 - line.o),
                          length.out = n.cols)

    }


    if(is.null(bar.col)) {

      bar.col <- transparent(pal, trans.val = 1 - bar.o)

    } else {

      bar.col <- rep(transparent(bar.col, trans.val = 1 - bar.o),
                              length.out = n.cols)

    }


  }

  if(n.iv == 2) {

    point.col <- rep(point.col, times = iv.lengths[2])
    bean.border.col <- rep(bean.border.col, times = iv.lengths[2])
    inf.col <- rep(inf.col, times = iv.lengths[2])
    average.line.col <- rep(average.line.col, times = iv.lengths[2])
    bar.col <- rep(bar.col, times = iv.lengths[2])

  }


  if(is.null(bar.border.col)) {bar.border.col <- bar.col}
  if(is.null(bar.border.col) == F) {bar.border.col <- rep(bar.border.col, times = length(bar.col))}



  # Create plotting space

# Determine y limits (y axis limits)
  # y axis breaks (y.levels)

  if(is.null(ylim) == TRUE & is.null(y.levels) == TRUE) {

    # Determine best step size

    steps.p <- c(
                seq(1e-3, 1e-2, 1e-3),
                seq(1e-4, 1e-3, 1e-3),
                seq(1e-5, 1e-4, 1e-4),
                seq(1e-6, 1e-5, 1e-5),
                seq(1e-7, 1e-6, 1e-6),
                seq(1e-8, 1e-7, 1e-7),
                seq(1e-9, 1e-8, 1e-8),
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
    plot.height <- ceiling((max(dv.v) - plot.min) /  best.step.size) * best.step.size
    plot.max <- plot.min + plot.height

    ylim <- c(plot.min, plot.max)
    y.levels <- seq(plot.min, plot.max, by = best.step.size)

  }

  if(is.null(ylim) == FALSE & is.null(y.levels) == TRUE) {


    steps.p <- c(
      seq(1e-3, 1e-2, 1e-3),
      seq(1e-4, 1e-3, 1e-3),
      seq(1e-5, 1e-4, 1e-4),
      seq(1e-6, 1e-5, 1e-5),
      seq(1e-7, 1e-6, 1e-6),
      seq(1e-8, 1e-7, 1e-7),
      seq(1e-9, 1e-8, 1e-8),
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

  if(is.null(ylim) == TRUE & is.null(y.levels) == FALSE) {

    ylim <- c(min(y.levels), max(y.levels))

    }


  if(is.null(xlim)) {xlim <- c(min(bean.loc) - .5, max(bean.loc) + .5)}



  # Determine x and y labels

  if(is.null(xlab)) {xlab <- iv.names[1]}
  if(is.null(ylab)) {ylab <- dv.name}


  if(add == F) {

    par(mar = c(5, 4, 4, 1) + .1)

  plot(1,
       xlim = xlim,
       ylim = ylim,
       type = "n",
       xaxt = "n",
       yaxt = "n",
       xlab = xlab,
       ylab = ylab,
       main = main,
       yaxt = yaxt,
       bty = bty,
       ...
  )


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

    abline(h = seq(min(y.levels),
                   max(y.levels),
                   length.out = 21),
           lwd = c(.75, .25),
           col = gl.col)
  }

  }



#    y.range <- max(dv.v) + total.dv.sd * .5 - min(dv.v) - total.dv.sd * .5
#
#     abline(h = seq(floor(min(dv.v) - total.dv.sd * .5),
#                    ceiling(max(dv.v) + total.dv.sd * .5),
#                    length.out = 10),
#            col = "white")



  # Add beans

  bean.lwd <- rep(bean.lwd, length.out = n.beans)
  inf.lwd <- rep(inf.lwd, length.out = n.beans)
  line.lwd <- rep(line.lwd, length.out = n.beans)
  bar.border.lwd <- rep(bar.border.lwd, length.out = n.beans)

  for (bean.i in 1:n.beans) {

    dv.i <- data.2[data.2$bean.num == bean.i, dv.name]
    fun.val <- line.fun(dv.i)

    x.loc.i <- bean.mtx$x.loc[bean.i]

    # Add bar

    rect(x.loc.i - width.max,
         0,
         x.loc.i + width.max,
         fun.val,
         col = bar.col[bean.i],
         border = bar.border.col[bean.i],
         lwd = bar.border.lwd[bean.i]
    )



    # Add bean
    {
      # Calculate bean densities

    if(length(dv.i) > 5) {  # only if n > 5

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


      polygon(c(x.loc.i - dens.y.plot.i[1:(length(dens.x.plot.i))],
                x.loc.i + rev(dens.y.plot.i[1:(length(dens.x.plot.i))])),
              c(dens.x.plot.i[1:(length(dens.x.plot.i))],
                rev(dens.x.plot.i[1:(length(dens.x.plot.i))])),
              col = gray(1, alpha = bean.o),
              border = bean.border.col[bean.i],
              lwd = bean.lwd[bean.i]
      )

    }

    }




    # Add raw data

    points(rep(x.loc.i, length(dv.i)) + rnorm(length(dv.i), mean = 0, sd = jitter.val),
           dv.i,
           pch = point.pch,
           col = point.col[bean.i],
           cex = point.cex,
           lwd = point.lwd
    )





    # Add Line
    segments(x.loc.i - width.max,
             fun.val,
             x.loc.i + width.max,
             fun.val,
             col = average.line.col[bean.i],
             lwd = line.lwd[bean.i],
             lend = 3
    )



    # Add inference band

    if(inf.o > 0) {

      if(inf == "hdi") {

        # Calculate one-sample bayesian T-test with BayesFactor package

      ttest.bf <- BayesFactor::ttestBF(dv.i, posterior = T, iterations = hdi.iter, progress = F)
      samples <- ttest.bf[,1]

      # using the hdi function from Kruschke

      inf.lb <- hdi(samples)[1]
      inf.ub <- hdi(samples)[2]

      dens.inf.x <- dens.x.i[dens.x.i >= inf.lb & dens.x.i <= inf.ub]
      dens.inf.y <- dens.y.i[dens.x.i >= inf.lb & dens.x.i <= inf.ub]

      }


      if(inf == "ci") {

        ci.i <- t.test(dv.i, conf.level = inf.p)$conf.int

        inf.lb <- ci.i[1]
        inf.ub <- ci.i[2]

        dens.inf.x <- dens.x.i[dens.x.i >= inf.lb & dens.x.i <= inf.ub]
        dens.inf.y <- dens.y.i[dens.x.i >= inf.lb & dens.x.i <= inf.ub]

      }



      #
      # if(band.type == "constrained") {
      #
      #   polygon(c(x.loc.i - dens.inf.y, x.loc.i + rev(dens.inf.y)),
      #           c(dens.inf.x, rev(dens.inf.x)),
      #           col = inf.col[bean.i],
      #           border = NA,
      #           lwd = inf.lwd[bean.i]
      #   )
      #
      # }



      # Draw HDI band

        rect(x.loc.i - width.max * .8,
             inf.lb,
             x.loc.i + width.max * .8,
             inf.ub,
             col = inf.col[bean.i],
             lwd = inf.lwd[bean.i],
             border = NA)



    }


    # Add function lines

    # fun.dens <- dens.y.i[abs(dens.x.i - fun.val) == min(abs(dens.x.i - fun.val))]
    #
    # if(band.type == "wide") {
    #
    #   segments(x.loc.i - width.max / 2,
    #            fun.val,
    #            x.loc.i + width.max / 2,
    #            fun.val,
    #            col = average.line.col[bean.i],
    #            lty = 1,
    #            lwd = 4
    #   )
    #
    # }
    #
    # if(band.type == "constrained") {
    #
    #
    #   segments(x.loc.i - fun.dens,
    #            fun.val,
    #            x.loc.i + fun.dens,
    #            fun.val,
    #            col = average.line.col[bean.i],
    #            lty = 1,
    #            lwd = 4
    #   )
    #
    # }








  }

  # Add bean names for IV 1

  mtext(bean.mtx[,1],
        side = 1,
        at = bean.mtx$x.loc,
        line = .5,
        cex = cex.lab)

  # Add names for IV 2

  if(n.iv == 2) {


      text.loc <- (iv.lengths[1] + 1) / 2 * (2 *(1:iv.lengths[2]) - 1)


    mtext(text = paste(names(bean.mtx)[2], "=", unique(bean.mtx[,2])),
          side = 1,
          line = 2,
          at = text.loc,
          cex = cex.lab
    )

  }




}

