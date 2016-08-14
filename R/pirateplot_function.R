#' pirateplot
#'
#' The pirateplot function creates an RDI plot (Raw data, Descriptive and Inferential statistic) pirate version of the fantastic beanplot function in the beanplot package. Just like a beanplot, pirateplot takes a discrete iv and a continuous dv, and creates a plot showing raw data, smoothed densities and central tendency. In addition, pirateplot adds the option for a 95% Highest Density Intervals (HDI), and has a few aesthetic differences preferred by pirates.
#'
#' @param formula (formula) A formula in the form y ~ x1 + x2 indicating the vertical response variable (y) and 1 or two independent varaibles
#' @param data (dataframe) A dataframe containing the variables specified in formula.
#' @param line.fun (function) A function that determines how average lines and bar heights are determined (default is mean).
#' @param pal (string) A string indicating the color palette of the plot. Can be a single color, a vector of colors, or the name of a palette in the piratepal() function (e.g.; "basel", "google", "southpark"). To see all the palettes, run piratepal(palette = "all", action = "show")
#' @param gl.col,back.col (string) An optional string indicating the color of the horizontal gridlines and plotting background.
#' @param point.cex,point.pch,point.lwd (numeric) Numbers indicating the size, pch type, and line width of raw data points.
#' @param width.min,width.max (numeric) The minimum and maximum width of a bean.
#' @param cut.min,cut.max (numeric) Optimal minimum and maximum values of the beans.
#' @param inf (string) A string indicating what types of inference lines to calculate. "ci" means frequentist confidence intervals, "hdi" means Bayesian Highest Density Intervals (HDI).
#' @param inf.p (numeric) A number between 0 and 1 indicating the level of confidence to use in calculating inferences for either confidence intervals or HDIs. The default is 0.95
#' @param theme.o (integer) An integer in the set 0, 1, 2, 3, specifying an opacity theme (that is, specific values of bar.o, point.o, etc.). You can override specific opacity values in a theme by specifying bar.o, inf.o (etc.)
#' @param bar.o,point.o,inf.o,line.o,bean.o (numeric) A number between 0 and 1 indicating how opaque to make the bars, points, inference line, average line, and beans respectively. These values override whatever is in the specified theme
#' @param point.col,bar.col,bean.border.col,inf.col,average.line.col,bar.border.col (string) An optional vector of colors specifying the colors of the plotting elements. This will override values in the palette.
#' @param bean.lwd,inf.lwd,line.lwd,bar.border.lwd (numeric) A vector of numbers indicating the line widths of various elements.
#' @param hdi.iter (integer) An integer indicating how many iterations to run when calculating the HDI. Larger values lead to better estimates, but can be more time consuming.
#' @param bw (string) The smoothing bandwidth to use for the bean. (see ?density)
#' @param adjust (numeric) Adjustment for the bandwidth (see ?density)
#' @param jitter.val (numeric) A number indicaing how much to jitter the points horizontally. Defaults to 0.05.
#' @param at (numeric) An optional vector specifying the locations of the beans. Especially helpful when adding beans to an existing plot with add = T
#' @param sortx (string) An optional argument indicating how to sort the x values. Can be "sequential" (as they are found in the original dataframe), "alphabetical", or a string indicating a function (i.e.; "mean")
#' @param add (logical) A logical value indicating whether to add the pirateplot to an existing plotting space or not.
#' @param evidence (logical) A logical value indicating whether to show Bayesian evidence (I'm still working on this...)
#' @param family a font family (may not be working yet...)
#' @param cex.lab,cex.axis Size of the labels and axes.
#' @param bty,xlim,ylim,xlab,ylab,main,yaxt,xaxt General plotting arguments
#' @param ... other arguments passed on to the plot function (e.g.; main, xlab, ylab, ylim, cex.axis, cex.main, cex.lab)
#' @keywords plot
#' @import datasets
#' @importFrom BayesFactor ttestBF
#' @importFrom grDevices col2rgb gray rgb
#' @importFrom graphics abline axis layout mtext par plot points polygon rasterImage rect segments text
#' @importFrom stats density model.frame optimize rnorm t.test
#' @importFrom utils vignette
#' @export
#' @examples
#'
#'
#'
#'   # See the vignette
#'  vignette("pirateplot", package = "yarrr")
#'
#'

pirateplot <- function(
  formula = weight ~ Time,
  data = ChickWeight,
  line.fun = mean,
  pal = "basel",
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
  inf = "hdi",
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
  xaxt = NULL,
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
  sortx = "alphabetical",
  cex.lab = 1,
  cex.axis = 1,
  bty = "n",
  evidence = F,
  family = NULL,
  ...
) {


## TESTING

{
  # formula = weight ~ Time
  # data = ChickWeight
  # line.fun = mean
  # pal = "appletv"
  # back.col = gray(1)
  # point.cex = 1
  # point.pch = 16
  # point.lwd = 1
  # cut.min = NULL
  # cut.max = NULL
  # width.min = .3
  # width.max = NA
  # bean.o = NULL
  # point.o = NULL
  # bar.o = NULL
  # inf.o = NULL
  # line.o = NULL
  # inf = "hdi"
  # inf.p = .95
  # theme.o = 1
  # hdi.iter = 1e3
  # jitter.val = .03
  # line.lwd = 4
  # bean.lwd = 1
  # inf.lwd = 1
  # bar.border.lwd = 1
  # gl.col = NULL
  # ylim = NULL
  # xlim = NULL
  # xlab = NULL
  # ylab = NULL
  # main = NULL
  # yaxt = NULL
  # xaxt = NULL
  # point.col = NULL
  # bar.col = NULL
  # bean.border.col = NULL
  # inf.col = NULL
  # average.line.col = NULL
  # bar.border.col = NULL
  # at = NULL
  # bw = "nrd0"
  # adjust = 1
  # add = F
  # sortx = "alphabetical"
  # cex.lab = 1
  # cex.axis = 1
  # bty = "n"
  # evidence = F
  # family = NULL
  #
  #
  #
  # formula = Accuracy ~ Set + ORDER
  # data = dataset[dataset$ORDER == "Resp_SCREE3",]
  # main = "PPV by condition"
  # line.fun = mean
  # #theme.o = 2
  # jitter.val = .2
  # inf = "ci" #Show confidence interval (95%)
  # inf.o = 0.2 #Opacity of ci
  # pal = "google"

}


# -----
#  SETUP
# ------
  {
  # Reshape dataframe to include relevant variables

  data.2 <- model.frame(formula = formula,
                        data = data)

  dv.name <- names(data.2)[1]
  dv.v <- data.2[,1]

  # Determine levels of each IV

  if(substr(sortx, 1, 1) == "a") {

    iv.levels <- lapply(2:ncol(data.2), FUN = function(x) {sort(unique(data.2[,x]))})

  }
  if(substr(sortx, 1, 1) == "s") {

    iv.levels <- lapply(2:ncol(data.2), FUN = function(x) {unique(data.2[,x])})

  }

  # TO DO
  # Add a new sorting function based on function (e.g.; mean) values.

  # if(is.function(get(sortx))) {
  #
  #   tapply()
  #
  #   iv.levels <- lapply(2:ncol(data.2), FUN = function(x) {unique(data.2[,x])})
  #
  #
  #
  # }

  # Info about IV(s)

  n.iv <- length(iv.levels)
  iv.lengths <- sapply(1:n.iv, FUN = function(x) {length(iv.levels[[x]])})
  iv.names <- names(data.2)[2:ncol(data.2)]


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

  # Determine number of colors (equal to the number of unique values of IV 1)

  n.cols <- iv.lengths[1]

  # Determine opacity values

  if(theme.o == 2) {

    if(is.null(point.o)) {point.o <- .3}
    if(is.null(bean.o)) {bean.o <- .1}
    if(is.null(inf.o)) {inf.o <- 0}
    if(is.null(line.o)) {line.o <- .5}
    if(is.null(bar.o)) {bar.o <- .5}

  }

  if(theme.o == 3) {


    if(is.null(point.o)) {point.o <- .8}
    if(is.null(bean.o)) {bean.o <- .5}
    if(is.null(inf.o)) {inf.o <- 0}
    if(is.null(line.o)) {line.o <- .1}
    if(is.null(bar.o)) {bar.o <- .1}


  }

  if(theme.o == 1) {

    if(is.null(point.o)) {point.o <- .2}
    if(is.null(bean.o)) {bean.o <- .2}
    if(is.null(inf.o)) {inf.o <- .8}
    if(is.null(line.o)) {line.o <- 1}
    if(is.null(bar.o)) {bar.o <- .1}

  }

  if(theme.o == 0) {

    if(is.null(point.o)) {point.o <- 0}
    if(is.null(bean.o)) {bean.o <- 0}
    if(is.null(inf.o)) {inf.o <- 0}
    if(is.null(line.o)) {line.o <- 0}
    if(is.null(bar.o)) {bar.o <- 0}

  }

  ## Repeat transparency values

  point.o <- rep(point.o, length.out = n.beans)
  bean.o <- rep(bean.o, length.out = n.beans)
  inf.o <- rep(inf.o, length.out = n.beans)
  line.o <- rep(line.o, length.out = n.beans)
  bar.o <- rep(bar.o, length.out = n.beans)


  # Get colors

  # If palette is in piratepal()...

  if(mean(pal %in% piratepal("names")) == 1) {

    if (is.null(point.col)) {

      point.col <- piratepal(palette = pal,
                             length.out = n.cols,
                             trans = 0)


      } else {

        point.col <- rep(transparent(point.col,
                                     trans.val = 0),
                                     length.out = n.cols)
        }


    if (is.null(bean.border.col)) {

      bean.border.col <- piratepal(palette = pal,
                             length.out = n.cols,
                             trans = 0)

    } else {

      bean.border.col <- rep(transparent(bean.border.col,
                                   trans.val = 0),
                       length.out = n.cols)
    }


    if (is.null(inf.col)) {

      inf.col <- piratepal(palette = pal,
                                   length.out = n.cols,
                                   trans = 0)

    } else {

      inf.col <- rep(transparent(inf.col,
                                         trans.val = 0),
                             length.out = n.cols)
    }


    if (is.null(average.line.col)) {

      average.line.col <- piratepal(palette = pal,
                                length.out = n.cols,
                                trans = 0)

    } else {

      average.line.col <- rep(transparent(average.line.col,
                                          trans.val = 0),
                                          length.out = n.cols)
    }

    if (is.null(bar.col)) {

      bar.col <- piratepal(palette = pal,
                                    length.out = n.cols,
                                    trans = 0)

    } else {

      bar.col <- rep(transparent(bar.col,
                                trans.val = 0),
                              length.out = n.cols)
    }



  }

  # If palette is NOT in piratepal()...

  if(mean(pal %in% piratepal("names")) != 1) {

    if(length(pal) < n.cols) {pal <- rep(pal, n.cols)}

    if(is.null(point.col)) {

    point.col <- rep(pal, length.out = n.cols)

    } else {

      point.col <- rep(point.col, length.out = n.cols)

    }


    if(is.null(bean.border.col)) {

      bean.border.col <- rep(pal, length.out = n.cols)

    } else {

      bean.border.col <- rep(bean.border.col, length.out = n.cols)

    }

    if(is.null(inf.col)) {

      inf.col <- rep(pal, length.out = n.cols)

    } else {

      inf.col <- rep(inf.col, length.out = n.cols)

    }


    if(is.null(average.line.col)) {

      average.line.col <- rep(pal, length.out = n.cols)

    } else {

      average.line.col <- rep(average.line.col, length.out = n.cols)

    }


    if(is.null(bar.col)) {

      bar.col <- rep(pal, length.out = n.cols)

    } else {

      bar.col <- rep(bar.col, length.out = n.cols)

    }






  }


  # Make colors transparent according to transparency (point.o, bar.o, ...) vectors


  if(n.iv == 1) {

    for(i in 1:n.beans) {

      point.col[i] <- transparent(point.col[i], trans.val = 1 - point.o[i])
      bean.border.col[i] <- transparent(bean.border.col[i], trans.val = 1 - bean.o[i])
      inf.col[i] <- transparent(inf.col[i], trans.val = 1 - inf.o[i])
      average.line.col[i] <- transparent(average.line.col[i], trans.val = 1 - line.o[i])
      bar.col[i] <- transparent(bar.col[i], trans.val = 1 - bar.o[i])

    }

  }

  if(n.iv == 2) {

    point.col <- rep(point.col, times = iv.lengths[2])
    bean.border.col <- rep(bean.border.col, times = iv.lengths[2])
    inf.col <- rep(inf.col, times = iv.lengths[2])
    average.line.col <- rep(average.line.col, times = iv.lengths[2])
    bar.col <- rep(bar.col, times = iv.lengths[2])


    point.o <- rep(point.o, times = 2)
    bean.o <- rep(bean.o, times = 2)
    inf.o <- rep(inf.o, times = 2)
    line.o <- rep(line.o, times = 2)
    bar.o <- rep(bar.o, times = 2)



    for(i in 1:n.beans) {

      point.col[i] <- transparent(point.col[i], trans.val = 1 - point.o[i])
      bean.border.col[i] <- transparent(bean.border.col[i], trans.val = 1 - bean.o[i])
      inf.col[i] <- transparent(inf.col[i], trans.val = 1 - inf.o[i])
      average.line.col[i] <- transparent(average.line.col[i], trans.val = 1 - line.o[i])
      bar.col[i] <- transparent(bar.col[i], trans.val = 1 - bar.o[i])

    }


  }



  if(is.null(bar.border.col)) {bar.border.col <- bar.col}
  if(is.null(bar.border.col) == F) {bar.border.col <- rep(bar.border.col, times = length(bar.col))}



  # Create plotting space

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

  if(n.iv == 1 & is.null(xlab)) {my.xlab <- iv.names[1]}
  if(n.iv == 1 & is.null(xlab) == F) {my.xlab <- xlab}

  if(n.iv > 1) {my.xlab <- ""}


  if(is.null(ylab)) {ylab <- dv.name}
}


# -----
#  MAIN PLOT
# ------

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

 # mtext(side = 1, text = my.xlab)
 # mtext(side = 1, text = my.xlab, family = family)


  # Add labels




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

# BEANS
{

if(is.na(width.max)) {

  if(n.iv == 1) {width.max <- .45}
  if(n.iv == 2) {width.max <- .5}

}


bean.lwd <- rep(bean.lwd, length.out = n.beans)
inf.lwd <- rep(inf.lwd, length.out = n.beans)
line.lwd <- rep(line.lwd, length.out = n.beans)
bar.border.lwd <- rep(bar.border.lwd, length.out = n.beans)

  for (bean.i in 1:n.beans) {

    dv.i <- data.2[data.2$bean.num == bean.i, dv.name]

    if(is.logical(dv.i)) {dv.i <- as.numeric(dv.i)}

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


  if(length(setdiff(dv.i, c(0, 1))) > 0) {

  polygon(c(x.loc.i - dens.y.plot.i[1:(length(dens.x.plot.i))],
            x.loc.i + rev(dens.y.plot.i[1:(length(dens.x.plot.i))])),
          c(dens.x.plot.i[1:(length(dens.x.plot.i))],
            rev(dens.x.plot.i[1:(length(dens.x.plot.i))])),
          col = gray(1, alpha = bean.o[bean.i]),
          border = bean.border.col[bean.i],
          lwd = bean.lwd[bean.i]
  )

  }

    }

    }


    # Add raw data

#if(setequal(dv.i, c(0, 1)) == F) {

    points(rep(x.loc.i, length(dv.i)) + rnorm(length(dv.i), mean = 0, sd = jitter.val),
           dv.i,
           pch = point.pch,
           col = point.col[bean.i],
           cex = point.cex,
           lwd = point.lwd
    )

#}

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

    if(inf.o[bean.i] > 0 & length(dv.i) > 3 & sd(dv.i) > 0) {

      if(length(dv.i) <= 3) {

        message(paste("Note: Group ", bean.i, " had too few observations (", length(dv.i), ") for an inference band", sep = ""))
        message(paste("Note: Group", bean.i, "had no variance, so no inference band :("))

        }


# Binary data

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

# Non-Binary data

if(length(setdiff(dv.i, c(0, 1))) > 0) {

if(inf == "hdi") {

ttest.bf <- BayesFactor::ttestBF(dv.i, posterior = T, iterations = hdi.iter, progress = F)
samples <- ttest.bf[,1]

# using the hdi function from Kruschke

inf.lb <- hdi(samples)[1]
inf.ub <- hdi(samples)[2]

  }

if(inf == "ci") {

ci.i <- t.test(dv.i, conf.level = inf.p)$conf.int

inf.lb <- ci.i[1]
inf.ub <- ci.i[2]

}
}

dens.inf.x <- dens.x.i[dens.x.i >= inf.lb & dens.x.i <= inf.ub]
dens.inf.y <- dens.y.i[dens.x.i >= inf.lb & dens.x.i <= inf.ub]

# Draw HDI band

rect(x.loc.i - width.max * .8,
     inf.lb,
     x.loc.i + width.max * .8,
     inf.ub,
     col = inf.col[bean.i],
     lwd = inf.lwd[bean.i],
     border = NA)

}

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

# Add bean names for IV 1

if(n.iv == 1) {line.t <- .5}
if(n.iv == 2) {line.t <- 2}

if(is.null(xaxt) == T) {

  mtext(bean.mtx[,1],
        side = 1,
        at = bean.mtx$x.loc,
        line = line.t,
        cex = cex.lab)


  # Add names for IV 2

  if(n.iv == 2) {

    mtext(iv.names[1], side = 1, line = 2, at = par("usr")[1], adj = 1)

    mtext(iv.names[2], side = 1, line = .5, at = par("usr")[1], adj = 1)

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

# ----
# EVIDENCE
# ----
#
# if(evidence) {
#
#   # Convert IVs to factors
#
# for(iv.i in 1:n.iv) {
#
# data.2[,iv.i] <- as.factor(data.2[,iv.i])
#
#   }
#
# bf <- BayesFactor::anovaBF(formula = formula, data = data.2[,1:(n.iv + 1)])
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
# if(n.iv == 1) {bar.loc.vec <- c(.5) ; bar.widths <- .1}
# if(n.iv == 2) {bar.loc.vec <- c(.3, .7) ; bar.widths <- .1}
#
# for(iv.i in 1:n.iv) {
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



