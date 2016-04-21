#' pirateplot
#'
#' The pirateplot function creates an RDI plot (Raw data, Descriptive and Inferential statistic) pirate version of the fantastic beanplot function in the beanplot package. Just like a beanplot, pirateplot takes a discrete iv and a continuous dv, and creates a plot showing raw data, smoothed densities and central tendency. In addition, pirateplot adds the option for a 95% Highest Density Intervals (HDI), and has a few aesthetic differences preferred by pirates.
#'
#' @param formula (formula) A formula in the form y ~ x1 + x2 indicating the vertical response variable (y) and 1 or two independent varaibles
#' @param data (dataframe) Data which to perform the beanplot on. This data can consist of dataframes, vectors and/or formulas. For each formula, a dataset can be specified with data=[dataset], and a subset can be specified with subset=[subset]. If subset/data arguments are passed, but there are not enough subset/data arguments, they are reused. Additionally, na.action, drop.unused.levels and xlev can be passed to model.frame in the same way. Also, parameters for axis and title can be passed.
#' @param line.fun (function) A function that determines how average lines and bar heights are determined (default is mean).
#' @param line.lwd (numeric) A number indicating the width of the average lines.
#' @param pal (string) A string indicating the color palette of the plot. Can be a single color, a vector of colors, or the name of a palette in the piratepal() function (e.g.; "basel", "google", "southpark"). To see all the palettes, run piratepal(palette = "all", action = "show")
#' @param gl.col (string) An optional string indicating the color of the horizontal gridlines.
#' @param bar.border.col (string) An optional string indicating the color of the borders of the bars.
#' @param back.col (string) A string indicating the color of the plotting background
#' @param point.cex,point.pch,point.lwd (numeric) Numbers indicating the size, pch type, and line width of raw data points.
#' @param width.min,width.max (numeric) The minimum and maximum width of a bean.
#' @param cut.min, cut.max (numeric) Optimal minimum and maximum values of the beans.
#' @param theme.o (integer) An integer in the set 0, 1, 2, 3, specifying an opacity theme (that is, specific values of bar.o, point.o, etc.). You can override specific opacity values in a theme by specifying bar.o, hdi.o (etc.)
#' @param bar.o,point.o,hdi.o,line.o,bean.o (numeric) A number between 0 and 1 indicating how opaque to make the bars, points, hdi band, average line, and beans respectively. These values override whatever is in the specified theme
#' @param hdi.iter (integer) An integer indicating how many iterations to run when calculating the HDI. Larger values lead to better estimates, but can be (very) time consuming.
#' @param bw (string) The smoothing bandwidth to use for the bean. (see ?density)
#' @param adjust (numeric) Adjustment for the bandwidth (see ?density)
#' @param jitter.val (numeric) A number indicaing how much to jitter the points horizontally. Defaults to 0.05.
#' @param at (numeric) An optional vector specifying the locations of the beans
#' @param add (logical) A logical value indicating whether to add the pirateplot to an existing plotting space or not.
#' @param ... other arguments passed on to the plot function (e.g.; main, xlab, ylab, ylim)
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
#'# Plot 3: Theme 2
#'
#'pirateplot(formula = weight ~ Diet,
#'           data = ChickWeight,
#'           main = "Theme 2",
#'           theme.o = 2
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
#'           main = "Theme 3\nHDIs take time to calculate...",
#'           theme.o = 3
#')
#'
#'# Plot 6: Theme 3 + white on black
#'
#'pirateplot(formula = weight ~ Diet,
#'           data = ChickWeight,
#'           main = "Theme 3 + white on black\nHDIs take time to calculate...",
#'           pal = "white",
#'           theme.o = 3,
#'           point.pch = 16,
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
#'           point.pch = 16,
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
#'           point.pch = 16,
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
#'           point.pch = 16,
#'           main = "Two IVs\nTheme 1, default palette"
#'           )
#'
#'# Theme 2
#'
#'pirateplot(formula = weight ~ Diet + Time,
#'           data = subset(ChickWeight, Time < 10),
#'           theme.o = 2,
#'           pal = "basel",
#'           point.pch = 16,
#'           main = "Two IVs\nTheme 2, Basel palette"
#'           )
#'
#'# Theme 3
#'
#'pirateplot(formula = weight ~ Diet + Time,
#'           data = subset(ChickWeight, Time < 10),
#'           theme.o = 3,
#'           pal = "ipod",
#'           point.pch = 16,
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
#'           hdi.o = 0,
#'           pal = "ipod",
#'           point.pch = 16,
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
  point.pch = 1,
  point.lwd = 1,
  cut.min = NULL,
  cut.max = NULL,
  width.min = .3,
  width.max = NA,
  bean.o = NULL,
  point.o = NULL,
  bar.o = NULL,
  hdi.o = NULL,
  line.o = NULL,
  theme.o = 1,
  hdi.iter = 1e3,
  jitter.val = .03,
  line.lwd = 4,
  gl.col = NULL,
  ylim = NULL,
  xlim = NULL,
  xlab = NULL,
  ylab = NULL,
  main = NULL,
  yaxt = NULL,
  at = NULL,
  bw = "nrd0",
  adjust = 1,
  add = F,
  y.levels = NULL,
  bar.border.col = NULL,
  ...
) {

## TESTING



#
#   line.fun = mean
#   pal = "appletv"
#   back.col = gray(1)
#   point.cex = 1
#   point.pch = 1
#   point.lwd = 1
#   cut.min = ""
#   cut.max = ""
#   width.min = .3
#   width.max = NA
#   bean.o = NULL
#   point.o = NULL
#   bar.o = NULL
#   hdi.o = NULL
#   line.o = NULL
#   theme.o = 1
#   hdi.iter = 1e3
#   jitter.val = .03
#   line.lwd = 4
#   ylim = NULL
#   xlim = NULL
#   xlab = NULL
#   ylab = NULL
#   main = NULL
#   add = F
#   y.levels = NULL
# at = NULL
# yaxt = NULL
#
#
#
#
#
#   formula = Allowed_CMS_per_Infusion ~ Drug + POS
#   data = InfusedDrugsAnnual
#   theme.o = 1
#   #gl.col = gray(.8),
#   #ylim = c(0,13000),
#   point.pch = 16
#   main = "RA Infusion\nCost of Drug, by Place of Service, 2014 Data"
#   ylab = "Cost per Infusion (based on CMS ASP)"
#   xlab = "Drug, by Place of Service"
#
# ylim = c(0, 13000)










  data.2 <- model.frame(formula = formula,
                        data = data)

  dv.name <- names(data.2)[1]
  dv.v <- data.2[,1]

  # Determine levels of each IV

  iv.levels <- lapply(2:ncol(data.2), FUN = function(x) {unique(data.2[,x])})
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
    hdi.o <- ifelse(is.null(hdi.o), 0, hdi.o)
    line.o <- ifelse(is.null(line.o), .5, line.o)
    bar.o <- ifelse(is.null(bar.o), .5, bar.o)

  }

  if(theme.o == 2) {

    point.o <- ifelse(is.null(point.o), .8, point.o)
    bean.o <- ifelse(is.null(bean.o), .5, bean.o)
    hdi.o <- ifelse(is.null(hdi.o), 0, hdi.o)
    line.o <- ifelse(is.null(line.o), .1, line.o)
    bar.o <- ifelse(is.null(bar.o), .1, bar.o)

  }

  if(theme.o == 3) {

    point.o <- ifelse(is.null(point.o), .2, point.o)
    bean.o <- ifelse(is.null(bean.o), .2, bean.o)
    hdi.o <- ifelse(is.null(hdi.o), .8, hdi.o)
    line.o <- ifelse(is.null(line.o), 1, line.o)
    bar.o <- ifelse(is.null(bar.o), .1, bar.o)

  }

  if(theme.o == 0) {

    point.o <- ifelse(is.null(point.o), 0, point.o)
    bean.o <- ifelse(is.null(bean.o), 0, bean.o)
    hdi.o <- ifelse(is.null(hdi.o), 0, hdi.o)
    line.o <- ifelse(is.null(line.o), 0, line.o)
    bar.o <- ifelse(is.null(bar.o), 0, bar.o)

  }




  # Get colors

  if(mean(pal %in% piratepal(action = "p")) == 1) {

    col.vec <- rep(piratepal(palette = pal, length.out = n.cols))
    point.col <- piratepal(palette = pal, length.out = n.cols, trans = 1 - point.o)
    bean.border.col <- piratepal(palette = pal, length.out = n.cols, trans = 1 - bean.o)
    hdi.band.col <- piratepal(palette = pal, length.out = n.cols, trans = 1 - hdi.o)
    average.line.col <- piratepal(palette = pal, length.out = n.cols, trans = 1 - line.o)
    bar.col <- piratepal(palette = pal, length.out = n.cols, trans = 1 - bar.o)

  }

  if(mean(pal %in% piratepal(action = "p")) != 1) {

    if(length(pal) < n.cols) {pal <- rep(pal, n.cols)}

    col.vec <- pal
    point.col <- sapply(1:length(pal), function(x) {transparent(pal[x], trans.val = 1 - point.o)})
    bean.border.col <- sapply(1:length(pal), function(x) {transparent(pal[x], trans.val = 1 - bean.o)})
    hdi.band.col <- sapply(1:length(pal), function(x) {transparent(pal[x], trans.val = 1 - hdi.o)})
    average.line.col <- sapply(1:length(pal), function(x) {transparent(pal[x], trans.val = 1 - line.o)})
    bar.col <- sapply(1:length(pal), function(x) {transparent(pal[x], trans.val = 1 - bar.o)})

  }

  if(n.iv == 2) {

    col.vec <- rep(col.vec, times = iv.lengths[2])
    point.col <- rep(point.col, times = iv.lengths[2])
    bean.border.col <- rep(bean.border.col, times = iv.lengths[2])
    hdi.band.col <- rep(hdi.band.col, times = iv.lengths[2])
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
    plot.max <- plot.min + 10 * best.step.size

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
    plot.max <- ylim[1] + 10 * best.step.size

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
       ...
  )


  # Add y-axis

  if(is.null(yaxt)) {

  axis(side = 2,
       at = y.levels,
       las = 1,
       lwd = 1,
       lwd.ticks = 1)

  }

  # Add background

    rect(-Inf, -Inf, Inf, Inf, col = back.col, border = NA)

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
         lwd = 1
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
              lwd = 2
      )

    }

    }



    # Add Line
    segments(x.loc.i - width.max,
             fun.val,
             x.loc.i + width.max,
             fun.val,
             col = average.line.col[bean.i],
             lwd = line.lwd,
             lend = 3
    )



    # Add HDI band

    if(hdi.o > 0) {

      hdi.i <- BEST::hdi(BEST::BESTmcmc(dv.i,
                                        numSavedSteps = hdi.iter,
                                        verbose = F))

      hdi.lb <- hdi.i[1, 1]
      hdi.ub <- hdi.i[2, 1]


      dens.hdi.x <- dens.x.i[dens.x.i >= hdi.lb & dens.x.i <= hdi.ub]
      dens.hdi.y <- dens.y.i[dens.x.i >= hdi.lb & dens.x.i <= hdi.ub]


      band.type <- "wide"

      if(band.type == "constrained") {

        polygon(c(x.loc.i - dens.hdi.y, x.loc.i + rev(dens.hdi.y)),
                c(dens.hdi.x, rev(dens.hdi.x)),
                col = hdi.band.col[bean.i],
                border = NA,
                lwd = 2
        )

      }


      if(band.type == "wide") {


        rect(x.loc.i - width.max,
             hdi.lb,
             x.loc.i + width.max,
             hdi.ub,
             col = hdi.band.col[bean.i], border = NA)



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




    # Add raw data

    points(rep(x.loc.i, length(dv.i)) + rnorm(length(dv.i), mean = 0, sd = jitter.val),
           dv.i,
           pch = point.pch,
           col = point.col[bean.i],
           cex = point.cex,
           lwd = point.lwd
    )






  }

  # Add bean names for IV 1

  mtext(bean.mtx[,1],
        side = 1,
        at = bean.mtx$x.loc,
        line = .5)

  # Add names for IV 2

  if(n.iv == 2) {


      text.loc <- (iv.lengths[1] + 1) / 2 * (2 *(1:iv.lengths[2]) - 1)


    mtext(text = paste(names(bean.mtx)[2], "=", unique(bean.mtx[,2])),
          side = 1,
          line = 2,
          at = text.loc
    )

  }




}

