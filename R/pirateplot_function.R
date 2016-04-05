#' pirateplot
#'
#' The pirateplot function creates an RDI plot (Raw data, Descriptive and Inferential statistic) pirate version of the fantastic beanplot function in the beanplot package. Just like a beanplot, pirateplot takes a discrete iv and a continuous dv, and creates a plot showing raw data, smoothed densities and central tendency. In addition, pirateplot adds the option for a 95% Highest Density Intervals (HDI), and has a few aesthetic differences preferred by pirates.
#'
#' @param formula (formula) A formula in the form dv ~ iv indicating the dv and iv to be plotted.
#' @param data, (dataframe) Data which to perform the beanplot on. This data can consist of dataframes, vectors and/or formulas. For each formula, a dataset can be specified with data=[dataset], and a subset can be specified with subset=[subset]. If subset/data arguments are passed, but there are not enough subset/data arguments, they are reused. Additionally, na.action, drop.unused.levels and xlev can be passed to model.frame in the same way. Also, parameters for axis and title can be passed.
#' @param line.fun (function) A function that determines how average lines and bar heights are determined (default is mean).
#' @param pal (string) A string indicating the color palette of the plot. Can be a single color, or the name of a palette in the piratepal() function (e.g.; "basel", "google", "southpark")
#' @param backcol (string) A string indicating the color of the plotting background
#' @param point.cex, point.pch, point.lwd (numeric) Numbers indicating the size, pch type, and line width of raw data points.
#' @param width.min, width.max (numeric) The minimum and maximum width of a bean.
#' @param cut.min, cut.max (numeric) Optimal minimum and maximum values of the beans.
#' @param bar.o, point.o, hdi.o, line.o, bean.o (numeric) A number between 0 and 1 indicating how opaque to make the bars, points, hdi band, average line, and beans respectively.
#' @param hdi.iter (integer) An integer indicating how many iterations to run when calculating the HDI.
#' @param jitter.val (numeric) A number indicaing how much to jitter the points. Defaults to 0.05.
#' @param ... other arguments passed on to the plot function (e.g.; main, xlab, ylab, ylim)
#' @keywords plot
#' @export
#' @examples
#'
#'
#'
#'# Pirateplots of the ChickWeight dataframe
#'
#'par(mfrow = c(2, 3))
#'
#'# Plot 1: Colorful pirateplot
#'
#'pirateplot(formula = weight ~ Diet,
#'           data = ChickWeight,
#'           main = "All Elements",
#'           hdi.o = .7,
#'           bar.o = .1,
#'           line.o = 1
#')
#'
#'
#'# Plot 2: Black and white, turn down the beans and points,
#'#   Turn up the HDI
#'
#'pirateplot(formula = weight ~ Diet,
#'           data = ChickWeight,
#'           main = "B&W with 95% HDI",
#'           pal = "black",
#'           bar.o = .1,
#'           hdi.o = .5,
#'           point.o = .1,
#'           bean.o = 0
#')
#'
#'# Plot 3: Turn everything off except the bars
#'
#'pirateplot(formula = weight ~ Diet,
#'           data = ChickWeight,
#'           main = "Super boring bars",
#'           pal = "black",
#'           bar.o = .9,
#'           hdi.o = 0,
#'           point.o = 0,
#'           bean.o = 0,
#'           back.col = "white",
#'           line.o = 0
#')
#'
#'
#'# Plot 3: Only points, but larger with more jitter
#'
#'pirateplot(formula = weight ~ Diet,
#'           data = ChickWeight,
#'           main = "Large, jittered points\nGoogle colors",
#'           pal = "google",
#'           bar.o = 0,
#'           hdi.o = 0,
#'           point.o = .1,
#'           bean.o = 0,
#'           back.col = "white",
#'           line.o = 1,
#'           point.pch = 16,
#'           point.cex = 1.5,
#'           jitter.val = .1
#')
#'
#'# Plot 3: Beans and HDI
#'
#'pirateplot(formula = weight ~ Diet,
#'           data = ChickWeight,
#'           main = "Beans and HDI\nSouth Park colors",
#'           pal = "southpark",
#'           bar.o = 0,
#'           hdi.o = .5,
#'           point.o = 0,
#'           bean.o = 1,
#'           back.col = "white",
#'           line.o = 0,
#'           point.pch = 16
#')
#'
#'
#'# Plot 3: Beans and HDI
#'
#'pirateplot(formula = weight ~ Diet,
#'           data = ChickWeight,
#'           main = "HDI and points\nDark background",
#'           pal = "ipod",
#'           bar.o = .2,
#'           point.o = .3,
#'           bean.o = 0,
#'           back.col = gray(.3),
#'           line.o = 0,
#'           hdi.o = .8,
#'           point.pch = 16
#')
#'
#'par(mfrow = c(1, 1))
#'
#'
#'
#'
#'
#'
#'

pirateplot <- function(
  formula,
  data,
  line.fun = mean,
  pal = "appletv",
  back.col = gray(1),
  point.cex = 1,
  point.pch = 1,
  point.lwd = 1,
  cut.min = "",
  cut.max = "",
  width.min = .3,
  width.max = NA,
  bean.o = .5,
  point.o = .5,
  bar.o = .5,
  hdi.o = 0,
  line.o = .5,
  hdi.iter = 1e3,
  jitter.val = .03,
  ylim = "",
  xlim = "",
  ...
) {

## TESTING

  #   jitter.val = .03
  #   pal = "black"
  #   line.fun = mean
  #   background = 1
  #   labels = ""
  #   cex.lab = 1
  #   point.cex = 1
  #   point.pch = 1
  #   point.lwd = 1
  #   cut.min = ""
  #   cut.max = ""
  #   width.max = .45
  #   width.min = .2
  #   bean.o = .5
  #   point.o = .5
  #   bar.o = .5
  #   hdi.o = 1
  #   line.o = .5
  #   hdi.iter = 1e3
  #   ylim = ""
  #   xlim = ""
  #
  #
  #
  #
  #
  # formula = time.s ~ Company
  # line.fun = mean
  # data = df


  data.2 <- model.frame(formula = formula,
                        data = data)

  dv.name <- names(data.2)[1]
  dv.v <- data.2[,1]

  # Determine levels of each IV

  iv.levels <- lapply(2:ncol(data.2), FUN = function(x) {sort(unique(data.2[,x]))})
  iv.lengths <- sapply(1:length(iv.levels), FUN = function(x) {length(iv.levels[[x]])})
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

  bean.loc <- 1:n.beans

  group.spacing <- 1

  if(n.iv == 2) {

    bean.loc <- bean.loc + rep(group.spacing * (0:(iv.lengths[2] - 1)), each = iv.lengths[1])

  }

  bean.mtx$x.loc <- bean.loc

  data.2 <- merge(data.2, bean.mtx)


  n.cols <- iv.lengths[1]

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



  # Create plotting space
  total.dv.sd <- sd(dv.v)

  if(mean(ylim == "") == 1) {my.ylim <- c(min(dv.v) - total.dv.sd * .5, max(dv.v) + total.dv.sd * .5)}
  if(mean(ylim == "") != 1) {my.ylim <- ylim ; rm(ylim)}

  par(mar = c(5, 4, 4, 1) + .1)

  plot(1,
       xlim = c(min(bean.loc) - .5, max(bean.loc) + .5),
       ylim = my.ylim,
       type = "n",
       xaxt = "n",
       ...
  )

  # Add background

    rect(-1000, -1000, 10000, 10000, col = back.col)

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
         border = bar.col[bean.i],
         lwd = 0
    )



    # Add bean
    {
      # Calculate bean densities

    if(length(dv.i) > 5) {  # only if n > 5

      dens.i <- density(dv.i)

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

      if(is.numeric(cut.min)) {

        dens.x.plot.i <- dens.x.i[dens.x.i > cut.min]
        dens.y.plot.i <- dens.y.i[dens.x.i > cut.min]

      }


      if(is.numeric(cut.max)) {

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
             lwd = 4
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

