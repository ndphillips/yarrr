#' pirateplot
#'
#' The pirateplot function creates an RDI plot (Raw data, Descriptive and Inferential statistic) pirate version of the fantastic beanplot function in the beanplot package. Just like a beanplot, pirateplot takes a discrete iv and a continuous dv, and creates a plot showing raw data, smoothed densities and central tendency. In addition, pirateplot adds the option for a 95% Highest Density Intervals (HDI), and has a few aesthetic differences preferred by pirates.
#'
#' @param dv.name, iv.name (string) Two strings indicating the names of the dv and iv in the dataframe
#' @param data, (dataframe) Data which to perform the beanplot on. This data can consist of dataframes, vectors and/or formulas. For each formula, a dataset can be specified with data=[dataset], and a subset can be specified with subset=[subset]. If subset/data arguments are passed, but there are not enough subset/data arguments, they are reused. Additionally, na.action, drop.unused.levels and xlev can be passed to model.frame in the same way. Also, parameters for axis and title can be passed.
#' @param add.mean, add.median (logical) Logical values indicating whether or not to include median and median lines.
#' @param background (binary) A number indicating which type of background to use. 1 creates a gray background with white horizontal gridlines.
#' @param point.cex (numeric) A number indicaing the size of the raw data points. Defaults to 1.
#' @param jitter.val (numeric) A number indicaing how much to jitter the points. Defaults to 0.05.
#' @param add.hdi (logical) A logical value indicating whether or not to add 95\% Highest Density Interval (HDI) bands. If T, HDIs will be calculated using the BESTmcmc function from the BEST package. Note: Calculating HDIs can be time-consuming!
#' @param n.iter (integer) An integer indicating how many iterations to run when calculating the HDI.
#' @param labels (string) A string vector indicating the names of the beans
#' @param width.min, width.max (numeric) The minimum and maximum width of a bean. Defaults to 0.5
#' @param cut.min, cut.max (numeric) The minimum and maximum width of a bean. Defaults to 0.5
#' @param my.palette (string) A string (or vector of strings) indicating the colors of the beans. This can either be the name of a color palette in the piratepal function (run piratepal(action = "p") to see the names of all the palettes), or a vector of strings referring to colors.
#' @param trans.vec (numeric) A numeric vector of 5 values between 0 and 1 that indicate how transparent to make the colors in each bean. The four numbers correspond to the points, bean outlines, hdi band, the average line, and the white background respectively.
#' @param add.margin.desc (logical) A logical value indicating whether or not to add a description of the average line (and possible HDI) to the plot margins.
#' @param ... other arguments passed on to the plot function (e.g.; main, xlab, ylab, ylim)
#' @keywords plot
#' @export
#' @examples
#'
#'
#' # ChickWeight Dataset
#' pirateplot(dv.name = "weight",
#'         iv.name = "Diet",
#'         data = ChickWeight)
#'
#'
#' # pirates dataset
#' pirateplot(dv.name = "age",
#'         iv.name = "sword.type",
#'         data = pirates,
#'         my.palette = "google"
#'         )
#'
#'
#'  # Some made-up survey data
#'
#'PhoneData <- data.frame("Phone" = rep(c("iPhone", "Android", "Windows"), each = 50),
#'                        "Age" = c(rnorm(50, mean = 25, sd = 4),
#'                                  rnorm(50, mean = 20, sd = 3),
#'                                  rnorm(50, mean = 30, sd = 3)
#'                                  ),
#'                        stringsAsFactors = F
#'                        )
#'
#'  pirateplot(dv.name = "Age",
#'          iv.name = "Phone",
#'          data = PhoneData,
#'          width.min = .45,
#'          my.palette = "black",
#'          main = "Age of Phone Users"
#'          )
#'
#'
#'
#'
#'
#'
#'

pirateplot <- function(dv.name,
                    iv.name,
                    data,
                    jitter.val = .03,
                    my.palette = "appletv",
                    add.mean = T,
                    add.median = T,
                    background = 1,
                    labels = "",
                    ylim = "",
                    ylab = "",
                    xlab = "",
                    add.hdi = T,
                    point.cex = 1,
                    cut.min = "",
                    cut.max = "",
                    add.margin.desc = T,
                    width.max = .45,
                    width.min = .2,
                    trans.vec = c(.5, .8, .5, 0, .2),
                    n.iter = 1e4,
                    ...
                    ) {

iv.v <- unlist(data[iv.name])
dv.v <- unlist(data[dv.name])

n.iv <- length(unique(iv.v))

# Get colors

if(mean(my.palette %in% piratepal(action = "p")) == 1) {

col.vec <- piratepal(palette = my.palette, length.out = n.iv)
point.col <- piratepal(palette = my.palette, length.out = n.iv, trans = trans.vec[1])
bean.border.col <- piratepal(palette = my.palette, length.out = n.iv, trans = trans.vec[2])
hdi.band.col <- piratepal(palette = my.palette, length.out = n.iv, trans = trans.vec[3])
average.line.col <- piratepal(palette = my.palette, length.out = n.iv, trans = trans.vec[4])

}


if(mean(my.palette %in% piratepal(action = "p")) != 1) {

  if(length(my.palette) < n.iv) {my.palette <- rep(my.palette, n.iv)}
  col.vec <- my.palette
  point.col <- sapply(1:length(my.palette), function(x) {transparent(my.palette[x], trans.val = trans.vec[1])})
  bean.border.col <- sapply(1:length(my.palette), function(x) {transparent(my.palette[x], trans.val = trans.vec[2])})
  hdi.band.col <- sapply(1:length(my.palette), function(x) {transparent(my.palette[x], trans.val = trans.vec[3])})
  average.line.col <- sapply(1:length(my.palette), function(x) {transparent(my.palette[x], trans.val = trans.vec[4])})

}

# Create plotting space

total.dv.sd <- sd(dv.v)

if(mean(ylim == "") == 1) {my.ylim <- c(min(dv.v) - total.dv.sd * .5, max(dv.v) + total.dv.sd * .5)}
if(mean(ylim == "") != 1) {my.ylim <- ylim ; rm(ylim)}

plot(1,
     xlim = c(.5, n.iv + .5),
     ylim = my.ylim,
     type = "n",
     xlab = "",
     ylab = "",
     xaxt = "n", ...
     )

# Add background

if(background == 1) {

  rect(-1000, -1000, 10000, 10000, col = gray(.97))

y.range <- max(dv.v) + total.dv.sd * .5 - min(dv.v) - total.dv.sd * .5

abline(h = seq(floor(min(dv.v) - total.dv.sd * .5),
               ceiling(max(dv.v) + total.dv.sd * .5),
               length.out = 10),
       col = "white")


}

# Add beans

for (i in 1:n.iv) {

  dv.i <- dv.v[iv.v == sort(unique(iv.v))[i]]
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


  # Add bean

  polygon(c(i - dens.y.plot.i[1:(length(dens.x.plot.i))], i + rev(dens.y.plot.i[1:(length(dens.x.plot.i))])),
          c(dens.x.plot.i[1:(length(dens.x.plot.i))], rev(dens.x.plot.i[1:(length(dens.x.plot.i))])),
          col = gray(1, alpha = 1 - trans.vec[5]),
          border = bean.border.col[i],
          lwd = 2
  )

  # Add HDI band

  if(add.hdi == T) {

    hdi.i <- hdi(BESTmcmc(dv.i,
                          numSavedSteps = n.iter,
                          verbose = F))

    hdi.lb <- hdi.i[1, 1]
    hdi.ub <- hdi.i[2, 1]


    dens.hdi.x <- dens.x.i[dens.x.i >= hdi.lb & dens.x.i <= hdi.ub]
    dens.hdi.y <- dens.y.i[dens.x.i >= hdi.lb & dens.x.i <= hdi.ub]

    polygon(c(i - dens.hdi.y, i + rev(dens.hdi.y)),
           c(dens.hdi.x, rev(dens.hdi.x)),
            col = hdi.band.col[i],
            border = NA,
            lwd = 2
    )

  }


  # Add mean and median line(s)

  mean.i <- mean(dv.i)
  median.i <- median(dv.i)

  mean.i.dens <-  dens.y.i[abs(dens.x.i - mean.i) == min(abs(dens.x.i - mean.i))]
  median.i.dens <-  dens.y.i[abs(dens.x.i - median.i) == min(abs(dens.x.i - median.i))]

  if(add.mean == T) {

  segments(i - mean.i.dens,
           mean.i,
           i + mean.i.dens,
           mean.i,
         col = average.line.col[i],
         lty = 1,
         lwd = 4
  )

  }

  if(add.median == T) {

  segments(i - median.i.dens,
           median.i,
           i + median.i.dens,
           median.i,
           col = average.line.col[i],
           lty = 2,
           lwd = 2
  )

  }



  # Add raw data

  points(rep(i, length(dv.i)) + rnorm(length(dv.i), mean = 0, sd = jitter.val),
         dv.i,
         pch = 1,
         col = point.col[i],
         cex = point.cex
         )






}

# Add bean names

if(mean(labels == "") == 1) {labels <- sort(unique(iv.v))}

mtext(labels,
      side = 1,
      at = 1:n.iv,
      line = 1)


# Add margin description

if(add.margin.desc) {

 if(add.hdi) {margin.text <- paste("Horizontal bands are 95% HDIs of the population mean.", sep = "")}
if(add.hdi == F) {margin.text <- ""}


  mtext(margin.text, side = 3, line = .1, cex = .7)

}

# Add x and y labels

if(mean(xlab == "") == 1) {my.xlab <- iv.name}
if(mean(xlab == "") != 1) {my.xlab <- xlab ; rm(xlab)}

mtext(my.xlab,
      side = 1,
      at = (n.iv + 1) / 2,
      line = 2.5
      )

if(mean(ylab == "") == 1) {my.ylab <- dv.name}
if(mean(ylab == "") != 1) {my.ylab <- ylab ; rm(ylab)}


mtext(my.ylab,
      side = 2,
      line = 2.5,
      las = 1
)



}


