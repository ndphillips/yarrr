#' piratepal
#'
#' This function provides a number of color palettes
#'
#' @param palette A string defining the color palette to use (see examples). To use a random palette, use "random"
#' @param action Either "return" to return a vector of colors, "s" to show the full palette, or "showall" to plot all palettes
#' @param trans A number in the interval [0, 1] indicating how transparent to make the colors
#' @keywords colors
#' @export
#' @examples
#'
#' # Show all palettes
#'
#
#' piratepal(palette = "all", action = "show")
#'
#'# Show som2 palettes
#'
#'piratepal(palette = "nemo", action = "show")
#'piratepal(palette = "espresso", action = "show")
#'piratepal(palette = "bugs", action = "show")
#'
#'# Plot some baloons using the "up" palette
#'x <- rnorm(100, 0)
#' y <- rnorm(100, 2, 1.3)
#' plot(1, xlim = c(-7, 7), ylim = c(-7, 7),
#'      xlab = "", ylab = "", type = "n", xaxt = "n", yaxt = "n", bty = "n")
#'
#' rect(-2, -6, 2, -2)
#' polygon(c(-2, 0, 2),
#'         c(-2, 0, -2)
#' )
#' rect(-7, -7, -2, 100)
#' rect(2, -7, 7, 100)
#' rect(-.5, -6, .5, -4)
#' points(.3, -5)




piratepal <- function(palette = "random", action = "return", trans = 0) {


  palette.names <- c("ipod", "espresso", "info", "info2", "google", "drugs",
                     "goldfish", "provoking", "emo", "cake", "pancake", "lubitel",
                     "brave", "bugs", "cars", "nemo", "rat", "up"
                     )

  if(!(palette %in% c(palette.names, "random", "all"))) {

    return("You did not specify a valid palette. Please try again!")

  }


# Define all palettes
{

   # http://a.dilcdn.com/bl/wp-content/uploads/sites/2/2015/05/disneyPixar_PixarPalette_BRA_01.jpg

  brave.pal <- data.frame(
    "brown" = rgb(168, 100, 59, alpha = (1 - trans) * 255, maxColorValue = 255),
    "yellow" = rgb(182, 91, 35, alpha = (1 - trans) * 255, maxColorValue = 255),
    "red" = rgb(148, 34, 14, alpha = (1 - trans) * 255, maxColorValue = 255),
    "green" = rgb(39, 45, 23, alpha = (1 - trans) * 255, maxColorValue = 255),
    "blue" = rgb(32, 33, 38, alpha = (1 - trans) * 255, maxColorValue = 255),
    stringsAsFactors = F

  )

  # http://a.dilcdn.com/bl/wp-content/uploads/sites/2/2015/05/disneyPixar_PixarPalette_BUG_01.jpg

  bugs.pal <- data.frame(
    "green1" = rgb(102, 120, 64, alpha = (1 - trans) * 255, maxColorValue = 255),
    "green2" = rgb(186, 214, 168, alpha = (1 - trans) * 255, maxColorValue = 255),
    "blue" = rgb(133, 199, 193, alpha = (1 - trans) * 255, maxColorValue = 255),
    "brown1" = rgb(165, 154, 107, alpha = (1 - trans) * 255, maxColorValue = 255),
    "brown2" = rgb(103, 85, 63, alpha = (1 - trans) * 255, maxColorValue = 255),
    stringsAsFactors = F

  )

  # http://a.dilcdn.com/bl/wp-content/uploads/sites/2/2015/05/disneyPixar_PixarPalette_CAR_01.jpg


  cars.pal <- data.frame(
    "peach" = rgb(231, 176, 143, alpha = (1 - trans) * 255, maxColorValue = 255),
    "purple" = rgb(136, 76, 73, alpha = (1 - trans) * 255, maxColorValue = 255),
    "red" = rgb(224, 54, 58, alpha = (1 - trans) * 255, maxColorValue = 255),
    "brown" = rgb(106, 29, 26, alpha = (1 - trans) * 255, maxColorValue = 255),
    "blue" = rgb(157, 218, 230, alpha = (1 - trans) * 255, maxColorValue = 255),
    stringsAsFactors = F

  )


  # http://a.dilcdn.com/bl/wp-content/uploads/sites/2/2015/05/disneyPixar_PixarPalette_FIN_021.jpg


  nemo.pal <- data.frame(
    "yellow" = rgb(251, 207, 53, alpha = (1 - trans) * 255, maxColorValue = 255),
    "orange" = rgb(237, 76, 28, alpha = (1 - trans) * 255, maxColorValue = 255),
    "brown" = rgb(156, 126, 112, alpha = (1 - trans) * 255, maxColorValue = 255),
    "blue1" = rgb(90, 194, 241, alpha = (1 - trans) * 255, maxColorValue = 255),
    "green" = rgb(17, 119, 108, alpha = (1 - trans) * 255, maxColorValue = 255),
    stringsAsFactors = F

  )

  # http://a.dilcdn.com/bl/wp-content/uploads/sites/2/2015/05/disneyPixar_PixarPalette_RAT_02.jpg

  rat.pal <- data.frame(
    "brown" = rgb(159, 77, 35, alpha = (1 - trans) * 255, maxColorValue = 255),
    "purple" = rgb(146, 43, 73, alpha = (1 - trans) * 255, maxColorValue = 255),
    "red" = rgb(178, 29, 19, alpha = (1 - trans) * 255, maxColorValue = 255),
    "green" = rgb(127, 134, 36, alpha = (1 - trans) * 255, maxColorValue = 255),
    "yellow" = rgb(241, 156, 31, alpha = (1 - trans) * 255, maxColorValue = 255),
    stringsAsFactors = F

  )

  # http://a.dilcdn.com/bl/wp-content/uploads/sites/2/2015/05/disneyPixar_PixarPalette_UP_02.jpg

  up.pal <- data.frame(
    "blue1" = rgb(95, 140, 244, alpha = (1 - trans) * 255, maxColorValue = 255),
    "blue2" = rgb(220, 214, 252, alpha = (1 - trans) * 255, maxColorValue = 255),
    "orange" = rgb(226, 122, 72, alpha = (1 - trans) * 255, maxColorValue = 255),
    "brown" = rgb(96, 86, 70, alpha = (1 - trans) * 255, maxColorValue = 255),
    "blue" = rgb(67, 65, 89, alpha = (1 - trans) * 255, maxColorValue = 255),
    stringsAsFactors = F

  )



  # Taken from a cellphone photo of espresso cups in the ARC kitchen

    espresso.pal<- data.frame(
      "purple" = rgb(39, 27, 48, alpha = (1 - trans) * 255, maxColorValue = 255),
      "blue" = rgb(35, 102, 192, alpha = (1 - trans) * 255, maxColorValue = 255),
      "yellow" = rgb(233, 215, 56, alpha = (1 - trans) * 255, maxColorValue = 255),
      "red" = rgb(185, 18, 38, alpha = (1 - trans) * 255, maxColorValue = 255),
      "green" = rgb(163, 218, 75, alpha = (1 - trans) * 255, maxColorValue = 255),
      "orange" = rgb(255, 100, 53, alpha = (1 - trans) * 255, maxColorValue = 255),
      stringsAsFactors = F

    )


    ipod.pal  <- data.frame(
    "lightgray" = rgb(215, 215, 215, alpha = (1 - trans) * 255, maxColorValue = 255),
    "red" = rgb(243, 174, 175, alpha = (1 - trans) * 255, maxColorValue = 255),
    "darkgray" = rgb(174, 173, 176, alpha = (1 - trans) * 255, maxColorValue = 255),
    "green" = rgb(158, 217, 191, alpha = (1 - trans) * 255, maxColorValue = 255),
    "blue" = rgb(92, 203, 235, alpha = (1 - trans) * 255, maxColorValue = 255),
    "yellow" = rgb(222, 235, 97, alpha = (1 - trans) * 255, maxColorValue = 255),
    "background" = rgb(242, 242, 242, alpha = (1 - trans) * 255, maxColorValue = 255),
    stringsAsFactors = F
  )

    info.pal <- data.frame(
    "red" = rgb(231, 105, 93, alpha = (1 - trans) * 255, maxColorValue = 255),
    "darkblue" = rgb(107, 137, 147, alpha = (1 - trans) * 255, maxColorValue = 255),
    "creme" = rgb(246, 240, 212, alpha = (1 - trans) * 255, maxColorValue = 255),
    "green" = rgb(149, 206, 138, alpha = (1 - trans) * 255, maxColorValue = 255),
    "lightgray" = rgb(210, 210, 210, alpha = (1 - trans) * 255, maxColorValue = 255),
    "lightblue" = rgb(148, 212, 212, alpha = (1 - trans) * 255, maxColorValue = 255),
    "lightgray" = rgb(150, 150, 150, alpha = (1 - trans) * 255, maxColorValue = 255),
    "background" = rgb(241, 243, 232, alpha = (1 - trans) * 255, maxColorValue = 255),
    "brown" = rgb(136, 119, 95, alpha = (1 - trans) * 255, maxColorValue = 255),
    stringsAsFactors = F
  )



    info2.pal <- data.frame(
    "darkblue" = rgb(0, 106, 64, alpha = (1 - trans) * 255, maxColorValue = 255),
    "pink" = rgb(240, 136, 146, alpha = (1 - trans) * 255, maxColorValue = 255),
    "lightgreen" = rgb(117, 180, 30, alpha = (1 - trans) * 255, maxColorValue = 255),
    "lightgray" = rgb(149, 130, 141, alpha = (1 - trans) * 255, maxColorValue = 255),
    "grayblue" = rgb(112, 140, 152, alpha = (1 - trans) * 255, maxColorValue = 255),
    "lightblue" = rgb(138, 184, 207, alpha = (1 - trans) * 255, maxColorValue = 255),
    "turquoise" = rgb(0, 126, 127, alpha = (1 - trans) * 255, maxColorValue = 255),
    "green" = rgb(53, 131, 89, alpha = (1 - trans) * 255, maxColorValue = 255),
    "paleblue" = rgb(139, 161, 188, alpha = (1 - trans) * 255, maxColorValue = 255),
    "purple" = rgb(90, 88, 149, alpha = (1 - trans) * 255, maxColorValue = 255),
    "orange" = rgb(242, 153, 12, alpha = (1 - trans) * 255, maxColorValue = 255),
    "purple" = rgb(90, 88, 149, alpha = (1 - trans) * 255, maxColorValue = 255),
    "paleorange" = rgb(229, 186, 58, alpha = (1 - trans) * 255, maxColorValue = 255),
    "salmon" = rgb(216, 108, 79, alpha = (1 - trans) * 255, maxColorValue = 255),
    stringsAsFactors = F
  )



    # http://www.google.com

    google.pal <- data.frame(
      "blue" = rgb(61, 121, 243, alpha = (1 - trans) * 255, maxColorValue = 255),
      "red" = rgb(230, 53, 47, alpha = (1 - trans) * 255, maxColorValue = 255),
      "yellow" = rgb(249, 185, 10, alpha = (1 - trans) * 255, maxColorValue = 255),
      "green" = rgb(52, 167, 75, alpha = (1 - trans) * 255, maxColorValue = 255),
      stringsAsFactors = F)




    # http://www.informationisbeautiful.net/visualizations/drugs-world/

   drugs.pal <- data.frame(
      "blue" = rgb(170, 220, 226, alpha = (1 - trans) * 255, maxColorValue = 255),
      "green" = rgb(162, 206, 37, alpha = (1 - trans) * 255, maxColorValue = 255),
      "yellow" = rgb(244, 238, 43, alpha = (1 - trans) * 255, maxColorValue = 255),
      "orange" = rgb(207, 79, 26, alpha = (1 - trans) * 255, maxColorValue = 255),
      "purple" = rgb(217, 82, 156, alpha = (1 - trans) * 255, maxColorValue = 255),
      "gray" = rgb(225, 224, 224, alpha = (1 - trans) * 255, maxColorValue = 255),
      stringsAsFactors = F
      )




    # http://www.colourlovers.com/palette/92095/Giant_Goldfish

    goldfish.pal <- data.frame(
      "blue" = rgb(105, 210, 231, alpha = (1 - trans) * 255, maxColorValue = 255),
      "green" = rgb(167, 219, 216, alpha = (1 - trans) * 255, maxColorValue = 255),
      "gray" = rgb(224, 228, 204, alpha = (1 - trans) * 255, maxColorValue = 255),
      "orange1" = rgb(243, 134, 48, alpha = (1 - trans) * 255, maxColorValue = 255),
      "orange2" = rgb(250, 105, 0, alpha = (1 - trans) * 255, maxColorValue = 255),
      stringsAsFactors = F
    )


    # http://www.colourlovers.com/palette/694737/Thought_Provoking

    provoking.pal <- data.frame(
      "yellow" = rgb(236, 208, 120, alpha = (1 - trans) * 255, maxColorValue = 255),
      "orange" = rgb(217, 91, 67, alpha = (1 - trans) * 255, maxColorValue = 255),
      "red" = rgb(192, 41, 66, alpha = (1 - trans) * 255, maxColorValue = 255),
      "brown" = rgb(84, 36, 55, alpha = (1 - trans) * 255, maxColorValue = 255),
      "blue" = rgb(83, 119, 122, alpha = (1 - trans) * 255, maxColorValue = 255),
      stringsAsFactors = F
    )


    # http://www.colourlovers.com/palette/1930/cheer_up_emo_kid

    emo.pal <- data.frame(
      "grayblue" = rgb(85, 98, 112, alpha = (1 - trans) * 255, maxColorValue = 255),
      "blue" = rgb(78, 205, 196, alpha = (1 - trans) * 255, maxColorValue = 255),
      "green" = rgb(199, 244, 100, alpha = (1 - trans) * 255, maxColorValue = 255),
      "salmon" = rgb(255, 107, 107, alpha = (1 - trans) * 255, maxColorValue = 255),
      "magenta" = rgb(196, 77, 88, alpha = (1 - trans) * 255, maxColorValue = 255),
      stringsAsFactors = F
    )



    # http://www.colourlovers.com/palette/49963/let_them_eat_cake

    cake.pal <- data.frame(
      "brown" = rgb(119, 79, 56, alpha = (1 - trans) * 255, maxColorValue = 255),
      "red" = rgb(224, 142, 121, alpha = (1 - trans) * 255, maxColorValue = 255),
      "creme1" = rgb(241, 212, 175, alpha = (1 - trans) * 255, maxColorValue = 255),
      "creme2" = rgb(236, 229, 206, alpha = (1 - trans) * 255, maxColorValue = 255),
      "blue" = rgb(197, 224, 220, alpha = (1 - trans) * 255, maxColorValue = 255),
      stringsAsFactors = F
    )



    # http://www.colourlovers.com/palette/443995/i_demand_a_pancake

    pancake.pal<- data.frame(
      "brown" = rgb(89, 79, 79, alpha = (1 - trans) * 255, maxColorValue = 255),
      "blue1" = rgb(84, 121, 128, alpha = (1 - trans) * 255, maxColorValue = 255),
      "blue2" = rgb(69, 173, 168, alpha = (1 - trans) * 255, maxColorValue = 255),
      "green" = rgb(157, 224, 173, alpha = (1 - trans) * 255, maxColorValue = 255),
      "yellow" = rgb(229, 252, 194, alpha = (1 - trans) * 255, maxColorValue = 255),
      stringsAsFactors = F
    )



    #   http://www.colourlovers.com/fashion/trends/street-fashion/7759/LUBITEL

    lubitel.pal <- data.frame(
      "blue" = rgb(55, 58, 69, alpha = (1 - trans) * 255, maxColorValue = 255),
      "gray" = rgb(194, 194, 194, alpha = (1 - trans) * 255, maxColorValue = 255),
      "brown1" = rgb(179, 152, 109, alpha = (1 - trans) * 255, maxColorValue = 255),
      "red" = rgb(120, 36, 29, alpha = (1 - trans) * 255, maxColorValue = 255),
      "brown2" = rgb(94, 62, 53, alpha = (1 - trans) * 255, maxColorValue = 255),
      stringsAsFactors = F
    )

}

if(palette == "random") {palette <- sample(palette.names[palette.names != "random"], 1)}

if(palette != "all") {palette.df <- get(paste(palette, ".pal", sep = ""))}


# Return color vector

if(substr(action, 1, 1) == "r") {

return(unlist(palette.df))

}


# Plot single palette

if(substr(action, 1, 1) == "s" & palette %in% palette.names) {

  palette.df <- get(paste(palette, ".pal", sep = ""))
  col.vec <- unlist(palette.df)
  n.colors <- length(col.vec)

  par(mar = c(0, 0, 0, 0))
  plot(1, xlim = c(0, 1), ylim = c(0, 1),
       type='n',xaxs='i',xaxt = "n", yaxt = "n",
       bty = "n", yaxs='i',xlab='',ylab='')



  # is there a picture?

  if(system.file(paste(palette, ".jpg", sep = ""), package="yarrr") != "") {

  point.heights <- .4
  text.heights <- .2
  pic.center <- c(.5, .65)

  require('jpeg')

  jpg <-  readJPEG(system.file(paste(palette, ".jpg", sep = ""), package="yarrr"), native=T) # read the file
  res <-  dim(jpg)[1:2] # get the resolution
  ar <- res[2] / res[1]

  desired.width <- .50
  required.height <- desired.width / ar

  rasterImage(jpg,
              pic.center[1] - desired.width / 2,
              pic.center[2] - required.height / 2,
              pic.center[1] + desired.width / 2,
              pic.center[2] + required.height / 2)


}

  if(floor(n.colors / 2) != n.colors / 2) {

    possible.locations <- seq(0, 1, 1 / 16)
    start.location <- ceiling(.5 * length(possible.locations)) -  floor(n.colors / 2)

  }
  if(floor(n.colors / 2) == n.colors / 2) {

    possible.locations <- seq(0, 1, 1 / 15)
    start.location <- .5 * length(possible.locations) - n.colors / 2 + 1

}

end.location <- start.location + n.colors - 1
  locations.to.use <- possible.locations[start.location:end.location]


if(system.file(paste(palette, ".jpg", sep = ""), package = "yarrr") == "") {

  point.heights <- .6
  text.heights <- .25
}

# Add segments

segments(locations.to.use, text.heights + .1, locations.to.use, point.heights, lwd = 1, lty = 2)


  # Add points

  points(x = locations.to.use, y = rep(point.heights, length(col.vec)),
         pch = 16, col = col.vec, cex = 10)

  text(locations.to.use, text.heights, names(col.vec), srt = 45)

  text(.5, .9, palette, cex = 2)


}

# Plot all palettes

if(substr(action, 1, 1) == "s" & palette == "all") {

  par(mar = c(1, 4, 4, 0))

  n.palettes <- length(palette.names)

  plot(1, xlim = c(0, 12), ylim = c(0, 1), xaxt = "n", yaxt = "n",
       bty = "n", type = "n", xlab = "", ylab = "",
       main = "pirate palettes"
       )


  mtext(paste("trans = ", trans, sep = ""), 3)


    y.locations <- seq(0, 1, length.out = n.palettes)

  for(i in 1:n.palettes) {

    palette.df <- unlist(get(paste(palette.names[i], ".pal", sep = "")))

    n.colors <- length(palette.df)

  #  rect(1:n.colors - .5, i - .25, 1:n.colors + .5, i + .25, col = palette.df, border = NA)

  points(1:n.colors, rep(y.locations[i], n.colors) * 1, col = palette.df, pch = 16, cex = 1.4)

    mtext(palette.names[i], side = 2, at = y.locations[i], las = 1, cex = .9)

  }

}





}

