#' piratepal
#'
#' This function provides a number of color palettes
#'
#' @param palette A string defining the color palette to use (see examples). To use a random palette, use "random". To plot all palettes, use "all". To see all palette names, use "names"
#' @param plot.result A logical value indicating whether or not to display the colors.
#' @param trans A number in the interval [0, 1] indicating how transparent to make the colors. A value of 0 means no transparency and a value of 1 means completely transparency.
#' @param length.out An integer indicating how many colors to return. If length.out is larger than the number of colors in the palette, colors will be repeated.
#' @keywords colors
#' @export
#' @importFrom jpeg readJPEG
#' @examples
#'
#'
#' # Check out the vignette for a full guide
#'
#' vignette("piratepal", package = "yarrr")
#'
#' # Show all palettes
#'
#' piratepal(palette = "all")
#'
#' # Show some palettes
#'
#'piratepal(palette = "basel", trans = .5, plot.result = TRUE)
#'
#'
#' # Using a palette in a scatterplot
#'
#'nemo.cols <- piratepal(palette = "nemo",  trans = .5)
#'
#'set.seed(100) # For reproducibility
#'x <- rnorm(100)
#'y <- x + rnorm(100)
#'
#'plot(x = x, y = y, col = nemo.cols,
#'     pch = 16,
#'     cex = runif(100, min = 0, max = 2),
#'     main = "piratepal('nemo', trans = .5)")
#'
#'

piratepal <- function(palette = "all",
                      trans = 0,
                      plot.result = FALSE,
                      length.out = NULL) {

# Check inputs
if(trans < 0 | trans > 1) {stop("Problem: trans must be a number between 0 and 1")}

# Define all palettes
  {

    piratepal.ls <- list(

      "basel.pal" =  data.frame(
        "blue1" = rgb(12, 91, 176, alpha = (1 - trans) * 255, maxColorValue = 255),
        "red" = rgb(238, 0, 17, alpha = (1- trans) * 255, maxColorValue = 255),
        "green" = rgb(21, 152, 61, alpha = (1 - trans) * 255, maxColorValue = 255),
        "pink" = rgb(236, 87, 154, alpha = (1 - trans) * 255, maxColorValue = 255),
        "orange" = rgb(250, 107, 9, alpha = (1 - trans) * 255, maxColorValue = 255),
        "blue2" = rgb(20, 155, 237, alpha = (1 - trans) * 255, maxColorValue = 255),
        "green2" = rgb(161, 199, 32, alpha = (1 - trans) * 255, maxColorValue = 255),
        "yellow" = rgb(254, 193, 11, alpha = (1 - trans) * 255, maxColorValue = 255),
        "turquoise" = rgb(22, 160, 140, alpha = (1 - trans) * 255, maxColorValue = 255),
        "poop" = rgb(154, 112, 62, alpha = (1 - trans) * 255, maxColorValue = 255),
        stringsAsFactors = F),
      "pony.pal" = data.frame(
        "pink" = rgb(235, 82, 145, alpha = (1 - trans) * 255, maxColorValue = 255),
        "orange" = rgb(251, 187, 104, alpha = (1 - trans) * 255, maxColorValue = 255),
        "lpink" = rgb(245, 186, 207, alpha = (1 - trans) * 255, maxColorValue = 255),
        "lblue" = rgb(157, 218, 245, alpha = (1 - trans) * 255, maxColorValue = 255),
        "purple1" = rgb(99, 81, 160, alpha = (1- trans) * 255, maxColorValue = 255),
        "gray" = rgb(236, 241, 244, alpha = (1 - trans) * 255, maxColorValue = 255),
        "yellow" = rgb(254, 247, 158, alpha = (1 - trans) * 255, maxColorValue = 255),
        "dblue" = rgb(23, 148, 206, alpha = (1 - trans) * 255, maxColorValue = 255),
        "purple2" = rgb(151, 44, 141, alpha = (1- trans) * 255, maxColorValue = 255),
        stringsAsFactors = F),
      "southpark.pal" = data.frame(
        "blue" = rgb(47, 134, 255, alpha = (1 - trans) * 255, maxColorValue = 255),
        "yellow" = rgb(235, 171, 22, alpha = (1- trans) * 255, maxColorValue = 255),
        "red" = rgb(222, 0, 18, alpha = (1 - trans) * 255, maxColorValue = 255),
        "green" = rgb(34, 196, 8, alpha = (1 - trans) * 255, maxColorValue = 255),
        "tan" = rgb(254, 205, 170, alpha = (1 - trans) * 255, maxColorValue = 255),
        "orange" = rgb(241, 72, 9, alpha = (1 - trans) * 255, maxColorValue = 255),
        stringsAsFactors = F),

      "eternal.pal" = data.frame(
        "purple1" = rgb(23, 12, 46, alpha = (1 - trans) * 255, maxColorValue = 255),
        "red" = rgb(117, 16, 41, alpha = (1 - trans) * 255, maxColorValue = 255),
        "purple2" = rgb(82, 25, 76, alpha = (1 - trans) * 255, maxColorValue = 255),
        "purple3" = rgb(71, 59, 117, alpha = (1 - trans) * 255, maxColorValue = 255),
        "blue1" = rgb(77, 112, 156, alpha = (1 - trans) * 255, maxColorValue = 255),
        "tan" = rgb(111, 118, 107, alpha = (1 - trans) * 255, maxColorValue = 255),
        "blue2" = rgb(146, 173, 196, alpha = (1 - trans) * 255, maxColorValue = 255),
        stringsAsFactors = F),

      "evildead.pal" =  data.frame(
        "brown" = rgb(25, 24, 13, alpha = (1 - trans) * 255, maxColorValue = 255),
        "green" = rgb(33, 37, 16, alpha = (1 - trans) * 255, maxColorValue = 255),
        "red" = rgb(46, 16, 11, alpha = (1 - trans) * 255, maxColorValue = 255),
        "brown2" = rgb(57, 46, 18, alpha = (1 - trans) * 255, maxColorValue = 255),
        "brown3" = rgb(87, 81, 43, alpha = (1 - trans) * 255, maxColorValue = 255),
        "tan" = rgb(150, 142, 76, alpha = (1 - trans) * 255, maxColorValue = 255),
        stringsAsFactors = F),

      "monalisa.pal" = data.frame(
        "tan" = rgb(187, 163, 112, alpha = (1 - trans) * 255, maxColorValue = 255),
        "yellow" = rgb(202, 162, 65, alpha = (1 - trans) * 255, maxColorValue = 255),
        "green1" = rgb(187, 190, 112, alpha = (1 - trans) * 255, maxColorValue = 255),
        "green2" = rgb(89, 89, 41, alpha = (1 - trans) * 255, maxColorValue = 255),
        "brown" = rgb(42, 20, 1, alpha = (1 - trans) * 255, maxColorValue = 255),
        stringsAsFactors = F),

      malcovich.pal =data.frame(
        "gray1" = rgb(5, 5, 5, alpha = (1 - trans) * 255, maxColorValue = 255),
        "blue1" = rgb(20, 24, 27, alpha = (1 - trans) * 255, maxColorValue = 255),
        "green" = rgb(46, 77, 73, alpha = (1 - trans) * 255, maxColorValue = 255),
        "brown" = rgb(77, 64, 57, alpha = (1 - trans) * 255, maxColorValue = 255),
        "blue2" = rgb(48, 92, 110, alpha = (1 - trans) * 255, maxColorValue = 255),
        "blue3" = rgb(117, 125, 139, alpha = (1 - trans) * 255, maxColorValue = 255),
        "gray" = rgb(164, 160, 159, alpha = (1 - trans) * 255, maxColorValue = 255),

        stringsAsFactors = F),

      toystory.pal =data.frame(
        "gray1" = rgb(15, 10, 10, alpha = (1 - trans) * 255, maxColorValue = 255),
        "blue1" = rgb(36, 24, 40, alpha = (1 - trans) * 255, maxColorValue = 255),
        "red" = rgb(251, 27, 34, alpha = (1 - trans) * 255, maxColorValue = 255),
        "brown" = rgb(94, 72, 57, alpha = (1 - trans) * 255, maxColorValue = 255),
        "green" = rgb(125, 153, 58, alpha = (1 - trans) * 255, maxColorValue = 255),
        "yellow" = rgb(227, 191, 71, alpha = (1 - trans) * 255, maxColorValue = 255),
        "blue2" = rgb(167, 208, 235, alpha = (1 - trans) * 255, maxColorValue = 255),

        stringsAsFactors = F),

      usualsuspects.pal =data.frame(
        "gray1" = rgb(50, 51, 55, alpha = (1 - trans) * 255, maxColorValue = 255),
        "gray2" = rgb(83, 76, 83, alpha = (1 - trans) * 255, maxColorValue = 255),
        "blue" = rgb(63, 81, 106, alpha = (1 - trans) * 255, maxColorValue = 255),
        "brown" = rgb(155, 102, 89, alpha = (1 - trans) * 255, maxColorValue = 255),
        "red" = rgb(232, 59, 65, alpha = (1 - trans) * 255, maxColorValue = 255),
        "gray3" = rgb(159, 156, 162, alpha = (1 - trans) * 255, maxColorValue = 255),
        "tan" = rgb(234, 174, 157, alpha = (1 - trans) * 255, maxColorValue = 255),

        stringsAsFactors = F),


      ohbrother.pal =data.frame(
        "brown1" = rgb(26, 15, 10, alpha = (1 - trans) * 255, maxColorValue = 255),
        "brown2" = rgb(61, 41, 26, alpha = (1 - trans) * 255, maxColorValue = 255),
        "brown3" = rgb(113, 86, 57, alpha = (1 - trans) * 255, maxColorValue = 255),
        "green" = rgb(116, 125, 109, alpha = (1 - trans) * 255, maxColorValue = 255),
        "tan1" = rgb(173, 157, 11, alpha = (1 - trans) * 255, maxColorValue = 255),
        "blue" = rgb(148, 196, 223, alpha = (1 - trans) * 255, maxColorValue = 255),
        "tan2" = rgb(230, 221, 168, alpha = (1 - trans) * 255, maxColorValue = 255),

        stringsAsFactors = F),


      appletv.pal =data.frame(
        "green" = rgb(95, 178, 51, alpha = (1 - trans) * 255, maxColorValue = 255),
        "gray" = rgb(106, 127, 147, alpha = (1 - trans) * 255, maxColorValue = 255),
        "orange" = rgb(245, 114, 6, alpha = (1 - trans) * 255, maxColorValue = 255),
        "red" = rgb(235, 15, 19, alpha = (1 - trans) * 255, maxColorValue = 255),
        "purple" = rgb(143, 47, 139, alpha = (1 - trans) * 255, maxColorValue = 255),
        "blue" = rgb(19, 150, 219, alpha = (1 - trans) * 255, maxColorValue = 255),
        stringsAsFactors = F
      ),


      # http://a.dilcdn.com/bl/wp-content/uploads/sites/2/2015/05/disneyPixar_PixarPalette_BRA_01.jpg

      brave.pal =data.frame(
        "brown" = rgb(168, 100, 59, alpha = (1 - trans) * 255, maxColorValue = 255),
        "yellow" = rgb(182, 91, 35, alpha = (1 - trans) * 255, maxColorValue = 255),
        "red" = rgb(148, 34, 14, alpha = (1 - trans) * 255, maxColorValue = 255),
        "green" = rgb(39, 45, 23, alpha = (1 - trans) * 255, maxColorValue = 255),
        "blue" = rgb(32, 33, 38, alpha = (1 - trans) * 255, maxColorValue = 255),
        stringsAsFactors = F),

      # http://a.dilcdn.com/bl/wp-content/uploads/sites/2/2015/05/disneyPixar_PixarPalette_BUG_01.jpg

      bugs.pal =data.frame(
        "green1" = rgb(102, 120, 64, alpha = (1 - trans) * 255, maxColorValue = 255),
        "green2" = rgb(186, 214, 168, alpha = (1 - trans) * 255, maxColorValue = 255),
        "blue" = rgb(133, 199, 193, alpha = (1 - trans) * 255, maxColorValue = 255),
        "brown1" = rgb(165, 154, 107, alpha = (1 - trans) * 255, maxColorValue = 255),
        "brown2" = rgb(103, 85, 63, alpha = (1 - trans) * 255, maxColorValue = 255),
        stringsAsFactors = F),

      # http://a.dilcdn.com/bl/wp-content/uploads/sites/2/2015/05/disneyPixar_PixarPalette_CAR_01.jpg


      cars.pal =data.frame(
        "peach" = rgb(231, 176, 143, alpha = (1 - trans) * 255, maxColorValue = 255),
        "purple" = rgb(136, 76, 73, alpha = (1 - trans) * 255, maxColorValue = 255),
        "red" = rgb(224, 54, 58, alpha = (1 - trans) * 255, maxColorValue = 255),
        "brown" = rgb(106, 29, 26, alpha = (1 - trans) * 255, maxColorValue = 255),
        "blue" = rgb(157, 218, 230, alpha = (1 - trans) * 255, maxColorValue = 255),
        stringsAsFactors = F),


      # http://a.dilcdn.com/bl/wp-content/uploads/sites/2/2015/05/disneyPixar_PixarPalette_FIN_021.jpg


      nemo.pal =data.frame(
        "yellow" = rgb(251, 207, 53, alpha = (1 - trans) * 255, maxColorValue = 255),
        "orange" = rgb(237, 76, 28, alpha = (1 - trans) * 255, maxColorValue = 255),
        "brown" = rgb(156, 126, 112, alpha = (1 - trans) * 255, maxColorValue = 255),
        "blue1" = rgb(90, 194, 241, alpha = (1 - trans) * 255, maxColorValue = 255),
        "green" = rgb(17, 119, 108, alpha = (1 - trans) * 255, maxColorValue = 255),
        stringsAsFactors = F),

      # http://a.dilcdn.com/bl/wp-content/uploads/sites/2/2015/05/disneyPixar_PixarPalette_RAT_02.jpg

      rat.pal =data.frame(
        "brown" = rgb(159, 77, 35, alpha = (1 - trans) * 255, maxColorValue = 255),
        "purple" = rgb(146, 43, 73, alpha = (1 - trans) * 255, maxColorValue = 255),
        "red" = rgb(178, 29, 19, alpha = (1 - trans) * 255, maxColorValue = 255),
        "green" = rgb(127, 134, 36, alpha = (1 - trans) * 255, maxColorValue = 255),
        "yellow" = rgb(241, 156, 31, alpha = (1 - trans) * 255, maxColorValue = 255),
        stringsAsFactors = F),

      # http://a.dilcdn.com/bl/wp-content/uploads/sites/2/2015/05/disneyPixar_PixarPalette_UP_02.jpg

      up.pal =data.frame(
        "blue1" = rgb(95, 140, 244, alpha = (1 - trans) * 255, maxColorValue = 255),
        "blue2" = rgb(220, 214, 252, alpha = (1 - trans) * 255, maxColorValue = 255),
        "orange" = rgb(226, 122, 72, alpha = (1 - trans) * 255, maxColorValue = 255),
        "brown" = rgb(96, 86, 70, alpha = (1 - trans) * 255, maxColorValue = 255),
        "blue" = rgb(67, 65, 89, alpha = (1 - trans) * 255, maxColorValue = 255),
        stringsAsFactors = F),



      # Taken from a cellphone photo of espresso cups in the ARC kitchen

      espresso.pal=data.frame(
        "purple" = rgb(39, 27, 48, alpha = (1 - trans) * 255, maxColorValue = 255),
        "blue" = rgb(35, 102, 192, alpha = (1 - trans) * 255, maxColorValue = 255),
        "yellow" = rgb(233, 215, 56, alpha = (1 - trans) * 255, maxColorValue = 255),
        "red" = rgb(185, 18, 38, alpha = (1 - trans) * 255, maxColorValue = 255),
        "green" = rgb(163, 218, 75, alpha = (1 - trans) * 255, maxColorValue = 255),
        "orange" = rgb(255, 100, 53, alpha = (1 - trans) * 255, maxColorValue = 255),
        stringsAsFactors = F),

      # Colors of apple ipods (can't remember which year)

      ipod.pal  =data.frame(
        "lightgray" = rgb(215, 215, 215, alpha = (1 - trans) * 255, maxColorValue = 255),
        "red" = rgb(243, 174, 175, alpha = (1 - trans) * 255, maxColorValue = 255),
        "darkgray" = rgb(174, 173, 176, alpha = (1 - trans) * 255, maxColorValue = 255),
        "green" = rgb(158, 217, 191, alpha = (1 - trans) * 255, maxColorValue = 255),
        "blue" = rgb(92, 203, 235, alpha = (1 - trans) * 255, maxColorValue = 255),
        "yellow" = rgb(222, 235, 97, alpha = (1 - trans) * 255, maxColorValue = 255),
        "background" = rgb(242, 242, 242, alpha = (1 - trans) * 255, maxColorValue = 255),
        stringsAsFactors = F),

      # Colors from an infographic (can't remember which one)

      info.pal =data.frame(
        "red" = rgb(231, 105, 93, alpha = (1 - trans) * 255, maxColorValue = 255),
        "darkblue" = rgb(107, 137, 147, alpha = (1 - trans) * 255, maxColorValue = 255),
        "creme" = rgb(246, 240, 212, alpha = (1 - trans) * 255, maxColorValue = 255),
        "green" = rgb(149, 206, 138, alpha = (1 - trans) * 255, maxColorValue = 255),
        "gray1" = rgb(210, 210, 210, alpha = (1 - trans) * 255, maxColorValue = 255),
        "lightblue" = rgb(148, 212, 212, alpha = (1 - trans) * 255, maxColorValue = 255),
        "gray2" = rgb(150, 150, 150, alpha = (1 - trans) * 255, maxColorValue = 255),
        "background" = rgb(241, 243, 232, alpha = (1 - trans) * 255, maxColorValue = 255),
        "brown" = rgb(136, 119, 95, alpha = (1 - trans) * 255, maxColorValue = 255),
        stringsAsFactors = F),

      # Colors from another mystery infographic

      info2.pal =data.frame(
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
        stringsAsFactors = F),



      # http://www.google.com

      google.pal =data.frame(
        "blue" = rgb(61, 121, 243, alpha = (1 - trans) * 255, maxColorValue = 255),
        "red" = rgb(230, 53, 47, alpha = (1 - trans) * 255, maxColorValue = 255),
        "yellow" = rgb(249, 185, 10, alpha = (1 - trans) * 255, maxColorValue = 255),
        "green" = rgb(52, 167, 75, alpha = (1 - trans) * 255, maxColorValue = 255),
        stringsAsFactors = F),




      # http://www.informationisbeautiful.net/visualizations/drugs-world/

      drugs.pal =data.frame(
        "blue" = rgb(170, 220, 226, alpha = (1 - trans) * 255, maxColorValue = 255),
        "green" = rgb(162, 206, 37, alpha = (1 - trans) * 255, maxColorValue = 255),
        "yellow" = rgb(244, 238, 43, alpha = (1 - trans) * 255, maxColorValue = 255),
        "orange" = rgb(207, 79, 26, alpha = (1 - trans) * 255, maxColorValue = 255),
        "purple" = rgb(217, 82, 156, alpha = (1 - trans) * 255, maxColorValue = 255),
        "gray" = rgb(225, 224, 224, alpha = (1 - trans) * 255, maxColorValue = 255),
        stringsAsFactors = F),




      # http://www.colourlovers.com/palette/92095/Giant_Goldfish

      goldfish.pal =data.frame(
        "blue" = rgb(105, 210, 231, alpha = (1 - trans) * 255, maxColorValue = 255),
        "green" = rgb(167, 219, 216, alpha = (1 - trans) * 255, maxColorValue = 255),
        "gray" = rgb(224, 228, 204, alpha = (1 - trans) * 255, maxColorValue = 255),
        "orange1" = rgb(243, 134, 48, alpha = (1 - trans) * 255, maxColorValue = 255),
        "orange2" = rgb(250, 105, 0, alpha = (1 - trans) * 255, maxColorValue = 255),
        stringsAsFactors = F),


      # http://www.colourlovers.com/palette/694737/Thought_Provoking

      provoking.pal =data.frame(
        "yellow" = rgb(236, 208, 120, alpha = (1 - trans) * 255, maxColorValue = 255),
        "orange" = rgb(217, 91, 67, alpha = (1 - trans) * 255, maxColorValue = 255),
        "red" = rgb(192, 41, 66, alpha = (1 - trans) * 255, maxColorValue = 255),
        "brown" = rgb(84, 36, 55, alpha = (1 - trans) * 255, maxColorValue = 255),
        "blue" = rgb(83, 119, 122, alpha = (1 - trans) * 255, maxColorValue = 255),
        stringsAsFactors = F),


      # http://www.colourlovers.com/palette/1930/cheer_up_emo_kid

      emo.pal =data.frame(
        "grayblue" = rgb(85, 98, 112, alpha = (1 - trans) * 255, maxColorValue = 255),
        "blue" = rgb(78, 205, 196, alpha = (1 - trans) * 255, maxColorValue = 255),
        "green" = rgb(199, 244, 100, alpha = (1 - trans) * 255, maxColorValue = 255),
        "salmon" = rgb(255, 107, 107, alpha = (1 - trans) * 255, maxColorValue = 255),
        "magenta" = rgb(196, 77, 88, alpha = (1 - trans) * 255, maxColorValue = 255),
        stringsAsFactors = F),



      # http://www.colourlovers.com/palette/49963/let_them_eat_cake

      cake.pal =data.frame(
        "brown" = rgb(119, 79, 56, alpha = (1 - trans) * 255, maxColorValue = 255),
        "red" = rgb(224, 142, 121, alpha = (1 - trans) * 255, maxColorValue = 255),
        "creme1" = rgb(241, 212, 175, alpha = (1 - trans) * 255, maxColorValue = 255),
        "creme2" = rgb(236, 229, 206, alpha = (1 - trans) * 255, maxColorValue = 255),
        "blue" = rgb(197, 224, 220, alpha = (1 - trans) * 255, maxColorValue = 255),
        stringsAsFactors = F),



      # http://www.colourlovers.com/palette/443995/i_demand_a_pancake

      pancake.pal=data.frame(
        "brown" = rgb(89, 79, 79, alpha = (1 - trans) * 255, maxColorValue = 255),
        "blue1" = rgb(84, 121, 128, alpha = (1 - trans) * 255, maxColorValue = 255),
        "blue2" = rgb(69, 173, 168, alpha = (1 - trans) * 255, maxColorValue = 255),
        "green" = rgb(157, 224, 173, alpha = (1 - trans) * 255, maxColorValue = 255),
        "yellow" = rgb(229, 252, 194, alpha = (1 - trans) * 255, maxColorValue = 255),
        stringsAsFactors = F),



      #   http://www.colourlovers.com/fashion/trends/street-fashion/7759/LUBITEL

      lubitel.pal =data.frame(
        "blue" = rgb(55, 58, 69, alpha = (1 - trans) * 255, maxColorValue = 255),
        "gray" = rgb(194, 194, 194, alpha = (1 - trans) * 255, maxColorValue = 255),
        "brown1" = rgb(179, 152, 109, alpha = (1 - trans) * 255, maxColorValue = 255),
        "red" = rgb(120, 36, 29, alpha = (1 - trans) * 255, maxColorValue = 255),
        "brown2" = rgb(94, 62, 53, alpha = (1 - trans) * 255, maxColorValue = 255),
        stringsAsFactors = F)


    )


  }

palette.names <- unlist(strsplit(names(piratepal.ls), ".pal", T))
n.palettes <- length(palette.names)

if(!(palette %in% c(palette.names, "random", "all", "names"))) {

  stop(c("You did not specify a valid palette. Please try one of the following: ", palette.names))

}


# if palette == "all", show all palettes
if(palette == "all") {

  output <- NULL

    par(mar = c(1, 6, 4, 0))

    n.palettes <- length(palette.names)

    plot(1, xlim = c(0, 15), ylim = c(0, 1), xaxt = "n", yaxt = "n",
         bty = "n", type = "n", xlab = "", ylab = "",
         main = "Here are all of the pirate palettes")


mtext(text = paste("Transparency is set to ", trans, sep = ""), side = 3)

    y.locations <- seq(1, 0, length.out = n.palettes)

    for(i in 1:n.palettes) {

      palette.df <- unlist(piratepal.ls[[paste(palette.names[i], ".pal", sep = "")]])

      n.colors <- length(palette.df)

      rect(0:(n.colors - 1),  rep(y.locations[i], n.colors) - 1 / (n.palettes * 2.2), 1:(n.colors), rep(y.locations[i], n.colors) + 1 / (n.palettes * 2.2), col = palette.df, border = NA)

      # points(1:n.colors, rep(y.locations[i], n.colors) * 1, col = palette.df, pch = 16, cex = 1.4)

      mtext(unlist(strsplit(palette.names[i], fixed = T, split = "."))[1],
            side = 2, at = y.locations[i], las = 1, cex = .9, line = 0)

    }

  }

if(palette == "random") {

  palette <- sample(palette.names[palette.names != "random"], 1)
  palette <- unlist(strsplit(palette, ".", fixed = T))[1]

  message(paste("Here's the", palette, "palette"))

  }

if(palette == "names") {

  output <- palette.names

}

# Get result vector
if(palette %in% c("all", "random", "names") == F) {

  palette.df <- piratepal.ls[[paste(palette, ".pal", sep = "")]]

  if(is.null(length.out)) {output <- unlist(palette.df)}
  if(is.null(length.out) == F) {output <- rep(unlist(palette.df), length.out = length.out)}

}

# Plot single palette
if(plot.result & palette %in% palette.names) {

  palette.df <- piratepal.ls[[paste(palette, ".pal", sep = "")]]
  col.vec <- unlist(palette.df)
  n.colors <- length(col.vec)

  par(mar = c(1, 1, 1, 1))
  plot(1, xlim = c(0, 1), ylim = c(0, 1),
       type='n',xaxs='i',xaxt = "n", yaxt = "n",
       bty = "n", yaxs='i',xlab='',ylab='')



  # is there a picture?

  if(system.file(paste(palette, ".jpg", sep = ""), package = "yarrr") != "") {

  point.heights <- .3
  text.heights <- .05
  pic.center <- c(.5, .65)

  jpg <-  jpeg::readJPEG(system.file(paste(palette, ".jpg", sep = ""), package="yarrr"), native=T) # read the file
  res <-  dim(jpg)[1:2] # get the resolution
  ar <- res[2] / res[1]

  if(res[2] >= res[1]) {

  desired.width <- .6
  required.height <- desired.width / ar

  rasterImage(jpg,
              pic.center[1] - desired.width / 2,
              pic.center[2] - required.height / 2,
              pic.center[1] + desired.width / 2,
              pic.center[2] + required.height / 2)

  }

  if(res[2] < res[1]) {

    desired.height <- .40
    required.width <- desired.height * ar

    rasterImage(jpg,
                pic.center[1] - required.width / 2,
                pic.center[2] - desired.height / 2,
                pic.center[1] + required.width / 2,
                pic.center[2] + desired.height / 2)

  }


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

segments(locations.to.use, text.heights + .05, locations.to.use, point.heights, lwd = 1, lty = 2)


  # Add points

  points(x = locations.to.use, y = rep(point.heights, length(col.vec)),
         pch = 16, col = col.vec, cex = 10)

  text(locations.to.use, text.heights, names(col.vec), srt = 45)

  text(.5, .95, palette, cex = 2)
  text(.5, .9, paste("trans = ", trans, sep = ""))


  # Reset margins

  par(mar = c(5, 4, 4, 1) + .1)

}

if(is.null(output) == F & plot.result == F) {
return(output)
}

}

