#' transparent function
#'
#' This function takes a standard color as an argument and returns a
#' transparent version of that color
#'
#' @param orig.col The original color to be made transparent. Can be specified as a string or a vector of rgb values
#' @param trans.val A number in the interval [0, 1] indicating how transparent to make the color.
#' @param maxColorValue The maximum color value (only used when orig.col is an rgb vector)
#' @keywords colors
#' @examples
#' # Diagram of some examples
#'plot(1, ylim = c(0, 1), xlim = c(0, 12), bty = "n", xaxt = "n", yaxt = "n", ylab = "", xlab = "", type = "na")
#'
#'
#'text(6, .9, "transparent('red', trans.val = x)")
#'points(x = 1:11, y = rep(.8, 11), pch = 16, col = transparent("red", seq(0, 1, .1)), cex = 2)
#'text(x = 1:11, y = rep(.85, 11), seq(0, 1, .1))
#'
#'text(6, .7, "transparent('red', trans.val = x)")
#'points(x = 1:11, y = rep(.6, 11), pch = 16, col = transparent("blue", seq(0, 1, .1)), cex = 2)
#'text(x = 1:11, y = rep(.65, 11), seq(0, 1, .1))
#'
#'text(6, .5, "transparent('forestgreen', trans.val = x)")
#'points(x = 1:11, y = rep(.4, 11), pch = 16, col = transparent("forestgreen", seq(0, 1, .1)), cex = 2)
#'text(x = 1:11, y = rep(.45, 11), seq(0, 1, .1))
#'
#'text(6, .3, "transparent('orchid1', trans.val = x)")
#'points(x = 1:11, y = rep(.2, 11), pch = 16, col = transparent("orchid1", seq(0, 1, .1)), cex = 2)
#'text(x = 1:11, y = rep(.25, 11), seq(0, 1, .1))
#'
#'
#'# Scatterplot with transparent colors
#'
#'a.x <- rnorm(100, mean = 0, sd = 1)
#'a.y <- a.x + rnorm(100, mean = 0, sd = 1)
#'
#'par(mfrow = c(3, 3))
#'
#'for(trans.val.i in seq(0, .1, length.out = 9)) {
#'
#'  plot(a.x, a.y, pch = 16, col = transparent("blue", trans.val.i), cex = 1.5,
#'       xlim = c(-5, 5), ylim = c(-5, 5), xlab = "x", ylab = "y",
#'       main = paste("trans.val = ", round(trans.val.i, 2), sep = ""))
#'
#'}




transparent <- function(orig.col = "red", trans.val = 1, maxColorValue = 255) {

  if(length(orig.col) == 1) {orig.col <- col2rgb(orig.col)}
  if(!(length(orig.col) %in% c(1, 3))) {return(paste("length of original color must be 1 or 3!"))}

  final.col <- rgb(orig.col[1], orig.col[2], orig.col[3], alpha = (1 - trans.val) * 255, maxColorValue = maxColorValue)

  return(final.col)
}
