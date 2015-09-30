#' transparent function
#'
#' This function takes a standard color as an argument and returns a
#' transparent version of that color
#'
#' @param orig.col The original color to be made transparent. Can be specified as a string or a vector of rgb values
#' @param trans.val A number in the interval [0, 1] indicating how transparent to make the color.
#' @param maxColorValue The maximum color value (only used when orig.col is an rgb vector)
#' @keywords colors
#' @export
#' @examples
#' transparent()



transparent <- function(orig.col = "red", trans.val = 1, maxColorValue = 255) {

  if(length(orig.col) == 1) {orig.col <- col2rgb(orig.col)}
  if(!(length(orig.col) %in% c(1, 3))) {return(paste("length of original color must be 1 or 3!"))}

  final.col <- rgb(orig.col[1], orig.col[2], orig.col[3], alpha = trans.val * 255, maxColorValue = maxColorValue)

  return(final.col)
}
