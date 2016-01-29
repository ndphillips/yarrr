#' recodev
#'
#' This function takes a vector original.vector, and converts all values in a vector old.values to the values in a new vector new.values.
#'
#' @param original.vector A vector you want to recode
#' @param old.values A vector of length M.
#' @param new.values A vector of length M.
#' @param others An optional value indicating what to convert all values in original.vector that are not found in old.values.
#' @export
#' @examples
#'
#' x <- c("y", "y", "XSF", "y", "0", "X", "0", "0", "y", "n", "0", "1", "1")
#' recodev(original.vector = x,
#'          old.values = c("y", "1", "n", "0"),
#'          new.values = c(1, 1, 0, 0)
#' )
#'
#'
#' x <- c("y", "y", "XSF", "y", "0", "X", "0", "0", "y", "n", "0", "1", "1")
#' recodev(original.vector = x,
#'          old.values = c("y", "1", "n", "0"),
#'          new.values = c(1, 1, 0, 0),
#'          others = NA
#' )
#'
#'



recodev <- function(original.vector,
                     old.values,
                     new.values,
                     others = NULL) {

  if(is.null(others)) {

    new.vector <- original.vector

  }

  if(is.null(others) == F) {

    new.vector <- rep(others,
                      length(original.vector))

  }

  for (i in 1:length(old.values)) {

    change.log <- original.vector == old.values[i] &
      is.na(original.vector) == F

    new.vector[change.log] <- new.values[i]

  }

  return(new.vector)

}
