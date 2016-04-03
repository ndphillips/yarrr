#' pircharter
#'
#' A dataframe containing travel times of chartered ships from three pirate companies to three different destinations.
#'
#'
#' @format A data frame containing 1000 rows and 10 columns
#' \describe{
#'   \item{company}{(string) - The charter company: JoRo = Jolly Roger, BmcB = Boaty McBoat, MiPa = Millenium Parrot}
#'   \item{destination}{(string) - The destination of the charter}
#'   \item{time}{(numeric) - The travel time of the ship in hours}
#'  }
#' @source 2015 annual international pirate meeting at the Bodensee in Konstanz, Germany
#' @examples
#'
#' pirateplot(formula = time ~ company
#'            data = pircharter,
#'            )
#'
#'
#'
"pircharter"
