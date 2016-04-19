#' auction
#'
#' A dataframe containing data from 1000 ships sold at a pirate auction.
#'
#'
#' @format A data frame containing 1000 rows and 8 columns
#' \describe{
#'   \item{cannons}{(integer) The number of cannons on the ship}
#'   \item{rooms}{(integer) The number of rooms on the ship}
#'   \item{age}{(numeric) The age of the ship in years}
#'   \item{condition}{(integer) The condition of the ship on a scale of 1 to 10}
#'   \item{color}{(string) The color of the ship}
#'   \item{style}{(string) The style of the ship - either modern or classic}
#'   \item{jbb}{(numeric) The pre-sale predicted value of the ship according to Jack's Blue Book (JBB)}
#'   \item{price}{(numeric) The actual selling price of the ship (in gold pieces, obviously)}
#' }
#' @source 2015 annual pirate auction in Portland Oregon
#' @examples
#'
#' pirateplot(dv.name = "price",
#' iv.name = "condition",
#' data = auction
#' )
#'
#'
"auction"
