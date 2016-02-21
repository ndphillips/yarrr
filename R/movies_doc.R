#' movies
#'
#' A dataset containing the 5,000 top grossing movies (of all time?)
#'
#'
#' @format A data frame containing 5,000 rows and 14 columns
#' \describe{
#'   \item{name}{movie name}
#'   \item{revenue.all}{Total box office revenue (in millions of USD)}
#'   \item{revenue.int}{International box office revenue (in millions of USD)}
#'   \item{revenue.usa}{Box office revenue in the USA (in millions of USD)}
#'   \item{revenue.usa.infadj}{Box office revenue in the USA after being adjusted for inflation (in millions of USD). Because inflation depreciates the value of a dollar over time, this means that the older a movie is, the higher its inflation adjusted revenue will be.}
#'   \item{dvd.usa}{DVD revenue in the USA (in millions of USD)}
#'   \item{budget}{Production budget (in millions of USD)}
#'   \item{rating}{MPAA (Motion Picture Association of America) rating. G = General Audiences. PG = Parents cautioned. PG-13 = Parents strongly cautioned for children under 13. R = Children under 18 must be accompanied by a parent. NC-17 = No children under 18 allowed.}
#'   \item{genre}{Movie genre}
#'   \item{creative.type}{Type of movie}
#'   \item{time}{Running time (in minutes)}
#'   \item{year}{Release year}
#'   \item{sequel}{Was the movie a sequel?}
#'   \item{boxoffice.domestic.inflationadj}{Inflation adjusted domestic box office revenue (in millions of USD)}
#' }
#' @source www.the-numbers.com
#'
#'
#'
"movies"
