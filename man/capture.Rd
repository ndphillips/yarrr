% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/capture_doc.R
\docType{data}
\name{capture}
\alias{capture}
\title{capture}
\format{
A data frame containing 1000 rows and 10 columns
\describe{
  \item{size}{(integer) - The size (length) of the ship (maybe in meters?)}
  \item{cannons}{(integer) - The number of cannons on the ship}
  \item{style}{(string) - The style of the ship (either modern or classic)}
  \item{warnshot}{(binary) - Did the ship fire a warning shot at the Perilous Pigeon when it got close?}
  \item{date}{(integer) - The date of the capture (1 = January 1, 365 = December 31)}
  \item{heardof}{(binary) - Was the target ship recognized by the captain's first mate?}
  \item{decorations}{(integer) - An integer between 1 and 10 indicating how decorated the ship was. 1 means it looks totally common and shabby, 10 means it is among the finest looking ship you've ever seen!}
  \item{daysfromshore}{(integer) - How many days from the nearest land was the ship when it was found?}
  \item{speed}{(integer) - How fast was the ship going when it was caught?}
  \item{treasure}{(numeric) - How much treasure was found on the ship when it was captured?}
 }
}
\source{
2015 annual international pirate meeting at the Bodensee in Konstanz, Germany
}
\usage{
capture
}
\description{
A dataframe containing a historical record of every ship the Perilous Pigeon captured on the Bodensee in the years 2014 and 2015
}
\keyword{datasets}
