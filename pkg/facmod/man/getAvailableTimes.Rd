\name{getAvailableTimes}
\alias{getAvailableTimes}
\title{
getAvailableTimes
}

\description{
get dates for which factor data is availible
}

\usage{
getAvailableTimes(object)
}
\arguments{
  \item{object}{ an "fmdsCube" object}
}

\value{
a character vector of available dates
}

\author{
Guy Yollin, \url{http://www.r-programming.org} \cr
(based on S-PLUS code developed by Christopher G. Green, University of Washington)
}

\examples{
data(fmdata)
getAvailableTimes(fmdata)
}

