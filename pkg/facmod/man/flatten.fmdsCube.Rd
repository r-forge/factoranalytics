\name{flatten.fmdsCube}
\alias{flatten.fmdsCube}

\title{
flatten.fmdsCube
}

\description{
Converts an fmdsCube object to a data frame by flattening its data slot and expanding its info slot
}

\usage{
flatten.fmdsCube(cube, along = "asset")
}
\arguments{
  \item{cube}{ an "fmdsCube" object}
  \item{along}{direction along which to flatten. If along equals "asset" (the default), 
  the data will be stacked by asset (i.e., assets will be grouped together). If along equals "date", 
  the data will be stacked by date (i.e., dates will be grouped together)}
}

\value{
a data frame containing all the data in the data and info slots of cube
}

\author{
Guy Yollin, \url{http://www.r-programming.org} \cr
(based on S-PLUS code developed by Christopher G. Green, University of Washington)
}

\examples{
data(fmdata)
fc <- flatten(fmdata)
}

