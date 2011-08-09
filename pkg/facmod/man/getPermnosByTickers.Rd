\name{getPermnosByTickers}
\alias{getPermnosByTickers}

\title{
getPermnosByTickers
}

\description{
return list of CRSP PERMNO numbers based on ticker symbols
}

\usage{
getPermnosByTickers(object, values)
}
\arguments{
  \item{object}{ an "fmdsCube" object}
  \item{values}{character vector of ticker symbols}
}

\value{
a named list of PERMNOs
}

\author{
Guy Yollin, \url{http://www.r-programming.org} \cr
(based on S-PLUS code developed by Christopher G. Green, University of Washington)
}

\examples{
data(fmdata)
getPermnosByTickers(fmdata,c("MSFT","INTC"))
}

