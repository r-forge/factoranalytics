\name{getTickersByPermnos}
\alias{getTickersByPermnos}

\title{
getTickersByPermnos
}

\description{
return list of tickers based on their unique CRSP PERMNO number
}

\usage{
getTickersByPermnos(object, values)
}
\arguments{
  \item{object}{ an "fmdsCube" object}
  \item{values}{character vector of PERMNOs}
}

\value{
a named list of tickers (named by PERMNO)
}

\author{
Guy Yollin, \url{http://www.r-programming.org} \cr
(based on S-PLUS code developed by Christopher G. Green, University of Washington)
}

\examples{
data(fmdata)
getTickersByPermnos(fmdata,c("59328","10107"))
}

