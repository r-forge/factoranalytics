\name{getAssetLabels}
\alias{getAssetLabels}
\title{
getAssetLabels
}
\description{
Get list of asset labels (tickers)
}
\usage{
getAssetLabels(object)
}
\arguments{
  \item{object}{ an "fmdsCube" object}
}
\value{
named list of tickers (names are PERMNOs for CRSP data)
}
\author{
Guy Yollin, \url{http://www.r-programming.org} \cr
(based on S-PLUS code developed by Christopher G. Green, University of Washington)
}
\examples{
data(fmdata)
getAssetLabels(fmdata)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

