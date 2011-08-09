\name{getAssetIDs}
\alias{getAssetIDs}

\title{
getAssetIDs
}

\description{
Get unique asset identifiers (PERMNOs for CRSP data)
}

\usage{
getAssetIDs(object)
}

\arguments{
  \item{object}{ an "fmdsCube" object}
}

\value{
a character vector of asset identifiers
}

\author{
Guy Yollin, \url{http://www.r-programming.org} \cr
(based on S-PLUS code developed by Christopher G. Green, University of Washington)
}

\examples{
data(fmdata)
getAssetIDs(fmdata)
}

