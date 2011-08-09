\name{plotCorr}
\alias{plotCorr}

\title{
plotCorr
}

\description{
plot correlation ellipses
}

\usage{
plotCorr(cor1, cor2 = NULL, labels1 = "", labels2 = "", method1 = "", method2 = "", ndigits = 4, lab.cex = 1, col1 = 4, col2 = 2, ...)
}
\arguments{
  \item{cor1}{1st correlation (covariance) matrix}
  \item{cor2}{2nd correlation (covariance) matrix}
  \item{labels1}{column labels for plot}
  \item{labels2}{row labels for plot}
  \item{method1}{description of 1st correlation matrix}
  \item{method2}{description of 1st correlation matrix}
  \item{ndigits}{number of digits}
  \item{lab.cex}{character expansion factor for labels}
  \item{col1}{color for 1st matrix and associated labels}
  \item{col2}{color for 2nd matrix and associated labels}
  \item{\dots}{additional graphical parameters}
}

\section{Side Effects}{
correlation ellipeses and correlation coefficients are plotted
}

\author{
Guy Yollin, \url{http://www.r-programming.org}
}

\examples{
c1 <- var(matrix(rnorm(60),ncol=3,dimnames=list(NULL,c("V1","V2","V3"))))
plotCorr(c1)
c2 <- var(matrix(rnorm(60),ncol=3,dimnames=list(NULL,c("V1","V2","V3"))))
plotCorr(c1,c2,method1="classic",method2="robust")
}

