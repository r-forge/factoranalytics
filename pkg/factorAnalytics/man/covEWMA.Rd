\name{covEWMA}
\alias{covEWMA}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compute RiskMetrics-type EWMA Covariance Matrix
}
\description{
Compute time series of RiskMetrics-type EWMA covariance matrices of returns. Initial covariance 
matrix is assumed to be the unconditional covariance matrix. 
}
\usage{
covEWMA(factors, lambda = 0.96)
}

\arguments{
  \item{factors}{
\code{T x K} data.frame containing asset returns, where \code{T} is the number of time
periods and \code{K} is the number of assets.
}
  \item{lambda}{
Scalar exponential decay factor. Must lie between between 0 and 1.
}
}
\details{
Let EWMA covariance matrix at time \code{t} is compute as \cr 
\code{Sigma(t) = lambda*Sigma(t-1) + (1-lambda)*R(t)t(R(t))}  \cr
where \code{R(t)} is the \code{K x 1} vector of returns at time \code{t}.
}
\value{
\code{T x K x K} array giving the time series of EWMA covariance matrices. 
}
\references{
Zivot, E. and J. Wang (2006), \emph{Modeling Financial Time Series with S-PLUS, 
Second Edition}, Springer-Verlag.
}
\author{
Eric Zivot and Yi-An Chen.
}
\examples{
# compute time vaying covariance of factors.
data(managers.df)
factors    = managers.df[,(7:9)]
cov.f.ewma <- covEWMA(factors)
cov.f.ewma[120,,]
}

