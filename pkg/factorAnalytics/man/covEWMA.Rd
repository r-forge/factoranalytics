\name{covEWMA}
\alias{covEWMA}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
compute RiskMetrics-type EWMA covariance matrix
}
\description{
compute RiskMetrics-type EWMA covariance matrix. Covariance matrix at time 0 is 
presumed to be the unconditional covariance of factors. 
}
\usage{
covEWMA(factors, lambda = 0.96)
}

\arguments{
  \item{factors}{
N x K numerical factors data.  data is class data.frame
N is the time length and K is the number of the factors.
}
  \item{lambda}{
scalar. exponetial decay factor between 0 and 1.
}
}
\value{
array. dimension is N x K x K.
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

