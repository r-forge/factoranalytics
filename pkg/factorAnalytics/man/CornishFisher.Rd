\name{CornishFisher}
\alias{rCornishFisher}
\alias{dCornishFisher}
\alias{pCornishFisher}
\alias{qCornishFisher}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Functions for Cornish-Fisher density, CDF, random number simulation
and quantile.
}
\description{
\code{dCornishFisher} Computes Cornish-Fisher density from two term Edgeworth expansion given mean, 
standard deviation, skewness and excess kurtosis. \code{pCornishFisher} Computes Cornish-Fisher 
CDF from two term Edgeworth expansion given mean, standard deviation, skewness and excess kurtosis.
\code{qCornishFisher} Computes Cornish-Fisher quantiles from two term Edgeworth expansion given mean, 
standard deviation, skewness and excess kurtosis. \code{rCornishFisher} simulate observations based on Cornish-Fisher quantile expansion given mean, standard deviation, skewness and excess kurtosis.
}
\usage{
dCornishFisher(x, n,skew, ekurt)
pCornishFisher(q, n,skew, ekurt)
qCornishFisher(p, n,skew, ekurt)
rCornishFisher(n, sigma, skew, ekurt, seed = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{
scalar, number of simulated values in rCornishFisher. Sample length in density,distribution,quantile function.
}
  \item{sigma}{
scalar, standard deviation.
}
  \item{skew}{
scalar, skewness.
}
  \item{ekurt}{
scalar, excess kurtosis.
}
  \item{seed}{
set seed here. Default is \code{NULL}.
}
  \item{x,q}{
  vector of quantiles.
  }
  \item{p}{
  vector of probabilities.
  }
}
\value{
n simulated values from Cornish-Fisher distribution.
}
\references{
A.DasGupta, "Asymptotic Theory of Statistics and Probability", Springer Science+Business
Media,LLC 2008
}
\author{
Eric Zivot and Yi-An Chen.
}
\examples{
# generate 1000 observation from Cornish-Fisher distribution
rc <- rCornishFisher(1000,1,0,5)
hist(rc,breaks=100,freq=FALSE,main="simulation of Cornish Fisher Distribution",
      xlim=c(-10,10))
lines(seq(-10,10,0.1),dnorm(seq(-10,10,0.1),mean=0,sd=1),col=2) # compare with 
      standard normal curve

# generate student t distribution with df 5, so it fatter than norma distribution
x <- rt(10,df=5)
n <- length(x)
pCornishFisher(x,n,skew=0,ekurt=6) # student t has skewness 0 and excess kurtosis 6/(v-4)
pt(x,df=5) # exact cdf
dCornishFisher(x,n,skew=0,ekurt=6)
dt(x,df=5) # exact density

}
