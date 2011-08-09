\name{fitFundamentalFactorModel}
\alias{fitFundamentalFactorModel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
fit fundamental factor model by classic OLS or Robust regression technique
}
\description{
fit fundamental factor model or cross-sectional time series factor model by classic OLS or Robust regression technique.  Fundamental factor models use observable asset specific characteristics (fundamentals) like industry classification, market capitalization, style classification
(value, growth) etc. to determine the common risk factors.
}
\usage{
fitFundamentalFactorModel(fmdsobj, timedates, exposures, assets, wls = FALSE, regression = "classic", covariance = "classic", full.resid.cov = TRUE, robust.scale = FALSE, returnsvar = "RETURN")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fmdsobj}{
an fmdsCube object (i.e. the data)
}
  \item{timedates}{
a vector of Dates specifying the date range for the model fitting
}
  \item{exposures}{
a character vector of exposure names for the factor model
}
  \item{assets}{
a list of PERMNOs to be used for the factor model
}
  \item{wls}{ 
logical flag, TRUE for weighted least squares, FALSE for
ordinary least squares
}
  \item{regression}{
character string, "robust" for regression via lmRob, "classic"
for regression via lm
}
  \item{covariance}{
character string, "robust" for covariance matrix computed via
covRob, "classic" for covariance matrix via ccov
}
  \item{full.resid.cov}{
logical flag, TRUE for full residual covariance matrix
calculation, FALSE for diagonal residual covarinace matrix
}
  \item{robust.scale}{
logical flag, TRUE for exposure scaling via robust scale and
location, FALSE for scaling via mean and sd
}
  \item{returnsvar}{
character string giving the name of the return variable in the array.
}
}
\details{
The original function was designed by Doug Martin and originally implemented in S-PLUS by a number of
UW Ph.D. students:Christopher Green, Eric Aldrich, and Yindeng Jiang.
Guy Yullen re-implemented the function in R and requires the following additional R libraries:
zoo time series library, robust Insightful robust library ported to R and
robustbase Basic robust statistics package for R
}
\value{
\item{cov.returns}{
covariance information for asset returns, includes
covariance, mean, eigenvalus
}           
\item{cov.factor.rets}{
covariance information for factor returns, includes
covariance and mean
}          
\item{cov.resids}{
covariance information for model residuals, includes
covariance and mean
}         
\item{resid.vars}{
list of information regarding model residuals variance
}            
\item{factor.rets}{
zoo time series object of factor returns
}               
\item{resids}{
zoo time series object of model residuals
}               
\item{tstats}{
zoo time series object of model t-statistics
}                 
\item{returns.data}{
data.frame of return data including RETURN, DATE,PERMNO
} 
\item{exposure.data}{
data.frame of exposure data including DATE, PERMNO
}
\item{assets}{
character vector of PERMNOs used in the model
} 
\item{tickers}{
character vector of tickers used in the model
}                
\item{call}{
function call
}                   
}

\author{
Guy Yullen and Yi-An Chen
}

\examples{
library(facmod)

objects("package:facmod")
data(fmdata)
assets <- getAssetIDs(fmdata)
timedates <- as.Date(getAvailableTimes(fmdata))
exposures <- exposures.names <- c("BOOK2MARKET", "LOG.MARKETCAP")
slotNames(fmdata)
class(fmdata@data)
dim(fmdata@data)

test.fit <- fitFundamentalFactorModel(fmdsobj=fmdata, timedates=timedates, exposures=exposures,
                                      covariance="classic", assets=assets,full.resid.cov=TRUE,
                                      regression="classic",wls=TRUE)
names(test.fit)
test.fit$cov.returns
test.fit$cov.facrets
test.fit$facrets
}
