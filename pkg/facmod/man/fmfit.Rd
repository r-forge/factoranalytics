\name{fmfit}
\alias{fmfit}

\title{
fmfit
}

\description{
Fits a factor model using classical or robust cross-sectional time series regression
}

\usage{
fmfit(fmdsobj, timedates, exposures, assets, wls = T, regression = "robust", covariance = "robust", full.resid.cov = F, robust.scale = F, returnsvar = "RETURN", verbose = F)
}
\arguments{
  \item{fmdsobj}{an \code{fmdsCube} object}
  \item{timedates}{vector of class Date specifying the date range for the factor model}
  \item{exposures}{character vector giving the variable names to be used in the factor model}
  \item{assets}{character vector of PERMNOs to be used in the factor model}
  \item{wls}{logical flag, TRUE for weighted least squares, FALSE for ordinary least squares}
  \item{regression}{character string, "robust" for regression via \code{lmRob}, "classic" for regression via \code{lm}}
  \item{covariance}{character string, "robust" for covariance matrix computed via \code{covRob}, "classic" for covariance matrix via \code{cov}}
  \item{full.resid.cov}{logical flag, TRUE for full residual covariance matrix calculation, FALSE for diagonal residual covariance matrix}
  \item{robust.scale}{logical flag, TRUE for exposure scaling via robust scale and location, FALSE for scaling via mean and sd}
  \item{returnsvar}{character string giving the name of the return variable, default is "RETURN"}
  \item{verbose}{numeric value, values greater than zero produce additional diagnostic output}
}

\value{
a named list with the following components
  \item{Cov.returns}{covariance information for asset returns, includes components cov, center, evals, corr}
  \item{Cov.facrets}{covariance information for factor returns, includes components cov, center, evals, corr}
  \item{Cov.resids}{covariance information for model residuals, includes components cov, center, evals, corr}
  \item{resid.vars}{list of information regarding model residuals variance}
  \item{facrets}{zoo time series object of factor returns}
  \item{resids}{zoo time series object of model residuals}
  \item{tstats}{zoo time series object of model t-statistics}
  \item{returns.data}{data.frame of return data including RETURN, DATE, PERMNO}
  \item{exposure.data}{data.frame of exposure data including DATE, PERMNO}
  \item{assets}{character vector of PERMNOs used in the model}
  \item{tickers}{character vector of tickers used in the model}
  \item{call}{function call}
}

\author{
Guy Yollin, \url{http://www.r-programming.org} \cr
(based on S-PLUS code developed by Christopher G. Green, University of Washington)
}

\examples{
}


