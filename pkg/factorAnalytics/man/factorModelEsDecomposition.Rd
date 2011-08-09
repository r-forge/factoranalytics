\name{factorModelEsDecomposition}
\alias{factorModelEsDecomposition}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compute factor model factor ES decomposition
}
\description{
Compute factor model factor ES decomposition based on Euler's theorem given historic 
or simulated data and factor model parameters.
The partial derivative of ES wrt factor beta is computed
as the expected factor return given fund return is less than or equal to its VaR
VaR is compute either as the sample quantile or as an estimated quantile
using the Cornish-Fisher expansion.
}
\usage{
factorModelEsDecomposition(bootData, beta.vec, sig2.e, tail.prob = 0.01, 
                                VaR.method = c("HS", "CornishFisher"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{bootData}{
              B x (k+2) matrix of bootstrap data. First column contains the fund returns,
              second through k+1 columns contain factor returns, (k+2)nd column contain residuals
              scaled to have variance 1.
}
  \item{beta.vec}{
              k x 1 vector of factor betas.
}
  \item{sig2.e}{
             scalar, residual variance from factor model.
}
  \item{tail.prob}{
      scalar, tail probability
}
    \item{VaR.method}{
              character, method for computing VaR. Valid choices are "HS" for
            historical simulation (empirical quantile); "CornishFisher" for
            modified VaR based on Cornish-Fisher quantile estimate. Cornish-Fisher
            computation is done with the VaR.CornishFisher in the PerformanceAnalytics
            package
}
}
\details{
 The factor model has the form
 R(t) = beta'F(t) + e(t) = beta.star'F.star(t)
 where beta.star = (beta, sig.e)' and F.star(t) = (F(t)', z(t))'
 By Euler's theorem
 ES.fm = sum(cES.fm) = sum(beta.star*mcES.fm)
}
\value{
\item{VaR.fm}{
Scalar, bootstrap VaR value for fund reported as a positive number.
}  
\item{n.exceed}{
Scalar, number of observations beyond VaR.
}            
\item{idx.exceed}{
n.exceed x 1 vector giving index values of exceedences.
}
\item{ES.fm}{
scalar, bootstrap ES value for fund reported as a positive number.
}
\item{mcES.fm}{
(K+1) x 1 vector of factor marginal contributions to ES.
}   
\item{cES.fm}{
(K+1) x 1 vector of factor component contributions to ES.
}
\item{pcES.fm}{
(K+1) x 1 vector of factor percent contributions to ES.
}          
}
\references{
1. Hallerback (2003), "Decomposing Portfolio Value-at-Risk: A General Analysis",
    The Journal of Risk 5/2.
2. Yamai and Yoshiba (2002). "Comparative Analyses of Expected Shortfall and
   Value-at-Risk: Their Estimation Error, Decomposition, and Optimization
    Bank of Japan.
3. Meucci (2007). "Risk Contributions from Generic User-Defined Factors," Risk.
}
\author{
Eric Zviot and Yi-An Chen.
}
\examples{
data(managers.df)
ret.assets = managers.df[,(1:6)]
factors    = managers.df[,(7:9)]
# fit the factor model with OLS
fit <- fitMacroeconomicFactorModel(ret.assets,factors,fit.method="OLS",
                                 variable.selection="all subsets",factor.set=3)
# risk factor contribution to ETL
# combine fund returns, factor returns and residual returns for HAM1
tmpData = cbind(managers.df[,1], managers.df[,(7:9)],
               residuals(fit$asset.fit$HAM1)/sqrt(fit$residVars.vec[1]))
colnames(tmpData)[c(1,5)] = c("HAM1", "residual")
factor.es.decomp.HAM1 = factorModelEsDecomposition(tmpData, fit$beta.mat[1,],
                                                fit$residVars.vec[1], tail.prob=0.05)


}

