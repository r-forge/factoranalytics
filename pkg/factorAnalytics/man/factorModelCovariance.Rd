\name{factorModelCovariance}
\alias{factorModelCovariance}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
compute covariance matrix from estimated factor model.
}
\description{
compute covariance matrix from estimated factor model.
}
\usage{
factorModelCovariance(beta.mat, factor.cov, residVars.vec)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{beta.mat}{
N x K matrix of factor betas.
}
  \item{factor.cov}{
K x K factor excess return covariance matrix.
}
  \item{residVars.vec}{
N x 1 vector of residual variances from factor model.
}
}
\value{
N x N excess return covariance matrix based on estimated factor model.
}
\author{
Eric Zivot and Yi-An Chen. 
}
\examples{
# factorModelCovariance
data(managers.df)
factors    = managers.df[,(7:9)]
ret.assets = managers.df[,(1:6)]
fit <-fitMacroeconomicFactorModel(ret.assets,factors,fit.method="OLS",
                                  variable.selection="all subsets", factor.set = 3)
factorModelCovariance(fit$beta.mat,var(factors),fit$residVars.vec)
}
