\name{scaleNumericFactors.fmdsCube}
\alias{scaleNumericFactors.fmdsCube}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
scaleNumericFactors.fmdsCube
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
scaleNumericFactors.fmdsCube(cube, robust.location = F, robust.scale = F, na.rm = T, robust.location.args = NULL, robust.scale.args = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cube}{
%%     ~~Describe \code{cube} here~~
}
  \item{robust.location}{
%%     ~~Describe \code{robust.location} here~~
}
  \item{robust.scale}{
%%     ~~Describe \code{robust.scale} here~~
}
  \item{na.rm}{
%%     ~~Describe \code{na.rm} here~~
}
  \item{robust.location.args}{
%%     ~~Describe \code{robust.location.args} here~~
}
  \item{robust.scale.args}{
%%     ~~Describe \code{robust.scale.args} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (cube, robust.location = F, robust.scale = F, na.rm = T, 
    robust.location.args = NULL, robust.scale.args = NULL) 
{
    if (!is(cube, "fmdsCube")) 
        stop("Input must be an fmdsCube object.")
    x <- cube@data
    array.dims <- dim(x)
    locest <- if (robust.location) 
        apply(x, c(1, 2), location.m, na.rm = na.rm)
    else apply(x, c(1, 2), function(x, na.rm) mean(x[is.finite(x)], 
        na.rm = na.rm), na.rm = na.rm)
    x <- x - array(rep(locest, array.dims[3]), dim = array.dims)
    scalest <- if (robust.scale) 
        apply(x, c(1, 2), scale.tau, na.rm = na.rm)
    else apply(x, c(1, 2), function(x, na.rm) sd(x[is.finite(x)], 
        na.rm = na.rm), na.rm = na.rm)
    scalest[scalest == 0] <- 1
    x <- x/array(rep(scalest, array.dims[3]), dim = array.dims)
    cube@data <- x
    cube
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
