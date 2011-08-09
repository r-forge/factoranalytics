\name{fmdsCube-class}
\Rdversion{1.1}
\docType{class}
\alias{fmdsCube-class}
\alias{flatten,fmdsCube-method}
\alias{getAssetIDs,fmdsCube-method}
\alias{getAssetLabels,fmdsCube-method}
\alias{getAvailableTimes,fmdsCube-method}
\alias{scaleNumericFactors,fmdsCube-method}
\alias{show,fmdsCube-method}
\alias{subset,fmdsCube-method}

\title{Class \code{"fmdsCube"}}
\description{
%%  ~~ A concise (1-5 lines) description of what the class is. ~~
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("fmdsCube", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{data}:}{Object of class \code{"array"} ~~ }
    \item{\code{labels}:}{Object of class \code{"list"} ~~ }
    \item{\code{info}:}{Object of class \code{"data.frame"} ~~ }
    \item{\code{auxData}:}{Object of class \code{"list"} ~~ }
    \item{\code{datefmt}:}{Object of class \code{"character"} ~~ }
    \item{\code{matchfmt}:}{Object of class \code{"character"} ~~ }
    \item{\code{contents}:}{Object of class \code{"matrix"} ~~ }
    \item{\code{index}:}{Object of class \code{"list"} ~~ }
    \item{\code{labelvar}:}{Object of class \code{"character"} ~~ }
    \item{\code{buildDate}:}{Object of class \code{"Date"} ~~ }
  }
}
\section{Extends}{
Class \code{"\linkS4class{fmds}"}, directly.
}
\section{Methods}{
  \describe{
    \item{flatten}{\code{signature(cube = "fmdsCube")}: ... }
    \item{getAssetIDs}{\code{signature(object = "fmdsCube")}: ... }
    \item{getAssetLabels}{\code{signature(object = "fmdsCube")}: ... }
    \item{getAvailableTimes}{\code{signature(object = "fmdsCube")}: ... }
    \item{scaleNumericFactors}{\code{signature(cube = "fmdsCube")}: ... }
    \item{show}{\code{signature(object = "fmdsCube")}: ... }
    \item{subset}{\code{signature(x = "fmdsCube")}: ... }
	 }
}
\references{
%%  ~~put references to the literature/web site here~~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%%  ~~objects to See Also as \code{\link{~~fun~~}}, ~~~
%%  ~~or \code{\linkS4class{CLASSNAME}} for links to other classes ~~~
}
\examples{
showClass("fmdsCube")
}
\keyword{classes}
