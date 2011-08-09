\name{fmdsCube.CRSP-class}
\Rdversion{1.1}
\docType{class}
\alias{fmdsCube.CRSP-class}
\alias{getPermnosByTickers,fmdsCube.CRSP,vector-method}
\alias{getTickersByPermnos,fmdsCube.CRSP,vector-method}

\title{Class \code{"fmdsCube.CRSP"}}
\description{
%%  ~~ A concise (1-5 lines) description of what the class is. ~~
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("fmdsCube.CRSP", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{interp}:}{Object of class \code{"character"} ~~ }
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
Class \code{"\linkS4class{fmdsCube}"}, directly.
Class \code{"\linkS4class{fmds}"}, by class "fmdsCube", distance 2.
}
\section{Methods}{
  \describe{
    \item{getPermnosByTickers}{\code{signature(object = "fmdsCube.CRSP", values = "vector")}: ... }
    \item{getTickersByPermnos}{\code{signature(object = "fmdsCube.CRSP", values = "vector")}: ... }
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
showClass("fmdsCube.CRSP")
}
\keyword{classes}
