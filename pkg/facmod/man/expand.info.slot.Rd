\name{expand.info.slot}
\alias{expand.info.slot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
expand.info.slot
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
expand.info.slot(cube)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cube}{
%%     ~~Describe \code{cube} here~~
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
function (cube) 
{
    cube.dimen <- dim(cube@data)
    cube.dimnames <- dimnames(cube@data)
    dates.format <- cube@datefmt
    match.format <- cube@matchfmt
    dates.avail <- as.Date(cube.dimnames[[1]])
    datevar <- cube@index$datevar
    assetvar <- cube@index$assetvar
    info <- cube@info
    if (!is.null(info) && length(info)) {
        if (!all(match(c(datevar, assetvar), names(info), F))) 
            stop("Malformed cube: column(s) corresponding to one or both of the index variables is/are missing.")
        info <- info[order(info[[assetvar]], info[[datevar]]), 
            ]
        assetvar.data <- info[[assetvar]]
        dates.have.indices <- match(as.character(info[[datevar]]), 
            as.character(dates.avail))
        if (length(which(is.na(dates.have.indices)))) 
            stop("There were dates in info slot did not match dates in data slot---improperly built cube?")
        dates.wanted.indices <- tapply(dates.have.indices, assetvar.data, 
            function(x, n) rep(c(NA, seq(along = x)), diff(c(1, 
                x, n + 1)))[1:n], length(dates.avail))
        rowind <- unlist(lapply(seq(along = dates.wanted.indices), 
            function(i, a, b) a[[i]][b[[i]]], a = split(seq(nrow(info)), 
                assetvar.data), b = dates.wanted.indices))
        attr(info, "dup.row.names") <- T
        info <- info[rowind, ]
        julian = unlist(tapply(X = info[[datevar]], INDEX = info[[assetvar]], 
            FUN = function(x, d) as.numeric(d), dates.avail, 
            simplify = F))
        info[[datevar]] <- as.Date(julian, origin = "1970-01-01")
        otn <- names(info)
        otn[otn == datevar] <- paste(datevar, "2", sep = "")
        otn[otn == assetvar] <- paste(assetvar, "2", sep = "")
        names(info) <- otn
        attr(info, "row.names") <- as.character(seq(nrow(info)))
        attr(info, "dup.row.names") <- F
    }
    info
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
