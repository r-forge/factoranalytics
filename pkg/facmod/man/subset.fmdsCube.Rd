\name{subset.fmdsCube}
\alias{subset.fmdsCube}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
subset.fmdsCube
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
subset.fmdsCube(cube, assetsToRetrieve = NULL, datesToRetrieve = NULL, variablesToRetrieve = NULL, variablesToDrop = NULL, out.format = "cube", rebuildLabels = T, verbose = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cube}{
%%     ~~Describe \code{cube} here~~
}
  \item{assetsToRetrieve}{
%%     ~~Describe \code{assetsToRetrieve} here~~
}
  \item{datesToRetrieve}{
%%     ~~Describe \code{datesToRetrieve} here~~
}
  \item{variablesToRetrieve}{
%%     ~~Describe \code{variablesToRetrieve} here~~
}
  \item{variablesToDrop}{
%%     ~~Describe \code{variablesToDrop} here~~
}
  \item{out.format}{
%%     ~~Describe \code{out.format} here~~
}
  \item{rebuildLabels}{
%%     ~~Describe \code{rebuildLabels} here~~
}
  \item{verbose}{
%%     ~~Describe \code{verbose} here~~
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
function (cube, assetsToRetrieve = NULL, datesToRetrieve = NULL, 
    variablesToRetrieve = NULL, variablesToDrop = NULL, out.format = "cube", 
    rebuildLabels = T, verbose = 0) 
{
    require(zoo)
    if (!is(cube, "fmdsCube")) 
        stop("cube must be a fmdsCube object")
    out.format <- casefold(out.format)
    if (!match(out.format, c("cube", "data.frame"), nomatch = F)) 
        stop("Unrecognized output format option.")
    rebuildLabels <- as.logical(rebuildLabels)
    bHaveVTR <- !is.null(variablesToRetrieve)
    bHaveVTD <- !is.null(variablesToDrop)
    if (bHaveVTR && bHaveVTD) 
        stop("Cannot specify both 'variablesToRetrieve' and 'variablesToDrop'.")
    if (verbose == 2) {
        cat("Running subset.fmdsCube on ", date(), "\n")
        cat("\ttime at start: ", round(proc.time(), 3), " ")
        cat("mem usage(MB):", memory.size(), "\n")
        on.exit(print(mem.tally.report("Max allocation upon exiting subset.fmdsCube: ")))
        print(storageSummary())
        this.call <- sys.call()
        this.call[[2]] <- "cube"
        print(this.call)
        remove("this.call", frame = sys.nframe())
        cat("\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\%\n\n")
    }
    cube.dimen <- dim(cube@data)
    cube.dimnames <- dimnames(cube@data)
    assets.avail <- cube.dimnames[[3]]
    dates.format <- cube@datefmt
    match.format <- cube@matchfmt
    dates.avail <- as.Date(cube.dimnames[[1]], format = dates.format)
    assetvar <- cube@index$assetvar
    datevar <- cube@index$datevar
    labelvar <- cube@labelvar
    if (verbose == 2) {
        cat("\tAfter initialization...start time: ", round(proc.time(), 
            3))
        cat(" mem usage(MB):", memory.size(), "\n")
    }
    if (verbose == 2) 
        cat("\tBefore assetsToRetrieve: time: ", round(proc.time(), 
            3), " mem usage(MB):", memory.size(), "\n")
    if (is.null(assetsToRetrieve)) {
        permIndices <- seq(cube.dimen[3])
    }
    else {
        if (is.numeric(assetsToRetrieve)) {
            if (any((assetsToRetrieve < 1) | (assetsToRetrieve > 
                cube.dimen[3]))) 
                stop("assetsToRetrieve contains numbers greater than the 3rd dimension of the cube---perhaps you passed in PERMNOs as numeric instead of character?")
            permIndices <- assetsToRetrieve
        }
        else {
            if (is.character(assetsToRetrieve)) {
                assetsToRetrieve <- as.character(assetsToRetrieve)
                match.index <- match(assetsToRetrieve, assets.avail, 
                  nomatch = NA)
                match.nas <- is.na(match.index)
                if (length(match.index) == 0 || all(match.nas)) {
                  cat("Error in subset: assetsToRetrieve:\n")
                  print(assetsToRetrieve)
                  stop("Your assetsToRetrieve argument resulted in zero assets selected.")
                }
                else {
                  if (any(match.nas)) {
                    warning(paste("The following input assets did not match anything in the cube and were dropped:", 
                      paste(assetsToRetrieve[match.nas], collapse = " "), 
                      "\n"))
                  }
                }
                permIndices <- match.index[!match.nas]
            }
            else {
                stop("I couldn't make sense of your assetsToRetrieve argument.")
            }
        }
    }
    if (verbose == 2) {
        cat("\tAfter assetsToRetrieve time: ", round(proc.time(), 
            3), " mem usage(MB):", memory.size(), "\n")
        cat("Assets selected:\n")
        print(assets.avail[permIndices])
        cat("\n")
    }
    if (is.null(datesToRetrieve)) {
        datesToRetrieve <- seq(cube.dimen[1])
    }
    else {
        da <- as.character(dates.avail)
        wn <- as.character(time(window(zoo(, order.by = dates.avail), 
            start = datesToRetrieve$start, end = datesToRetrieve$end)))
        datesToRetrieve <- match(wn, da)
    }
    bUseAllDates <- (length(datesToRetrieve) == cube.dimen[1]) && 
        all(datesToRetrieve == seq(cube.dimen[1]))
    if (verbose == 2) {
        cat("\tAfter datesToRetrieve time: ", round(proc.time(), 
            3), " mem usage(MB):", memory.size(), "\n")
        if (bUseAllDates) 
            cat("Using all available dates.\n")
        else cat("Using dates:", datesToRetrieve, "\n")
    }
    data.factors <- cube.dimnames[[2]]
    bHaveInfoSlot <- !is.null(cube@info) && length(cube@info)
    info.factors <- if (bHaveInfoSlot) 
        names(cube@info)
    else character(0)
    if (bHaveVTR || bHaveVTD) {
        factor.loadings <- if (bHaveVTR) 
            variablesToRetrieve
        else variablesToDrop
        wIndex <- match(c(assetvar, datevar), factor.loadings, 
            F)
        if (bHaveVTR && !wIndex[1]) 
            factor.loadings <- c(assetvar, factor.loadings)
        if (bHaveVTR && !wIndex[2]) 
            factor.loadings <- c(datevar, factor.loadings)
        if (bHaveVTD && wIndex[1]) 
            factor.loadings <- factor.loadings[-wIndex[1]]
        if (bHaveVTD && wIndex[2]) 
            factor.loadings <- factor.loadings[-wIndex[2]]
        if (!is.null(labelvar) && rebuildLabels) {
            wIndex <- match(labelvar, factor.loadings, F)
            if (bHaveVTR && !wIndex) 
                factor.loadings <- c(labelvar, factor.loadings)
            if (bHaveVTD && wIndex) 
                factor.loadings <- factor.loadings[-wIndex]
        }
        datanames.indices <- match(factor.loadings, data.factors)
        data.unmatched <- is.na(datanames.indices)
        data.factors <- factor.loadings[!data.unmatched]
        if (all(data.unmatched)) {
            warning("None of the requested variables matched variables in the data slot---data slot will be empty!")
        }
        if (any(data.unmatched)) {
            infonames.indices <- match(factor.loadings[data.unmatched], 
                info.factors)
            info.unmatched <- is.na(infonames.indices)
            if (any(info.unmatched)) {
                warning(paste("The following requested variables could not matched in the data set: ", 
                  paste(factor.loadings[data.unmatched][info.unmatched], 
                    collapse = " "), "\nThese variables will be ignored.\n"))
            }
            info.factors <- factor.loadings[data.unmatched][!info.unmatched]
        }
        else {
            info.factors <- character(0)
        }
        if (verbose == 2) {
            cat("data variables screened: ", data.factors, "\n")
            cat("info variables screened: ", info.factors, "\n")
        }
    }
    data.fields <- as.list(seq(cube.dimen[2]))
    names(data.fields) <- cube.dimnames[[2]]
    if (length(data.factors)) {
        match.indices <- match(data.factors, names(data.fields))
        if (length(which(is.na(match.indices)))) {
            cat(paste(data.factors[is.na(match.indices)], collapse = " "))
            stop("There were unrecognized field names in data.factor?")
        }
        intDataColsToRetrieve <- try(sapply(data.factors, function(x, 
            y) eval(parse(text = x), envir = y), data.fields))
        if (is(intDataColsToRetrieve, "Error")) {
            cat("An error was encountered upon attempting to subset the data slot of the cube.  \n")
            cat("Check that your variablesToRetrieve/variablesToDrop argument conforms to the ")
            cat("documented standard (see the online documentation).\n")
            stop(get.message(intDataColsToRetrieve, even.if.used = T))
        }
        if (bHaveVTD) 
            intDataColsToRetrieve <- seq(cube.dimen[2])[-intDataColsToRetrieve]
    }
    else {
        intDataColsToRetrieve <- if (bHaveVTR) 
            numeric(0)
        else seq(cube.dimen[2])
    }
    if (verbose == 2) {
        cat("Retrieving variables: \n")
        print(data.fields[intDataColsToRetrieve])
        cat("\tAfter data variable subset: time: ", round(proc.time(), 
            3), " mem usage(MB):", memory.size(), "\n")
    }
    if (bHaveInfoSlot) {
        info.fields <- as.list(seq(ncol(cube@info)))
        names(info.fields) <- names(cube@info)
    }
    else info.fields <- list()
    n.info.fields <- length(info.fields)
    if (length(info.factors) && n.info.fields) {
        match.indices <- match(info.factors, names(info.fields))
        if (length(which(is.na(match.indices)))) {
            cat(paste(info.factors[which(is.na(match.indices))], 
                collapse = " "))
            stop("There were unrecognized field names in info.factor?")
        }
        intInfoColsToRetrieve <- try(unlist(lapply(info.factors, 
            function(x, y) eval(parse(text = x), envir = y), 
            info.fields)))
        if (is(intInfoColsToRetrieve, "Error")) {
            cat("An error was encountered upon attempting to subset the info slot of the cube.  \n")
            cat("Check that your infoVariablesToRetrieve conforms to the documented standard (see the online ")
            cat("documentation.\n")
            stop(get.message(intInfoColsToRetrieve, even.if.used = T))
        }
        if (bHaveVTD) 
            intInfoColsToRetrieve <- seq(n.info.fields)[-intInfoColsToRetrieve]
    }
    else {
        intInfoColsToRetrieve <- if (bHaveVTR) 
            numeric(0)
        else if (bHaveInfoSlot) 
            seq(n.info.fields)
        else numeric(0)
    }
    if (verbose == 2) {
        cat("Retrieving variables: \n")
        print(info.fields[intInfoColsToRetrieve])
        cat("\tAfter infoVariablesToRetrieve time: ", round(proc.time(), 
            3), " mem usage(MB):", memory.size(), "\n")
    }
    if (verbose == 2) {
        cat("permIndices:\n")
        print(permIndices)
        cat("\n")
    }
    permIndices <- permIndices[!is.na(permIndices)]
    if (bHaveInfoSlot) {
        info <- cube@info[intInfoColsToRetrieve]
        info <- info[order(info[[assetvar]], info[[datevar]]), 
            ]
        info <- info[unlist(split(seq(nrow(info)), info[[assetvar]])[permIndices], 
            use.names = F), ]
        if (verbose == 2) {
            cat("\tAfter subsetting info slot by variables and assets...time: ", 
                round(proc.time(), 3), " mem usage(MB):", memory.size(), 
                "\n")
        }
        if (!bUseAllDates) {
            makeInfoFunc <- function(xdf, start.date, end.date, 
                dtfmt, dtv, asv) {
                num.start.date <- as.numeric(start.date)
                num.end.date <- as.numeric(end.date)
                if (num.end.date < num.start.date) 
                  stop("start date must precede or be equal to end date.")
                dtvc <- xdf[[dtv]]
                asnm <- xdf[[asv]][1]
                if (!is(dtvc, "Date")) 
                  dtvc <- as.Date(as.character(dtvc), format = dtfmt)
                dtvc <- as.numeric(dtvc)
                y <- which(dtvc <= num.start.date)
                if (length(y) == 0) {
                  xdf <- rbind(rep(NA, ncol(xdf)), xdf)
                  xdf[[dtv]][1] <- start.date
                  xdf[[asv]][1] <- asnm
                  dtvc <- c(num.start.date, dtvc)
                }
                else {
                  z <- max(y)
                  dtvc[z] <- num.start.date
                  xdf[[dtv]][z] <- start.date
                  n <- seq(z, length(dtvc))
                  dtvc <- dtvc[n]
                  xdf <- xdf[n, ]
                }
                y <- which(dtvc <= num.end.date)
                if (length(y) == 0) {
                  stop("end date occurs before start date---this should not be?")
                }
                z <- seq(1, max(y))
                xdf <- xdf[z, ]
                xdf
            }
            infoBy <- by(info, info[[assetvar]], FUN = makeInfoFunc, 
                start.date = dates.avail[datesToRetrieve[1]], 
                end.date = dates.avail[datesToRetrieve[length(datesToRetrieve)]], 
                dtfmt = dates.format, dtv = datevar, asv = assetvar, 
                simplify = T)
            info <- do.call("rbind", infoBy)
        }
    }
    else {
        info <- NULL
    }
    if (verbose == 2) {
        cat("\tAfter subsetting info slot...time: ", round(proc.time(), 
            3), " mem usage(MB):", memory.size(), "\n")
    }
    labels <- if (rebuildLabels && !is.null(labelvar)) 
        tapply(as.character(info[[labelvar]]), info[[assetvar]], 
            unique.default)
    else cube@labels
    labels <- as.list(labels)
    retval <- new("fmdsCube.CRSP", data = cube@data[datesToRetrieve, 
        intDataColsToRetrieve, permIndices, drop = F], labels = labels, 
        info = info, auxData = cube@auxData, buildDate = as.Date(format(Sys.time(), 
            "\%Y-\%m-\%d")), datefmt = dates.format, matchfmt = match.format, 
        index = cube@index, contents = cube@contents, labelvar = labelvar)
    return(retval)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
