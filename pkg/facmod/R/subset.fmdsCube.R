#
# subset.cube
# 
# function to extract subsets from a fmdsCube object
#
#
#
#
# cube                         a fmdsCube object (required!)
# out.format                   output format, "cube" or "data.frame"
# assetsToRetrieve             a set of assets, specified as one of the following: 
#                                1. a numeric vector of indices into the third dimension 
#                                   of the data slot of the cube;
#                                2. a character vector of labels taken from the third dimension
#                                   of the data slot of the cube.
#                              if assets is omitted, all assets will be used.
#
# datesToRetrieve              date range to retain, specified as a list, with entries "start", 
#                                   "end", and possibly "format".
#                                   the "start" and "end" fields are single dates and may be 
#                                   either character strings (i.e., "2001-09-30") or Date 
#                                   objects (it is okay to mix the two).  If character 
#                                   strings are used you must provide format specification
#                                   (compatible with that of the Date class---for example,
#                                   "%m/%d/%Y") that can be used to read the strings 
#                                   (so the two strings must have the same format).
#                                   if "start" is omitted, the first available date will be assumed.
#                                   if "end" is omitted, the last available date will be assumed.
#                              if omitted, all dates will be retained.
#
# variablesToRetrieve          variables to retain from the cube, specified as a character vector 
#                              of variable names.
#                              if omitted, all variables will be retained.
#
#                              note that some functions (such as getCompanyNamesByPermnos and 
#                              getTickersByCompanyNames) depend on the presence of certain variables 
#                              in the info slot of the cube, so if you do not retain
#                              these variables these functions won't work.
#                              
#                              the cube@index in the info slot will always be kept, as things 
#                              don't make much sense without them.
#
# variablesToDrop              variables to drop from the cube, specified as a character vector of 
#                              variable names.  if omitted, all variables will be retained.
#                              it is an error to specify both variablesToRetrieve and variablesToKeep.
#
# verbose  					debugging only
#
"subset.fmdsCube" <- function(cube, assetsToRetrieve=NULL, datesToRetrieve=NULL, variablesToRetrieve=NULL, variablesToDrop=NULL,
    out.format="cube", rebuildLabels=T, verbose=0) {
require(zoo)

    if ( !is(cube, "fmdsCube") )
        stop("cube must be a fmdsCube object")

    out.format <- casefold(out.format)    
    if ( !match(out.format, c("cube","data.frame"), nomatch=F) ) 
        stop("Unrecognized output format option.")

	rebuildLabels <- as.logical(rebuildLabels)
	
	bHaveVTR <- !is.null(variablesToRetrieve)
	bHaveVTD <- !is.null(variablesToDrop)

	if ( bHaveVTR && bHaveVTD ) stop("Cannot specify both 'variablesToRetrieve' and 'variablesToDrop'.")

	# only used for memory tuning
	if ( verbose == 2 ) {
        cat("Running subset.fmdsCube on ",date(),"\n")
		cat("\ttime at start: ",round(proc.time(),3)," ")
		cat("mem usage(MB):",memory.size(),"\n")
		on.exit(print(mem.tally.report("Max allocation upon exiting subset.fmdsCube: ")))
		print(storageSummary())
		this.call <- sys.call()
		# avoid printing the cube
		this.call[[2]] <- "cube"
		print(this.call); remove("this.call",frame=sys.nframe())
		cat("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n\n")
    }
    
    # dimensions of the cube
    cube.dimen    <- dim(cube@data)

	# names from the data slot
	cube.dimnames <- dimnames(cube@data)
    
    # available assets
    assets.avail <- cube.dimnames[[3]]
    
	# date format for dates in the cube
	dates.format <- cube@datefmt

	# format for matching dates
	match.format <- cube@matchfmt

    # available dates
    dates.avail <- as.Date(cube.dimnames[[1]],format=dates.format)

	# asset variable
	assetvar <- cube@index$assetvar
	# date variable
	datevar  <- cube@index$datevar
	# label variable
	labelvar <- cube@labelvar

	if ( verbose == 2 ) {
		cat("\tAfter initialization...start time: ",round(proc.time(),3))
		cat(" mem usage(MB):",memory.size(),"\n")
	}
		
    ##########################################################################################
    # compute indices to subset of assetsToRetrieve
    # 1. if assetsToRetrieve is missing, take all assetsToRetrieve
    #
    # 2. if assetsToRetrieve is a numeric vector of indices, we use those indices directly in 
    #    the third dimension of the data slot of the cube
    #
    # 3. if assetsToRetrieve is a character vector of asset labels, we match the entries against 
	#    the names of the data slot along the third dimension
    # 
    # 4. otherwise gripe
    ###########################################################################################
   
   	if ( verbose == 2 ) 
		cat("\tBefore assetsToRetrieve: time: ", round(proc.time(),3), " mem usage(MB):",memory.size(),"\n")
		
    if ( is.null(assetsToRetrieve) ) {
      permIndices <- seq(cube.dimen[3])
	  } else {
      if ( is.numeric(assetsToRetrieve) ) {
   	   # users may mistakenly pass in a numeric vector of labels 
		    # instead of a character vector
		    # try to catch this here, otherwise there will be an error later on
		    if ( any( (assetsToRetrieve < 1) | (assetsToRetrieve > cube.dimen[3]) ) )
			    stop("assetsToRetrieve contains numbers greater than the 3rd dimension of the cube---perhaps you passed in PERMNOs as numeric instead of character?")	
	      permIndices <- assetsToRetrieve
	    } else {
        if ( is.character(assetsToRetrieve) ) {
		      # for factors
		      assetsToRetrieve <- as.character(assetsToRetrieve)
          # is this a vector of labels?
          match.index <- match(assetsToRetrieve,assets.avail,nomatch=NA)
		      match.nas   <- is.na(match.index)
          if ( length(match.index) == 0 || all(match.nas) ) { 
			      cat("Error in subset: assetsToRetrieve:\n")
			      print(assetsToRetrieve)
			      stop("Your assetsToRetrieve argument resulted in zero assets selected.")
		      } else {
            if ( any(match.nas) ) {
    		      warning(paste("The following input assets did not match anything in the cube and were dropped:",
  				    paste(assetsToRetrieve[match.nas],collapse=" "),"\n"))
            }
		      }
          permIndices <- match.index[!match.nas]
		    } else { 
            stop("I couldn't make sense of your assetsToRetrieve argument.")
        }
		  }
    }

	
	if ( verbose == 2 )  {
		cat("\tAfter assetsToRetrieve time: ",round(proc.time(),3), " mem usage(MB):",memory.size(),"\n")
		cat("Assets selected:\n");print(assets.avail[permIndices]);cat("\n")
	}

    ###############################################################################
    # compute indices of datesToRetrieve 
    # 1. if datesToRetrieve is missing, use all dates
    # 
    # 2. if datesToRetrieve is a list with entries start and end, use those to match
    # 
    # otherwise gripe
    ###############################################################################
    if ( is.null(datesToRetrieve) ) {
      datesToRetrieve <-  seq(cube.dimen[1])
    } else {
      da <- as.character(dates.avail)
      wn <- as.character(time(window(zoo(,order.by=dates.avail),start=datesToRetrieve$start,end=datesToRetrieve$end)))
      datesToRetrieve <- match(wn,da)
    }
	
	# 5/26/2005
	# if we are using all dates, we won't need to do the expensive info slot operation at 
	# the end
	bUseAllDates <- (length(datesToRetrieve) == cube.dimen[1]) && all(datesToRetrieve == seq(cube.dimen[1]))

	if ( verbose == 2 ) {
		cat("\tAfter datesToRetrieve time: ",round(proc.time(),3), " mem usage(MB):",memory.size(),"\n")
		if ( bUseAllDates ) cat("Using all available dates.\n") else cat("Using dates:",datesToRetrieve,"\n")
	}

	###############################################################################
	# column (exposure/factor/variable) selection
	###############################################################################

	data.factors <- cube.dimnames[[2]]

	# this boolean expression is used multiple times below to identify a cube with a null
	# info slot
	bHaveInfoSlot <- !is.null(cube@info) && length(cube@info) 
	info.factors <- if ( bHaveInfoSlot ) names(cube@info) else character(0)

	# if we have neither variablesToRetrieve nor variablesToDrop, we are done, we just
	# use data.factors and info.factors in the next sections
	# otherwise, we have one of variablesToRetrieve/variablesToDrop (not both---that 
	# case handled at beginning of function)
	if ( bHaveVTR || bHaveVTD ) {
		# split variablesToRetrieve/Drop into data and info components
		factor.loadings <- if (bHaveVTR) variablesToRetrieve else variablesToDrop

		# make sure we hang onto the index variables
		wIndex <- match(c(assetvar, datevar),factor.loadings,F)

		if ( bHaveVTR && !wIndex[1] )	factor.loadings <- c(assetvar, factor.loadings)
		if ( bHaveVTR && !wIndex[2] )	factor.loadings <- c(datevar , factor.loadings)

		if ( bHaveVTD &&  wIndex[1] )  factor.loadings <- factor.loadings[-wIndex[1]]
		if ( bHaveVTD &&  wIndex[2] )  factor.loadings <- factor.loadings[-wIndex[2]]
			
		# if a label column was specified, make sure we hang onto that too, provided rebuildLabels is true
		if ( !is.null(labelvar) && rebuildLabels ) {
			wIndex <- match(labelvar, factor.loadings,F)
			if ( bHaveVTR && !wIndex ) factor.loadings <- c(labelvar, factor.loadings)
			if ( bHaveVTD &&  wIndex ) factor.loadings <- factor.loadings[-wIndex]
		}

		# worry about field names duplicated in info and data slots? 
		datanames.indices <- match(factor.loadings,data.factors)
		data.unmatched    <- is.na(datanames.indices) 
		data.factors      <- factor.loadings[!data.unmatched]

		# if there were any unmatched names, try to match them against the info names
		if ( all(data.unmatched) ) {
			warning("None of the requested variables matched variables in the data slot---data slot will be empty!")
		}
		if ( any(data.unmatched) ) {
			infonames.indices <- match(factor.loadings[data.unmatched],info.factors)
			info.unmatched <- is.na(infonames.indices)

			# we always have at least the two index variables, so if there are no matches, 
			# result will be a cube with an empty data slot (length 0 in second dimension)
			# and an info slot with just the two index variables
			
			if ( any(info.unmatched) ) {
					warning(paste("The following requested variables could not matched in the data set: ", 
						paste(factor.loadings[data.unmatched][info.unmatched],collapse=" "), "\nThese variables will be ignored.\n"))
			}
			info.factors <- factor.loadings[data.unmatched][!info.unmatched]
        }
        else {
                info.factors <- character(0)
        }

		if ( verbose == 2 ) {
        	cat("data variables screened: ", data.factors, "\n")
        	cat("info variables screened: ", info.factors, "\n")
		}
	} # end either bHaveVTR or bHaveVTD

    ###############################################################################    
    # find columns to keep in the data slot
    ###############################################################################

    # field names for matching and expression validation
    data.fields <- as.list(seq(cube.dimen[2])) 
    names(data.fields) <- cube.dimnames[[2]]

	if ( length(data.factors) ) {
		# try to match variable names against available fields
		match.indices <- match(data.factors, names(data.fields))
		if ( length(which(is.na(match.indices))) ) {
			# should never get here if computation of data.factors was correct
			cat(paste(data.factors[is.na(match.indices)],collapse=" "))
			stop("There were unrecognized field names in data.factor?")
		}
		intDataColsToRetrieve <- try(sapply(data.factors, function(x,y) eval(parse(text=x),envir=y), data.fields) )
		if ( is(intDataColsToRetrieve,"Error") ) {
			cat("An error was encountered upon attempting to subset the data slot of the cube.  \n")
			cat("Check that your variablesToRetrieve/variablesToDrop argument conforms to the ")
			cat("documented standard (see the online documentation).\n")
			stop(get.message(intDataColsToRetrieve,even.if.used=T))
		}	
		# if we want to drop these variables, invert the indices
		if ( bHaveVTD ) intDataColsToRetrieve <- seq(cube.dimen[2])[-intDataColsToRetrieve]
	} else { 
		intDataColsToRetrieve <- if ( bHaveVTR ) numeric(0) else seq(cube.dimen[2])
	}
  
	if ( verbose == 2 ) {
		cat("Retrieving variables: \n")
		print(data.fields[intDataColsToRetrieve])
		cat("\tAfter data variable subset: time: ",round(proc.time(),3), " mem usage(MB):",memory.size(),"\n")
	}

    ###############################################################################    
    # find columns to keep in the info slot
    ###############################################################################

    # field names for matching and expression validation
	if ( bHaveInfoSlot ) { 
		info.fields        <- as.list(seq(ncol(cube@info))) 
		names(info.fields) <- names(cube@info)
	} else info.fields <- list() 

	n.info.fields      <- length(info.fields)

	if ( length(info.factors) && n.info.fields ) {
		# try to match variable names against available fields
		match.indices <- match(info.factors, names(info.fields))
		if ( length(which(is.na(match.indices))) ) {
			# should never get here if computation of info.factors is correct
			cat(paste(info.factors[which(is.na(match.indices))],collapse=" "))
			stop("There were unrecognized field names in info.factor?")
		}
		intInfoColsToRetrieve <- try(unlist(lapply(info.factors, function(x,y) eval(parse(text=x),envir=y), info.fields)) )
		if ( is(intInfoColsToRetrieve,"Error") ) {
			cat("An error was encountered upon attempting to subset the info slot of the cube.  \n")
			cat("Check that your infoVariablesToRetrieve conforms to the documented standard (see the online ")
			cat("documentation.\n")
			stop(get.message(intInfoColsToRetrieve,even.if.used=T))
		}
		#print(intInfoColsToRetrieve)
		# if we want to drop these variables, invert the indices
		if ( bHaveVTD ) intInfoColsToRetrieve <- seq(n.info.fields)[-intInfoColsToRetrieve]
	} else { 
		intInfoColsToRetrieve <- if (bHaveVTR) numeric(0) else if (bHaveInfoSlot) seq(n.info.fields) else numeric(0)
	}
  
	if ( verbose == 2 ) {
		cat("Retrieving variables: \n")
		print(info.fields[intInfoColsToRetrieve])
		cat("\tAfter infoVariablesToRetrieve time: ",round(proc.time(),3), " mem usage(MB):",memory.size(),"\n")
	}

    ##############################################################################

	# drop any leftover NA's
	# should really do something about NA's, shouldn't happen.
	if ( verbose == 2 ) {cat("permIndices:\n");print(permIndices);cat("\n")}
  
	permIndices <- permIndices[!is.na(permIndices)]
	permIndices <- sort(permIndices)
	
	if ( bHaveInfoSlot ) {

		# subset by variables of interest
		info <- cube@info[intInfoColsToRetrieve]
		
		# make sure assetvar and datevar are of the proper form and are sorted by assets and dates
		# this should be enforced by construction
		#info[[assetvar]] <- ordered(info[[assetvar]], levels=sort(unique.default(info[[assetvar]])))

    #if ( !is(info[[datevar]],"Date") ) 
		#	info[[datevar]] <- as.Date(as.character(info[[datevar]]),format=cube@datefmt)
		
		info <- info[order(info[[assetvar]], info[[datevar]]),]		
		# subset by assets of interest
    info <- info[unlist(split(seq(nrow(info)), info[[assetvar]])[permIndices],use.names=F),]
        
		# drop unused factor levels
		#lv <- levels(info[[assetvar]])
		#cz <- as.integer( info[[assetvar]])
		#info[[assetvar]] <- ordered( lv[cz], levels=sort(unique.default(lv[cz])))
		
		if ( verbose == 2 ) {
			cat("\tAfter subsetting info slot by variables and assets...time: ",round(proc.time(),3), " mem usage(MB):",memory.size(),"\n")
		}
		
		if ( !bUseAllDates ) {
      
      makeInfoFunc <- function(xdf,start.date,end.date,dtfmt,dtv,asv) {  	

					num.start.date <- as.numeric(start.date)#; cat("num.start.date = ",num.start.date,"\n")
					num.end.date   <- as.numeric(end.date)  #; cat("num.end.date   = ",num.end.date  ,"\n")
					if ( num.end.date < num.start.date ) stop("start date must precede or be equal to end date.")
		
					dtvc <- xdf[[dtv]]
					asnm <- xdf[[asv]][1]
					if ( !is(dtvc,"Date") ) dtvc <- as.Date(as.character(dtvc),format=dtfmt)
					dtvc <- as.numeric(dtvc)
					
					#cat("dtvc start: ",dtvc,"\n")
					#cat("xdf  start: \n");print(xdf);cat("\n")
					# find start.date in the compressed date vector dtvc
					y <- which(dtvc <= num.start.date)#; cat("y = ",y,"\n")
					if ( length(y) == 0 ) {
						# start date occurs before first available date, so add it
						xdf <- rbind(rep(NA,ncol(xdf)),xdf)
						xdf[[dtv]][1] <- start.date
						xdf[[asv]][1] <- asnm
						dtvc <- c(num.start.date,dtvc)
					} else {
						# change date of latest record 
						z <- max(y)
						dtvc[z] <- num.start.date
						xdf[[dtv]][z] <- start.date
						# subset down so that start.date record is first record
						n <- seq(z,length(dtvc))
						dtvc <- dtvc[n]
						xdf  <- xdf[n,]
					}
					#cat("dtvc middle: ",dtvc,"\n")
					#cat("xdf  middle: \n");print(xdf);cat("\n")

					# find end.date in modified data frame
					y <- which(dtvc <= num.end.date)
					if ( length(y) == 0 ) {
						# case when end date occurs before all available dates
						# should never get here because we already checked that end 
						# date occurs on or after start date
						stop("end date occurs before start date---this should not be?")
					}
					# find the maximum value, and take up to that value
					z <- seq(1,max(y))
					#cat("dtvc end: ",dtvc[z],"\n")
					#cat("xdf  end: \n");print(xdf[z,]);cat("\n")
					xdf <- xdf[z,]			
					xdf
				}
        
        infoBy <- by(info, info[[assetvar]],FUN=makeInfoFunc,
				  start.date=dates.avail[datesToRetrieve[1]],
				  end.date=dates.avail[datesToRetrieve[length(datesToRetrieve)]],
				  dtfmt=dates.format,
				  dtv=datevar,
				  asv=assetvar,simplify=T)
        
			info <- do.call("rbind",infoBy)
			# end lapply
		} # end use all dates
		
	} else {
		info <- NULL
	}
	
	if ( verbose == 2 ) {
		cat("\tAfter subsetting info slot...time: ",round(proc.time(),3), " mem usage(MB):",memory.size(),"\n")
	}

	# 5/26/2005 important---need to regenerate the labels, not simply subset them
	# otherwise future lookups using the resulting cube could fail
	# as.character needed for factors
	labels <- if ( rebuildLabels && !is.null(labelvar) ) 
		tapply(as.character(info[[labelvar]]), info[[assetvar]], unique.default) else cube@labels
  labels <- as.list(labels)
    
	# code to output a cube
	retval <- new("fmdsCube.CRSP", 
					data=cube@data[datesToRetrieve,intDataColsToRetrieve,permIndices,drop=F], 
					labels=labels,
					info=info,
					auxData=cube@auxData,
					buildDate=as.Date(format(Sys.time(), "%Y-%m-%d")),
					datefmt=dates.format,
					matchfmt=match.format,
					index=cube@index,
					contents=cube@contents,
					labelvar=labelvar
					)
					# 5/26/2005 note to self need to subset contents properly

#     if ( match(out.format,"data.frame",nomatch=F) ) {
# 			# flatten
# 			retval <- flatten.fmdsCube(retval)
#     }
            
    return(retval)
}

