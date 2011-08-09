"expand.info.slot" <- 
#
# Christopher G. Green, April 2005
# helper function for various methods (like flatten and screening)
# uncompresses the info slot into a data frame
#
# cube  		cube object 
#
function(cube) {

	# dimensions of the cube
  cube.dimen  <- dim(cube@data)

	# names from the data slot
	cube.dimnames <- dimnames(cube@data)	
	
	dates.format <- cube@datefmt
	match.format <- cube@matchfmt	
	
  # available dates
  dates.avail <- as.Date(cube.dimnames[[1]])

	# date and asset variables
	datevar  <- cube@index$datevar
	assetvar <- cube@index$assetvar
	
	info <- cube@info

	# reshape list of data frames to single data frame	
	# cgg: 4/16/05 null info slots!
	if ( !is.null(info) && length(info) ) {		
			
		# safety check
		if ( !all(match(c(datevar,assetvar),names(info),F)) )
			stop("Malformed cube: column(s) corresponding to one or both of the index variables is/are missing.")

		# ensure correct sorting
		info <- info[order(info[[assetvar]], info[[datevar]]),]
		
		#cat("First thirty or less rows of info:\n")
		#print(info[1:min(nrow(info),30),])

		# change date format for matching
		#info[[datevar]]@format <- match.format

		assetvar.data <- info[[assetvar]]
		
		# try to match against available dates
		dates.have.indices <- match(as.character(info[[datevar]]), as.character(dates.avail))
		
		if ( length(which(is.na(dates.have.indices))) )
			stop("There were dates in info slot did not match dates in data slot---improperly built cube?")
			
		#cat("dates.have.indices:\n")			
		#print(dates.have.indices)

		# dates.have.indices gives indices into dates.avail of dates 
		# for each asset's data, we need to the rows of the info structure 
		# to match the dates 
		#
		# if the first date available in the data frame occurs after the 
		# first available date, we don't back fill; rather, those will be filled
		# with NA's to indicate that those values are missing
		#
		# now dfr[dtwind, ] would be the expanded info structure (wrong dates though)
		# dtwind[dtkp] subsets this down to match data slot of cube
			
		dates.wanted.indices <- tapply(dates.have.indices, assetvar.data, 
			function(x,n) rep(c(NA,seq(along=x)),diff(c(1,x,n+1)))[1:n], length(dates.avail))
		#cat("dates.wanted.indices\n")
		#print(dates.wanted.indices)
		
		# create indices for replicating each asset's data
		rowind <- unlist(lapply(seq(along=dates.wanted.indices), function(i,a,b) a[[i]][b[[i]]], 
			a=split(seq(nrow(info)),assetvar.data),
			b=dates.wanted.indices))
			
		#print(rowind)
		
		# set duplicate row names attribute for a moment
		attr(info,"dup.row.names") <- T
		info <- info[rowind,]
		
		# fix dates
    julian=unlist(
      tapply(X=info[[datevar]], INDEX=info[[assetvar]], 
		    FUN=function(x,d) as.numeric(d), dates.avail, simplify=F)
    )
		info[[datevar]] <- as.Date(julian,origin="1970-01-01")
		
		# change date format back
		#info[[datevar]]@format <- dates.format
		
		# fix names		
		# use DATE2 so as not to conflict with DATE (from data slot when as.data.frame is used)
		otn <- names(info)
		otn[otn==datevar ] <- paste(datevar,"2",sep="") 
		otn[otn==assetvar] <- paste(assetvar,"2",sep="")
		names(info) <- otn
		
		# fix row names
		attr(info, "row.names") <- as.character(seq(nrow(info)))
		attr(info, "dup.row.names") <- F
	}
	
	info
}
