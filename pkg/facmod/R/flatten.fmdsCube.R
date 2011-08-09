"flatten.fmdsCube" <-
#
#
# code to output a flattened data frame
#
# Christopher G. Green
# November 30, 2004
# revised April 2005
#
# along controls whether we flatten by asset or by date (i.e., group by assets or by dates)
#
function(cube,along="asset") {

  along <- casefold(along)
	if ( !match(along, c("asset","date"), F) ) stop("along must be either 'asset' or 'date'.")

  data.df <- as.data.frame(apply(cube@data,2,I))
  data.df <- cbind(data.df,DATE=rep(dimnames(cube@data)[[1]],dim(cube@data)[3]),
  PERMNO=rep(dimnames(cube@data)[[3]],each=dim(cube@data)[1]),stringsAsFactors=F)
  
  if( along=="date" )
  {
    idx <- order(data.df[,"DATE"],data.df[,"PERMNO"])
    data.df <- data.df[idx,]
  }

  datevar   <- cube@index$datevar
  assetvar  <- cube@index$assetvar
  datevar2  <- paste(datevar,"2",sep="")
	assetvar2 <- paste(assetvar,"2",sep="")

  # expand info slot
	# cgg: 4/16/05 again handle null info slots
	if ( !is.null(cube@info) && length(cube@info) ) {
		
		new.info <- expand.info.slot(cube) 
		# make sure names are correct
	
		# expand.info returns things grouped by asset
		# need to reorder if along=="date"
		if ( along == "date" ) new.info <- new.info[order(new.info[[datevar2]]), ]
	
		# join data and info parts, converting strings to factors to save space	
		# can drop superfluous DATE2 and PERMNO2 columns now
			
		superfluousCols <- match(c(datevar2,assetvar2),names(new.info))
	
		# if there are only two columns and their names are 'DATE2' and 'PERMNO2'
		# info slot didn't contribute anything new, so just return data.df
		if ( (ncol(new.info) == 2) && all(!is.na(superfluousCols)) ) {
			data.df
		} else {
			all.nas <- all(is.na(superfluousCols))
			# if there are no columns named datevar2 and assetvar2---malformed cube?
			out <- data.frame(data.df,
					if ( all.nas ) new.info else new.info[,-superfluousCols],
					row.names=NULL, 
					stringsAsFactors=F)
			# fix names
			names(out) <- c(names(data.df),
				if ( all.nas ) names(new.info) else names(new.info)[-superfluousCols])
			out
		}
	} else {
		# done, nothing else to do
		data.df
	}
}
