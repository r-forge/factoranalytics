"scaleNumericFactors.fmdsCube" <- 
#
# Christopher G. Green, April 2005
#
# rescale the data slot of an fmdsCube object
# each exposures is scaled to have mean 0 and standard deviation 1 for each month
# unless robust.location=T, in which case we subtract off a robust location estimate
# (location.m)
# unless robust.scale=T, in which case we divide by a robust scale (scale.tau)
#
# based on Eric Aldrich's scaleCube function
#
function(cube, robust.location=F, robust.scale=F, na.rm=T, robust.location.args=NULL, robust.scale.args=NULL) {
  
	if ( !is(cube,"fmdsCube") ) stop("Input must be an fmdsCube object.")

	x <- cube@data
			
	array.dims <- dim(x)
	# compute location estimate for each column
	# output will have dimensions array.dims[1] by array.dims[2]
	# 5/25/2005 --- if there are +/- Inf's in cube we must remove them, otherwise mean will return -Inf.
	# location.m is not affected by this, as it handles Inf's better, but Inf's will of course
	# change the estimate
	locest <- if (robust.location) apply(x,c(1,2),function(x) huberM(x)$mu) else 
					apply(x,c(1,2),function(x,na.rm) mean(x[is.finite(x)],na.rm=na.rm), na.rm=na.rm)
					
	# now remove the location estimates
	x <- x - array(rep(locest,array.dims[3]),dim=array.dims)
	
	# compute scale estimates
	# stdev will return NA if any entries in the input are -Inf
	# scale.tau can handle -Inf
	scalest <- if (robust.scale) apply(x,c(1,2), scaleTau2, na.rm=na.rm) else 
		apply(x,c(1,2), function(x,na.rm) sd(x[is.finite(x)],na.rm=na.rm), na.rm=na.rm)
		
	# 5/25/2005
	# scalest could be zero---if all nonmissing values are constant
	scalest[scalest==0] <- 1
	
	# divide by scale estimates
	x <- x / array(rep(scalest,array.dims[3]),dim=array.dims)

	cube@data <- x
	cube
}		
