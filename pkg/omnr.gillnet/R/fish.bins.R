fish.bins <-
function(lens,binsize){
	
	#####################################################################
	#
	# fish.bins(): creates histogram bins suitable for use with
	#    make.catch()
	#
	# REQUIRES:  na
	# REQUIRED BY:  make.catch()
	#
	# lens: vector of fish lengths (e.g. fork length)
	# binsize: size of histogram bins with which to categorize fish
	# 	by their length
	# 
	# L,R: left/right boundary of histogram (these ensure that all of
	#    the fish lengths will be within the boundaries and that the
	#    maximum (minimum) sizes will be in the rightmost (leftmost)
	#    bins).  the binning strategy also ensures 'pretty' bins if
	#	a nice 'round' number is chosen for binsizes.
	#
	# output: a vector of the breakpoints between bins, suitable for
	#   inputing as the breaks argument in hist().
	#
	#####################################################################
	
	L <- floor(min(lens,na.rm=TRUE)/binsize)*binsize
	R <- ceiling(max(lens,na.rm=TRUE)/binsize)*binsize
	output <- seq(from=L,to=R,by=binsize)
	return(output)
}

