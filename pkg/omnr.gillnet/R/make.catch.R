make.catch <-
function(mesh, lens, counts, binsize=20, data.name = deparse(substitute(lens))){
	
	#######################################################################
	#
	# make.catch(): makes an object of class catch, which contains
	#             a fish-size bins by mesh sizes matrix of fish
	#             counts for use in size-selectivity analyses.
	#
	# REQUIRES: fish.bins()
	# REQUIRED BY: plot.catch(), fit.catch()
	#
	# mesh: a vector of mesh sizes or (if counts is not missing) a vector
	#       of unique mesh sizes corresponding to the columns of counts.
	# lens: a vector of fish lengths or (if counts is not missing) a vector
	#       of the mid-points of fish-length bins corresponding to the
	#       rows of counts.
	# counts: a fish-length bins by mesh sizes matrix of fish counts.  can
	#         be missing if mesh is a vector of mesh sizes and size is a
	#         vector of fish sizes.  if counts is missing, it is created
	#         with the information in mesh, size and binsize.
	# binsize: the number of bins into which fish lengths are categorised.
	#
	# breaks: vector of breakpoints between bins (uses fish.bins).
	# meshes: a sorted vector of the unique mesh sizes
	# n.bins: number of fish-length histogram bins
	# n.meshes: number of unique mesh sizes
	#
	# output: a list of class "catch" with three components:
	#		counts: an n.bins by n.meshes matrix of fish counts
	#		lens: a vector of the mid-point lengths corresponding to
	#			the rows of counts (note diff. between input lens)
	#		mesh: a vector of the mesh sizes corresponding to the
	#			columns of counts (note diff. between input mesh)
	#
	#######################################################################
	
	if((!is.vector(mesh))|(!is.vector(lens)))stop("mesh and size must be vectors")
	
	if(missing(counts)){
	
		if(length(mesh)!=length(lens))stop(
			"mesh and lens must have the same number of entries when counts is missing"
		)
		if(binsize<1)stop("binsize must be positive")
			
		breaks <- fish.bins(lens,binsize)
		meshes <- sort(unique(mesh))

		n.bins <- length(breaks)-1
		n.meshes <- length(meshes)

		counts <- matrix(nrow=n.bins,ncol=n.meshes)
	
		for(k in 1:n.meshes){
			tmp.lens <- lens[mesh==meshes[k]]
			tmp.hist <- hist(tmp.lens,breaks=breaks,plot=FALSE)
			counts[,k] <- tmp.hist$counts
		}
		
		output <- list(counts=counts,lens=tmp.hist$mids,mesh=meshes,data.name=data.name)
	
	}
	
	else{
		if(binsize!=20)warning("supplied binsize not used because counts is provided")
		if(length(lens)!=dim(counts)[1])stop("counts must have as many rows as elements in size")
		if(length(mesh)!=dim(counts)[2])stop("counts must have as many columns as elements in mesh")		
		output <- list(counts=counts,lens=lens,mesh=meshes,data.name=data.name)
	}
	
	class(output) <- "catch"
	return(output)

}

