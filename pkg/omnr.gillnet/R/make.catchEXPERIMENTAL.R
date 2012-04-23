make.catchEXPERIMENTAL <-
function(mesh,lens,counts,binsize=20,strat=NULL){
	
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
		
		if(!is.null(strat)){
			strats <- unique(strat)
			meshlist <- lapply(seq_along(strats),function(i)mesh[strat==strats[i]])
			lenslist <- lapply(seq_along(strats),function(i)lens[strat==strats[i]])
		}
		else{
			meshlist <- list(mesh)
			lenslist <- list(lens)
			strats <- "only strat"
		}
			
		breaks <- fish.bins(lens,binsize)
		meshes <- sort(unique(mesh))

		n.bins <- length(breaks)-1
		n.meshes <- length(meshes)

		counts <- list()
		for(i in seq_along(strats)){
			counts[[i]] <- matrix(nrow=n.bins,ncol=n.meshes)
			for(k in 1:n.meshes){
				tmp.lens <- lenslist[[i]][meshlist[[i]]==meshes[k]]
				tmp.hist <- hist(tmp.lens,breaks=breaks,plot=FALSE)
				counts[[i]][,k] <- tmp.hist$counts
			}
		}
		
		names(counts) <- as.character(strats)
		output <- list(counts=counts,lens=tmp.hist$mids,mesh=meshes)
	
	}
	
	else{
		if(binsize!=20)warning("supplied binsize not used because counts is provided")
		if(length(lens)!=dim(counts)[1])stop("counts must have as many rows as elements in size")
		if(length(mesh)!=dim(counts)[2])stop("counts must have as many columns as elements in mesh")
		counts <- list(counts)
		output <- list(counts=counts,lens=lens,mesh=meshes)
	}
	
	class(output) <- "catch"
	return(output)

}


