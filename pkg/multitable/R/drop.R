dropdl <- function(x){
	
	# figure out what needs to be dropped
	dx <- dim(x)
	todrop <- dx==1
	
	# if there's nothing to drop, just return x
	if(all(!todrop)) return(x)
	
	# figure out the new dimids, which are needed to figure out
	# the new match.dimids (below)
	attrx <- attributes(x)
	dimids <- attrx$match.dimids[[attrx$bm]][!todrop]
	
	# drop dimensions and variables that lost their dimensions
	x <- lapply(unclass(x),dropdla,todrop)
	x <- dropl(x)	# drop null variables (i.e. that lost all dims)
	
	# figure out the new match.dimids
	match.dimids <- lapply(x, function(a) dimids[attr(a,"subsetdim")])
	names(match.dimids) <- names(x)
	
	# adjust the attributes
	attr(x,"bm") <- multitable:::which.fully.replicated(x)
	attr(x,"match.dimids") <- match.dimids
	attr(x,"repdim") <- attrx$repdim[!todrop]
	class(x) <- "data.list"

	return(x)
}

# a drop function for the variables in an unclassed data list.
# this is primarily a utility function for dropdl.
# dla stands for data list array
dropdla <- function(a,todrop){

	# adjust the subsetdim attribute
	ssd <- attr(a,"subsetdim")
	attr(a,"subsetdim") <- attr(a,"subsetdim")[!todrop]
	
	# if there's nothing to drop, just return a
	if(all(!todrop[ssd])) return(a)

	# treat one-dimensional variables differently from...
	else if(sum(ssd)==1) return(NULL)

	# ...matrices and arrays
	else return(drop(a))
}

# list drop -- drops null list elements
dropl <- function(x) x[!sapply(x,is.null)]