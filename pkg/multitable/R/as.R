is.data.list <- 
function(x) inherits(x, "data.list")

as.data.list <-
function(x, ...) UseMethod("as.data.list")

as.data.list.data.list <- 
function(x, ...) return(x)

as.data.list.default <-
function(x, dimids, match.dimids, check = TRUE, drop = TRUE, ...){

	# x really needs to be a list!
	if((!is.list(x))||is.data.frame(x)) x <- list(x)

	# the make.match.dimids function is an algorithm
	# for automatically generating the pattern of
	# dimension sharing between the objects to be
	# combined into a data list.
	if(missing(match.dimids)) match.dimids <- make.match.dimids(x, dimids)
		
	if(!is.list(match.dimids)) stop("match.dimids must be a list")
	if(length(x) != length(match.dimids)) stop(
"match.dimids must have the same
number of elements as the list of
objects to be combined into a data
list")
	if(check.full.rep(match.dimids) && check) stop(
"at least one variable must be
replicated along all dimensions")

	if(check.unique.dimids(match.dimids) && check) stop(
"the dimensions of replication for
each variable must be different
from each other")
	
	# some elements of x may themselves be lists of
	# variables (e.g. data frames). but data lists are
	# lists of atomic vectors, matrices, and arrays.
	# therefore, we must first `split up' these lists
	# within x into their constituent variables (i.e.
	# their variables must become elements in x just 
	# like all of the others). the split.dfs performs
	# this spliting. it then combines the result with
	# the match.dimids list in a two-element list of
	# lists (1) x and (2) match.dimids. 
	x <- split.dfs(x, match.dimids)
	
	# one of the rules of data lists is that each data
	# list must have at least one variable that is
	# replicated along all dimensions. we store the
	# index of this variable in bm (for benchmark) so
	# that we can compare the other variables with it.
	# the benchmark `holds together' the other
	# variables.
	bm <- which.fully.replicated(x$x)
	
	# the dimensions of a data list (or repdim's) are
	# simply the dimensions of the benchmark variable.
	repdim <- dim(x$x[[bm]])
	names(repdim) <- x$match.dimids[[bm]] # fixed a bug here by the 'x$'
										  # previously, for data with a
										  # 'late' benchmark variable
										  # without dimnames, 
										  # match.dimids was an 'older'
										  # version of x$match.dimids
	
	# all data list variables MUST have names, so if
	# you won't do it...i will. its just good practice
	# to name your variables. and the check.dims function
	# that comes next requires it in order to give 
	# informative error messages.
	if(is.null(names(x$x))) names(x$x) <- paste("V", seq_along(x$x), sep="")

	# if match.dimids have been specified such that 
	# the lengths of matched dimensions are not 
	# identical, then an invalid data list will result.
	# the check.dims function will give an informative
	# error message about which variable might be
	# causing such a problem.
	if(check) check.dims(x, bm, repdim)

	# all variables in a data list must have an 
	# attribute called "subsetdim", which is a
	# logical vector the same length as repdim
	# that has TRUE for dimensions along which
	# it is replicated and FALSE otherwise. the
	# subsetdim function adds this attribute.
	x <- subsetdim(x, bm, repdim)
	
	# split up match.dimids and x and prepare them
	# for output.
	match.dimids <- x$match.dimids
	names(match.dimids) <- names(x$x)
	x <- structure(x$x, bm = bm, match.dimids = match.dimids,
		repdim = repdim, class = "data.list")
	
	# it is important to make sure that all variables
	# have the same dimnames for shared dimensions.
	x <- make.dimnames.consistent(x, bm)
	
	# data lists with only one dimension of replication
	# are conceptually equivalent to data frames.
	# therefore, the default behaviour is to formally
	# convert such data lists to data frames but this
	# behaviour can be changed via the `drop' argument.
	if((length(repdim) == 1) && drop) return(as.data.frame(x))

	return(x)
}

make.match.dimids <- function(x, dimids){
	# the purpose of this function is to figure out
	# how the elements in x are related if the
	# relationships are not specified explicitly in 
	# match.dimids.
	#
	# the basic outline of the algorithm is:
	# 1. check if the dimensions of the objects in x
	# 	 are fully named.
	# 2a.if they are, try to match them based on their
	#	 names.
	# 2b.if they aren't, try to match them based on
	#	 their lengths.
	# 3. if a fully replicated match.dimids doesn't
	#    result, then try 2b one more time.
	
	match.dimids <- list()
	
	check <- FALSE
	
	# 1. create a list of the dimnames in x.
	# we use the utility function get.input.names
	# because it returns the right kind of 'dim'
	# name for each kind of object in x. different
	# object types in R have different ways of 
	# retrieving the names associated with it.
	# see the comments for get.input.names
	# for more detail. 
	innames <- lapply(x, get.input.names)
	ulinnames <- unlist(innames, recursive=FALSE)
	
	# get a logical vector indicating which elements
	# in x have non-null dimnames.
	#notnullnames <- !sapply(innames,is.null) # think this line is a bug...
	#notnullnames <- !sapply(ulinnames,is.null) # that this line fixes???

	# above still buggy so new attempt
	notnullnames <- !(any(sapply(innames,is.null)) || any(sapply(ulinnames,is.null)))
	
	#if(all(notnullnames) && !is.null(ulinnames)){
	# fixed a BUG here:  the !is.null(ulinnames) is not necessary,
	# because ulinnames will never have a NULL first element (b/c
	# unlist removes NULL elements).  the new condition is better
	# because if the length of x is different than the length of 
	# ulinnames, this indicates that either unlist (when creating
	# ulinnames) or get.input.names (when creating innames) has 
	# dropped some elements and this means that some of the innames 
	# were NULL, which is what we want to avoid in this condition.
	if(notnullnames && (length(x) == length(ulinnames))){ # removed all() around notnullnames...now handled in the defnition of notnullnames
		# 2a. this condition is evaluated when the dimensions
		# of the elements in x are 'fully named'
		# therefore, this is where the algorithm tries
		# to match dimnames
		
		# the first thing to be done is to check that there
		# is a comparison to make (i.e. the dimnames are not
		# all unique and there is more than one variable)
		unique.dimnames <- unique(ulinnames)
		allunique <- length(unique.dimnames) == length(ulinnames)
		morethanonevar <- length(x) > 1
		if(allunique && morethanonevar)
			stop(
"Resulting data list invalid:
some variables do not share any
dimensions with other variables")

		# create a list with one element for each in x,
		# with vector elements giving indices for each of
		# the dimensions associated with that element.
		# for example, for two elements such that the first
		# has two dimensions and the second shares the first
		# of those two, we would have something like:
		# 	$A
		# 	[1] 1 2
		# 	$B
		# 	[1] 1
		mat.ndims <- lapply(innames, match, unique.dimnames)

		# now create a vector of the sizes of the indexed
		# dimensions in mat.ndims (wfr stands for 'which
		# fully replicated' because the dimensions overall
		# are equal to the dimensions of a fully replicated
		# variable -- e.g. benchmark variable)
		indims.wfr <- sapply(unique.dimnames, length)

		# check that the dimensions of each element in x 
		# are fully replicated, when its time to do so
		check <- TRUE
	}
	else{
		# 2b. if this condition is evaluated then dimension
		# matching is attempted using the lengths of the
		# dimensions of the elements in x.
		 
		# get the dimensions of the elements in x
		indims <- lapply(x, get.input.dims)
		
		# find out which one is fully replicated (wfr)
		wfr <- which.max(sapply(indims, length))
		
		# get a vector of the sizes of the data list 
		# dimensions (compare with indims.wfr in the 
		# previous condition)
		indims.wfr <- indims[[wfr]]
		
		# if some dimensions appear to have an identical
		# length, then it is impossible to decide how to
		# match them.
		if(length(unique(indims.wfr)) < length(indims.wfr)){
			stop(
"Some dimensions are unnamed and some are 
of the same length and therefore require
specification of match.dimids. Type
?data.list and see the details section
of the help file for data.list.")
		}
		
		# see the explanation above for mat.ndims
		mat.ndims <- lapply(indims, match, indims.wfr)
		
		# set check to FALSE so that length-based matching
		# is only tried one time
		check <- FALSE
	}
	
	# make sure the dimension ids (dimids) exist
	if(missing(dimids)) dimids <- paste("D", seq_along(indims.wfr), sep="")
	
	# see ?as.data.list for definition of match.dimids
	match.dimids <- lapply(mat.ndims, function(ii) dimids[ii])
	
	# make sure that match.dimids is fully replicated.
	# if not, the condition will be evaluated, which
	# recursively calls make.match.dimids and tries
	# one more time to match the dimids
	if(check.full.rep(match.dimids) && check){
		names(x[[1]]) <- dimnames(x[[1]]) <- NULL
		match.dimids <- make.match.dimids(x, dimids)
	}
	
	##### possible paths through the algorithm: #####
	#
	# 1. try matching by names, but fail with error
	# 2. try matching by names, but get non-fully-replicated
	#    output, then try matching by lengths but fail with error
	# 3. try matching by names, but get non-fully-replicated
	#    output, then try matching by lengths and succeed
	# 4. try matching by names, and succeed
	# 5. try matching by lengths, but fail with error
	# 6. try matching by lengths, and succeed
	#
	#################################################
	
	return(match.dimids)
}

get.input.dims <- function(xi){

	if(is.null(dim(xi)) & is.atomic(xi))
		return(length(xi))

	else if(is.recursive(xi) & is.atomic(xi[[1]])){
		
		if(is.null(dim(xi[[1]])))
			return(length(xi[[1]]))
		else
			return(dim(xi[[1]]))

	}

	else if(is.recursive(xi))
		stop(
"recursive tables must contain only
atomic elements")
	
	else
		return(dim(xi))

}

get.input.names <- function(xi){
	# this function returns the `dim` names assocaited with
	# xi.  different types of objects have different types
	# of names, stored in attributes.  for:
	# data frames: list with one element containing row.names
	# atomic vector: list with one element containing names
	# matrix or array: dimnames
	# anything else: NULL, which will cause errors with bad messages
	# (non-data frame) list: work with the first element and extract
	#						 its names
	
	if(is.data.frame(xi))
		return(list(rownames(xi)))
		
	else if(is.null(dim(xi)) & is.atomic(xi))
		return(list(names(xi)))
	
	else if(is.recursive(xi) & is.atomic(xi[[1]])){
		
		if(is.null(dim(xi[[1]])))
			return(list(names(xi[[1]])))
		else
			return(dimnames(xi[[1]]))
	}
	
	else if(is.recursive(xi))
		stop(
"recursive tables must contain
only atomic elements")
	
	else return(dimnames(xi))	
}

check.full.rep <-
function(match.dimids){
	# return FALSE if match.dimids contains a fully replicated
	# (e.g. benchmark) variable and TRUE otherwise.
	
	mt <- match.dimids[[which.max(sapply(match.dimids, length))]]
	dims <- lapply(match.dimids, match, table = mt)
	any(sapply(dims, function(x) any(is.na(x))))
}

check.unique.dimids <- 
function(match.dimids) # return FALSE if no duplicated dimids
	any(unlist(lapply(match.dimids, duplicated)))

split.dfs <-
function(x, match.dimids){
	
	# altered x and match.dimids
	x.alt <- list()
	match.dimids.alt <- list()
	
	# loop over all data frames in x
	for(i in seq_along(x)){
		
		# all x[[i]] must be lists...
		if(!is.list(x[[i]])){
			x[[i]] <- list(x[[i]])
			names(x[[i]]) <- names(x)[i]
		}
		
		# ...and if they don't have names, name them
		else if(is.null(names(x[[i]])))
			names(x[[i]]) <- paste(names(x)[i], seq_along(x[[i]]), sep = ".")
		
		# loop over all elements of these lists
		for(j in seq_along(x[[i]])){
			
			if(length(x[[i]][[j]])==0)
				stop(
"variables with zero length are
not allowed in data lists")
			
			# save the dimensions and dimnames of the variables
			# to be added back if necessary
			dx <- dim(x[[i]][[j]])
			if(is.null(dx)) dnx <- names(x[[i]][[j]])
			else dnx <- dimnames(x[[i]][[j]])
			
			# convert characters to factors
			if(is.character(x[[i]][[j]])){
				x.attr <- attributes(x[[i]][[j]])
				which.other.attr <- !(names(x.attr) %in% c("dim", "levels", "dimnames", "class"))
				other.attr <- x.attr[which.other.attr]
				x[[i]][[j]] <- as.factor(x[[i]][[j]])
				dim(x[[i]][[j]]) <- dx
				
				# try to keep attributes (see aperm.factor for similar approach)
				for(ii in seq_along(other.attr))
					attr(x[[i]][[j]], names(other.attr)[[ii]]) <- other.attr[[ii]]
			}
			
			# put names (and maybe dimensions) back
			if(is.null(dx)){
				attr(x[[i]][[j]], "dim") <- length(x[[i]][[j]])	
				names(x[[i]][[j]]) <- dnx
			}
			else dimnames(x[[i]][[j]]) <- dnx
		
		}
		
		# grow the new data and their match.dimids
		match.dimids.alt <- c(match.dimids.alt, rep(list(match.dimids[[i]]), length(x[[i]])))
		x.alt <- c(x.alt, x[[i]])
	}
	
	return(list(x = x.alt, match.dimids = match.dimids.alt))
}

which.fully.replicated <-
function(x) which.max(sapply(x,function(xi)length(dim(xi))))

check.dims <-
function(x, bm, repdim){
	rdn.bm <- x$match.dimids[[bm]]
	for(i in seq_along(x$x)[-bm]){
		for(j in seq_along(x$match.dimids[[i]])){
			repdimij <- repdim[rdn.bm == x$match.dimids[[i]][j]]
			dimij <- dim(x$x[[i]])[j]
			if(repdimij != dimij) stop(paste("incompatible dimensions in", names(x$x)[i]))
		}
	}
}

subsetdim <-
function(x, bm, repdim){
	
	inds <- lapply(x$match.dimids, match, table = x$match.dimids[[bm]])
	
	for(i in seq_along(x$x)){
		ord.inds <- order(inds[[i]])
		notfactor <- !is.factor(x$x[[i]])
		if(notfactor){ # try to keep attributes (see aperm.factor for similar approach)
			x.attr <- attributes(x$x[[i]])
			which.other.attr <- !(names(x.attr) %in% c("dim","levels","dimnames","class"))
			other.attr <- x.attr[which.other.attr]
		}
		x$x[[i]] <- aperm(x$x[[i]], ord.inds)
		if(notfactor) # try to keep attributes (see aperm.factor for similar approach)
			for(ii in seq_along(other.attr))
				attr(x$x[[i]], names(other.attr)[[ii]]) <- other.attr[[ii]]
		x$match.dimids[[i]] <- x$match.dimids[[i]][ord.inds]
	}
	inds <- lapply(x$match.dimids, match, table = x$match.dimids[[bm]])
	nd <- length(repdim)
	for(i in seq_along(x$x)){
		attr(x$x[[i]], "subsetdim") <- rep(FALSE,nd)
		attr(x$x[[i]], "subsetdim")[inds[[i]]] <- TRUE
		names(attr(x$x[[i]], "subsetdim")) <- x$match.dimids[[bm]]
	}
	return(x)
}

make.dimnames.consistent <-
function(x,bm){
	dimnames(x) <- dimnames(bm(x))
	if(is.null(dimnames(x))) dimnames(x) <- lapply(dim(x), function(di) seq_len(di))
	return(x)
}

as.list.data.list <-
function(x, drop.attr=TRUE, factorsTOstrings=FALSE, ...){
	if(factorsTOstrings)
		out <- lapply(x,function(xx)
			if(is.factor(xx)) as.character(xx) else xx)
	else out <- unclass(x)
	if(drop.attr){
		for(i in seq_along(x)) attr(out[[i]], "subsetdim") <- NULL
		attr(out, "match.dimids") <- NULL
		attr(out, "bm") <- NULL
		attr(out, "repdim") <- NULL
	}
	return(out)
}

as.data.frame.data.list <-
function(x, row.names = NULL, optional = FALSE, scheme = "repeat", mold, ...){
	if(missing(mold)) mold <- data.list.mold(x)
	#out <- lapply(seq_along(x), function(i) as.vector(x[[i]][mold[[i]]]))
	out <- lapply(seq_along(x), function(i) 
		structure(x[[i]][mold[[i]]], dim = NULL))
	names(out) <- varnames(x)
	if(is.null(row.names)) row.names <- attr(mold, "df.rownames")
	as.data.frame(out, row.names = row.names, optional = optional,...)
}

as.matrix.data.list <- 
function(x, ...){
	fac <- mapply("||", sapply(x, is.factor), sapply(x, is.character))
	if(all(!fac)||all(fac))
		return(as.matrix(as.data.frame(x)))
	x.attr <- attributes(x)
	x.df <- as.data.frame(x)
	x.num <- x.df[!fac]
	x.fac <- x.df[fac]
	list(x.num = as.matrix(x.num), x.fac = as.matrix(x.fac))
}

data.list.mold <-
function(x){
	df.rownames <- as.vector(mouter(dimnames(x), FUN=paste, sep = "."))
	# an alternative to the above line (that reverses the order of the 
	# dimensions) is:
	# df.rownames <- big.kronecker(rev(dimnames(x)),FUN=paste,sep=".")
	
	repdims <- dim(x)
	xmold <- list()
	for(i in seq_along(x)){
		xmold[[i]] <- seq_along(x[[i]])
		dms <- attr(x[[i]], "subsetdim")
		prms <- c(which(dms), which(!dms))
		xmold[[i]] <- aperm(array(xmold[[i]], repdims[prms]), order(prms))
		dim(xmold[[i]]) <- NULL
	}
	structure(xmold, df.rownames = df.rownames)
}
