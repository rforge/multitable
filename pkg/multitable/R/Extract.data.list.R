`[.data.list` <-
function(x, ..., drop=TRUE, vextract=TRUE){
	
	mc <- match.call()
	
	x <- make.dimnames.consistent(x, attr(x, 'bm'))

	# obtain the correct number, nd, of dimension indices passed to ...
	# this involves not counting drop and vextract arguments if they
	# were specified.
	nmc <- length(mc)
	which.dv <- c("drop","vextract") %in% names(mc)
	if(any(which.dv)){
		ndv <- sum(which.dv)
		mc[(nmc-ndv+1):nmc] <- NULL
		nmc <- nmc - ndv
	}
	nd <- nmc - 2
	
	# return unmodified x if no subscripts are passed and check for
	# the passing of a single matrix (not allowed for data lists).
	if(nd==1){
		if(mc[[3]] == bquote()) return(x)
		if(is.matrix(eval(mc[[3]],envir=parent.frame())))
			stop("subscripting data lists with matrices is currently not allowed, but this may change in the future")
	}
	
	###################################
	## list-like (variable) extraction
	###################################
	if((nd==1) && vextract){
		
		# obtain subscript and determine its mode
		ss <- eval(mc[[3]],envir=parent.frame())
		mode.ss <- mode(ss)
		
		if(mode.ss=="numeric"){
			nv <- nvar(x)
			rng <- range(ss)
			if(((rng[2] > nv) || (rng[1] < 1)) && (rng[1] >= 0))
				stop("subscript out of bounds")
		}
		else if(mode.ss=="character"){
			vn <- varnames(x)
			if(!all(ss %in% vn))
				stop("variable name(s) not found")
		}
		else if(mode.ss!="logical")
			stop("invalid subscript type")
		
		# unclass so that variables can be extracted
		# using the method for lists
		xl <- unclass(x)
		match.dimids <- attr(xl,"match.dimids")[ss]
		xl <- xl[ss]
		
		xdl <- as.data.list(xl,match.dimids=match.dimids,drop=drop)
		return(xdl)
	}
	
	####################################################
	## array-like (dimension of replication) extraction
	####################################################
	repdim <- dim(x)
	if(nd!=length(repdim)) stop("incorrect number of dimensions")
	dim.names <- dimnames(bm(x))
	
	# loop over each dimension
	for(i in 3:nmc){
		
		# NULL subscripting not allowed
		if(is.null(mc[[i]]))
			stop("NULL subscripting is not allowed in data lists")
		
		# if subscripts are specified...
		else if(all(mc[[i]] != bquote())){  # the all() call here is experimental
			
			# get the subscript indices
			indi <- eval(mc[[i]],envir=parent.frame())
			
			indi.logical <- is.logical(indi)
			if(indi.logical){
				# logical vectors of indices that don't match
				# the length of the dimension, get their elements
				# recycled (in accordance with standard array-
				# like subscripting rules)
				ndm <- length(dim.names[[i-2]])
				indi <- suppressWarnings(
					as.logical(indi + rep(0,ndm))
				)
				if(all(!indi))
					stop("variables with zero length are not allowed in data lists")
			}
			if(is.character(indi)){
				# character index vectors are converted to
				# numeric, because it works even if the only
				# variable with dimnames is the benchmark
				# (i.e. its safer this way)
				indi <- match(indi,dim.names[[i-2]])
				if(!all(complete.cases(indi))){
					stop("supplied names not found in appropriate dimensions")
				}
			}
			if(is.numeric(indi) && any(abs(indi)<1))
				stop("zero subscripting not allowed in data lists")
			
			# update the replication dimensions
			if(any(sign(indi) == -1))
				repdim[i-2] <- dim(x)[i-2] - length(indi)
			else if(indi.logical) repdim[i-2] <- sum(indi)
			else repdim[i-2] <- length(indi)
			
			if(repdim[i-2]==0) stop("some replication dimensions have been reduced to zero length and this is not allowed")
			
			# save new subscript indices
			mc[[i]] <- indi
			dim.names[[i-2]] <- dim.names[[i-2]][indi]
		}
		
		# if no subscripts specified, just return the full unchanged dimension
		else mc[[i]] <- 1:repdim[i-2]
	}
	
	# create building blocks for the function calls that will
	# be used to extract from each variable
	mc1 <- list(`[`,bquote(xi))
	mc2 <- as.list(mc)[3:nmc]

	x <- unclass(x)
	
	# subscript each variable one-at-a-time
	for(i in seq_along(x)){
		xi <- x[[i]]
		dsi <- attr(xi,"subsetdim")
		mci <- as.call(c(mc1,mc2[dsi]))
		mci$drop <- FALSE
		x[[i]] <- eval(mci)
		attr(x[[i]],"subsetdim") <- dsi
	}
	
	attr(x,"repdim") <- repdim
	class(x) <- "data.list"
	return(x)
}

`$<-.data.list` <- function(x,i,value){
	nx <- names(x)
	if(!any(nx %in% i))
		stop("can't add variables this way...try using [[ instead of $...and don't forget to specify match.dimids or shape")
	
	# removing variables by replacing with NULL
	if(is.null(value)){
		ni <- which(nx==i)
		return(x[-ni])
	}
	
	# best to work with x as a list, rather than data list
	# because the base extraction methods are faster and
	# avoid recursive calling of multitable extraction methods
	# which makes debugging harder
	xl <- unclass(x)
	
	
	if(length(xl[[i]])!=length(value))
		stop("length of replacement value does not
			match length of the variable to be replaced")
	
	# treat factors specially to ensure that levels in the 
	# replacement value that are absent in the current value
	# are included properly
	if(is.factor(xl[[i]])){
		levels.in <- levels(xl[[i]])
		levels.add <- as.character(unique(value))
		levels(xl[[i]]) <- union(levels.in,levels.add)
	}
	
	# treat characters specially by converting them into
	# a factor, and preserving their dimensions (which is
	# a bit ugly since as.factor removes dimensions)
	if(is.character(value)){
		dimvalue <- dim(value)
		value <- as.factor(value)
		dim(value) <- dimvalue
	}
	
	# retain the appropriate attributes for the replaced
	# variable
	attr.xl <- datalistVARattributes(xl[[i]])
	xl[[i]] <- value
	datalistVARattributes(xl[[i]]) <- attr.xl
	
	class(xl) <- "data.list"
	return(xl)
}

`[[<-.data.list` <- function(x,i,match.dimids,shape,drop=TRUE,value){
	nx <- names(x)
	if(is.character(i)){
		
		# adding a new variable:
		if(!any(nx %in% i)){
			x <- unclass(x)
			if(missing(match.dimids)){
				if(missing(shape))
					stop("match.dimids or shape required for this assignment")
				else
					match.dimids <- attr(x,"match.dimids")[[shape]]
					if(is.null(match.dimids)) stop("shape did not match any existing variable")
			}
			match.dimids <- c(attr(x,"match.dimids"),list(match.dimids))
			x[[i]] <- value
			x <- as.data.list(x,match.dimids=match.dimids)
			return(x)
		}
	}
	else if(is.numeric(i)){
		if((i > length(x)) || (i < 1))
			stop("subscript out of bounds")
	}
	else stop("invalid subscript type")

	# removing variables by replacing with NULL
	# slightly more complicated then with $<-.data.list
	# because of the possibility of numeric i
	if(is.null(value)){
		if(is.character(i)) ni <- which(nx==i)
		else if(is.numeric(i)) ni <- i
		return(x[-ni,drop=drop])
	}
	
	# the rest is identical to $<-.data.list
	xl <- unclass(x)
	if(length(xl[[i]])!=length(value))
		stop("length of replacement value does not match length of the variable to be replaced")
	if(is.factor(xl[[i]])){
		levels.in <- levels(xl[[i]])
		levels.add <- as.character(unique(value))
		levels(xl[[i]]) <- union(levels.in,levels.add)
	}
	if(is.character(value)){
		dimvalue <- dim(value)
		value <- as.factor(value)
		dim(value) <- dimvalue
	}
	attr.xl <- datalistVARattributes(xl[[i]])
	xl[[i]] <- value
	datalistVARattributes(xl[[i]]) <- attr.xl
	class(xl) <- "data.list"
	return(xl)
}

# the purpose of this function (by Hadley Wickham) is to test if 'try' found an error
is.error <- function(x) inherits(x, "try-error")

# returns only those attributes required by variables in a data list
datalistVARattributes <- function(dlvar){
	list(
		dim=attr(dlvar,"dim"),
		dimnames=attr(dlvar,"dimnames"),
		subsetdim=attr(dlvar,"subsetdim")
	)
}

`datalistVARattributes<-` <- function(dlvar,value){
	# maybe in the future put a warning or error if these attributes don't match somehow
	attr(dlvar,"dim") <- value$dim
	attr(dlvar,"dimnames") <- value$dimnames 
	attr(dlvar,"subsetdim") <- value$subsetdim
	return(dlvar)
}
