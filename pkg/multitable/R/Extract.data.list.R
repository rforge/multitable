`[.data.list` <-
function(x,...,drop=TRUE,vextract=TRUE){
	mc <- match.call()
	nmc <- length(mc)
	which.dv <- c("drop","vextract") %in% names(mc)
	if(any(which.dv)){
		ndv <- sum(which.dv)
		mc[(nmc-ndv+1):nmc] <- NULL
		nmc <- nmc - ndv
	}
	nd <- nmc - 2
	if(nd==1){
		if(mc[[3]] == substitute()) return(x)
		if(is.matrix(eval(mc[[3]],envir=parent.frame())))
			stop("subscripting data lists with matrices is currently not allowed, but this may change in the future")
	}
	if((nd==1) && vextract){
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
		xl <- unclass(x)
		match.dimids <- attr(xl,"match.dimids")[ss]
		xl <- xl[ss]
		xdl <- as.data.list(xl,match.dimids=match.dimids,drop=drop)
		return(xdl)
	}
	repdim <- dim(x)
	if(nd!=length(repdim)) stop("incorrect number of dimensions")
	dim.names <- dimnames(bm(x))
	for(i in 3:nmc){
		if(is.null(mc[[i]]))
			stop("NULL subscripting is not allowed in data lists")
		else if(mc[[i]] != substitute()){
			indi <- eval(mc[[i]],envir=parent.frame())
			if(is.logical(indi)){
				ndm <- length(dim.names[[i-2]])
				indi <- suppressWarnings(
					as.logical(indi + rep(0,ndm))
				)
				if(all(!indi))
					stop("variables with zero length are not allowed in data lists")
			}
			if(is.character(indi)){
				indi <- match(indi,dim.names[[i-2]])
				if(!all(complete.cases(indi))){
					stop("supplied names not found in appropriate dimensions")
				}
			}
			if(is.numeric(indi) && any(abs(indi)<1))
				stop("zero subscripting not allowed in data lists")
			if(any(sign(indi) == -1))
				repdim[i-2] <- dim(x)[i-2] - length(indi)
			else repdim[i-2] <- length(indi)
			if(repdim[i-2]==0) stop("some replication dimensions have been reduced to zero length and this is not allowed")
			mc[[i]] <- indi
			dim.names[[i-2]] <- dim.names[[i-2]][indi]
		}
		else mc[[i]] <- 1:repdim[i-2]
	}	
	mc1 <- list(`[`,bquote(xi))
	mc2 <- as.list(mc)[3:nmc]
	x <- unclass(x)
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
		stop("can't add variables this way...try using [[ instead of $...and don't forget to specify match.dimids")
	if(is.null(value)){
		ni <- which(nx==i)
		return(x[-ni])
	}
	xl <- unclass(x)
	if(length(xl[[i]])!=length(value))
		stop("length of replacement value does not
			match length of the variable to be replaced")
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

`[[<-.data.list` <- function(x,i,match.dimids,drop=TRUE,value){
	nx <- names(x)
	if(is.character(i)){
		if(!any(nx %in% i)){
			if(missing(match.dimids))
				stop("match.dimids required for this assignment")
			match.dimids <- c(attr(x,"match.dimids"),list(match.dimids))
			x <- unclass(x)
			#if(is.factor(value)) 
			#else x[[i]] <- value
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
	if(is.null(value)){
		if(is.character(i)) ni <- which(names(x)==i)
		else if(is.numeric(i)) ni <- i
		return(x[-ni,drop=drop])
	}
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

# the purpose of this function (by Hadley Wickham) is to test if 'try' found and error
is.error <- function(x) inherits(x, "try-error")

datalistVARattributes <- function(dlvar){
	# returns only those attributes required by variables in a data list
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
