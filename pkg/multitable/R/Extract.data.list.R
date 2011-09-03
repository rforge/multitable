`[.data.list` <-
function(x,...){
	mc <- match.call()
	nmc <- length(mc)
	nd <- nmc - 2
	repdim <- dim(x)
	nv <- nvar(x)
	if(nd==1){
		rng <- range(eval(mc[[3]]))
		if((rng[2] > nv) || (rng[1] < 1))
			stop("subscript out of range")
		mc[[1]] <- `[`
		mc[[2]] <- bquote(xl)
		xl <- unclass(x)
		xl <- eval(mc)
		xl.summ <- summary.data.list(xl)
		dnames <- rownames(xl.summ)
		match.dnames <- lapply(seq_len(ncol(xl.summ)), function(i)
			dnames[xl.summ[,i]])
		xdl <- as.data.list(xl,match.dnames=match.dnames)
		return(xdl)
	}
	if(nd!=length(repdim)) stop("incorrect number of dimensions")
	dim.names <- dimnames(bm(x))
	for(i in 3:nmc){
		if(mc[[i]] != substitute()){
			indi <- eval(mc[[i]],envir=parent.frame())
			if(is.logical(indi)) indi <- which(indi)
			if(is.character(indi)){
				indi <- match(indi,dim.names[[i-2]])
				if(!all(complete.cases(indi))){
					stop("supplied names not found in appropriate dimensions")
				}
			}
			repdim[i-2] <- length(indi)
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
	xi.attr <- attributes(x[[i]])
	x <- as.list(x,drop.attr=FALSE)
	x[[i]] <- value
	if(!is.null(value)) attributes(x[[i]]) <- c(attributes(x[[i]]),xi.attr)
	class(x) <- "data.list"
	return(x)
}

`[[<-.data.list` <- function(x,i,value){
	xi.attr <- attributes(x[[i]])
	x <- as.list(x,drop.attr=FALSE)
	x[[i]] <- value
	if(!is.null(value)) attributes(x[[i]]) <- c(attributes(x[[i]]),xi.attr)
	class(x) <- "data.list"
	return(x)
}

#`[[.data.list` <- function(x,i)


