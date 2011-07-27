`[.data.list` <-
function(x,...){
	mc <- match.call()
	nmc <- length(mc)
	nd <- nmc - 2
	repdim <- dim(x)
	dim.names <- dimnames(bm(x))
	for(i in 3:nmc){
		if(mc[[i]] != substitute()){
			indi <- eval(mc[[i]],envir=parent.frame())
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
	attributes(x[[i]]) <- c(attributes(x[[i]]),xi.attr)
	class(x) <- "data.list"
	return(x)
}

`[[<-.data.list` <- function(x,i,value){
	xi.attr <- attributes(x[[i]])
	x <- as.list(x,drop.attr=FALSE)
	x[[i]] <- value
	attributes(x[[i]]) <- c(attributes(x[[i]]),xi.attr)
	class(x) <- "data.list"
	return(x)
}
