variablize <- function(x,...){
	UseMethod("variablize")
}

variablize.default <- function(x,...){
	dim.x <- dim(x)
	
	# if there's nothing to variablize, just return the input
	if(is.null(dim.x)||(length(dim.x)==1)) return(x)
	
	# calculate names
	var.names <- big.kronecker(rev(dimnames(x)[-1]),FUN=paste,sep=".")
	rep.names <- dimnames(x)[[1]]
	
	# variablizing puts all but the first dimension in the columns.
	# therefore, the dimensions of a variablized object are just the
	# first dimension and the product of the remaining dimensions.
	dim(x) <- c(dim.x[1],prod(dim.x[-1]))

	out <- as.data.frame(x)	
	dimnames(out) <- list(rep.names,var.names)
	return(out)
}

variablize.factor <- function(x,...){
	dim.x <- dim(x)
	names.x <- names(x)
	
	# if there's nothing to variablize, just return the input
	if(is.null(dim.x)||(length(dim.x)==1)) return(x)
	
	# calculate names
	var.names <- big.kronecker(rev(dimnames(x)[-1]),FUN=paste,sep=".")
	rep.names <- dimnames(x)[[1]]
	
	# BUG!! THIS ONLY WORKS FOR 2D ARRAYS (I.E. MATRICES)
	# what was i thinking?
	x <- lapply(seq_len(ncol(x)),function(i)x[,i])
	
	if(is.null(names.x)) names(x) <- paste("V",seq_along(x),sep="")
	else names(x) <- names.x
	
	out <- as.data.frame(x)
	dimnames(out) <- list(rep.names,var.names)
	return(out)
}

variablize.data.list <- function(x,...){
	# get all of the variables in x that are replicated along
	# the first dimension
	x <- as.list(x)[summary(x)[1,]]

	# variablize each component of x
	for(i in seq_along(x))
		x[[i]] <- variablize(x[[i]])

	# concatenate all of the resulting data frames together
	do.call(data.frame,x)
}
