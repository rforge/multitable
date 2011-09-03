variablize <- function(x,...){
	UseMethod("variablize")
}

variablize.default <- function(x,...){
	dim.x <- dim(x)
	if(is.null(dim.x)||(length(dim.x)==1)) return(x)
	var.names <- big.kronecker(rev(dimnames(x)[-1]),FUN=paste,sep=".")
	rep.names <- dimnames(x)[[1]]
	dim(x) <- c(dim.x[1],prod(dim.x[-1]))
	out <- as.data.frame(x)
	dimnames(out) <- list(rep.names,var.names)
	return(out)
}

variablize.factor <- function(x,...){
	dim.x <- dim(x)
	names.x <- names(x)
	if(is.null(dim.x)||(length(dim.x)==1)) return(x)
	var.names <- big.kronecker(rev(dimnames(x)[-1]),FUN=paste,sep=".")
	rep.names <- dimnames(x)[[1]]
	x <- lapply(seq_len(ncol(x)),function(i)x[,i])
	if(is.null(names.x)) names(x) <- paste("V",seq_along(x),sep="")
	else names(x) <- names.x
	out <- as.data.frame(x)
	dimnames(out) <- list(rep.names,var.names)
	return(out)
}

variablize.data.list <- function(x,...){
	x <- as.list(x)[summary(x)[1,]]
	for(i in seq_along(x))
		x[[i]] <- variablize(x[[i]])
	do.call(data.frame,x)
}

