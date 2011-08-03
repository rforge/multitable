variablize <- function(x,...){
	UseMethod("variablize")
}

variablize.default <- function(x,...){
	dim.x <- dim(x)
	if(is.null(dim.x)||(length(dim.x)==1)) return(x)
	dim(x) <- c(dim.x[1],prod(dim.x[-1]))
	return(as.data.frame(x))
}

variablize.factor <- function(x,...){
	dim.x <- dim(x)
	names.x <- names(x)
	if(is.null(dim.x)||(length(dim.x)==1)) return(x)
	x <- lapply(seq_len(ncol(x)),function(i)x[,i])
	if(is.null(names.x)) names(x) <- paste("V",seq_along(x),sep="")
	else names(x) <- names.x
	return(as.data.frame(x))
}

variablize.data.list <- function(x,...){
	x <- as.list(x)[summary(x)$dims[1,]]
	for(i in seq_along(x))
		x[[i]] <- variablize(x[[i]])
	do.call(data.frame,x)
}

