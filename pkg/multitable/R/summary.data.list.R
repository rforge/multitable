str.data.list <- function(object, give.attr = FALSE, ...){
	str(as.list(object), give.attr = give.attr, ...)
}

summary.data.list <- function(object, ...){
	x <- unclass(x)
	out <- list()
	out$dims <- sapply(x,attr,"subsetdim")
	out$modes <- sapply(x,mode)
	return(out)
}