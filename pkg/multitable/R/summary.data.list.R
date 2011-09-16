str.data.list <- function(object, give.attr = FALSE, ...){
	str(as.list(object), give.attr = give.attr, ...)
}

summary.data.list <- function(object, ...){
	x <- unclass(object)
	return(sapply(x,attr,"subsetdim"))
}