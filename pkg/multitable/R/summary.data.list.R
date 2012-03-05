str.data.list <- function(object, give.attr = FALSE, hide.internals = TRUE, ...){
	if(hide.internals)
		str(as.list(object), give.attr = give.attr, ...)
	else
		str(unclass(object), ...)
}

summary.data.list <- function(object, ...){
	# unclassing is necessary for sapply to
	# retain the attributes of each variable!
	x <- unclass(object)
	return(sapply(x,attr,"subsetdim"))
}
