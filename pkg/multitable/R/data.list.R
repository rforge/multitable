data.list <-
function(...,dimids,match.dimids,check=TRUE,drop=TRUE,unique=TRUE){
	
	# create names to give to unnamed arguments in ...
	# based on the names of the objects passed to those
	# arguments
	object <- as.list(substitute(list(...)))[-1L]
	x <- list(...)
	n <- length(x)
	vnames <- names(x)
	if(length(vnames) != n)
		vnames <- character(n)
	no.vn <- !nzchar(vnames)
	vnames[no.vn] <- object[no.vn]
	names(x) <- vnames
	
	# convert x (which is created from ...) to a data list
	x <- as.data.list(x,dimids,match.dimids,check=check,drop=drop)

	# process the names
	names(x) <- make.names(names(x),unique=unique)

	return(x)
}
