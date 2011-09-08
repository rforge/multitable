data.list <-
function(...,dimids,match.dimids,check=TRUE,drop=TRUE,unique=TRUE){
	object <- as.list(substitute(list(...)))[-1L]
	x <- list(...)
	n <- length(x)
	vnames <- names(x)
	if(length(vnames) != n)
		vnames <- character(n)
	no.vn <- !nzchar(vnames)
	vnames[no.vn] <- object[no.vn]
	names(x) <- vnames
	x <- as.data.list(x,dimids,match.dimids,check=check,drop=drop)
	names(x) <- make.names(names(x),unique=unique)
	return(x)
}