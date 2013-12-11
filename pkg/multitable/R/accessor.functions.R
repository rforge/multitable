bm <- function(x){
	x[[attr(x, "bm")]]
}

nvar <- function(dl) length(dl)

varnames <- function(dl) names(dl)

`varnames<-` <-
function(dl, value){
	if(length(value) != length(dl)) stop("number of names must equal number of variables")
	names(attr(dl, "match.dimids")) <- value
	attr(dl, "names") <- value
	names(attr(dl, "bm")) <- names(dl)[attr(dl, "bm")]
	return(dl)
}

names.data.list <- function(x) names(unclass(x))  #.Primitive("names")(x)

`names<-.data.list` <- function(x, value) `varnames<-`(x, value)

dimnames.data.list <- function(x) dimnames(bm(x))
#	ans <- dimnames(bm(x))
	#if(is.list(ans))
	#	names(ans) <- dimids(x)
#	return(ans)
#}

"dimnames<-.data.list" <- function(x, value){
	x <- unclass(x)
	for(i in seq_along(x)){
		attr(x[[i]], "dimnames") <- value[attr(x[[i]], "subsetdim")]
	}
	class(x) <- "data.list"
	return(x)
}

dim.data.list <- function(x){
	toprint <- attr(x, "repdim")
	names(toprint) <- dimids(x)
	toprint
}

repdim <- function(x) #attr(x, "repdim")
	dim(x)


`dim<-.data.list` <- function(x, value) stop("replication dimensions cannot be set in this fashion")
