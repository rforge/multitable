bm <- function(x){
	x[[attr(x,"bm")]]
}

nvar <- function(dl) length(dl)

varnames <- function(dl) names(dl)

`varnames<-` <-
function(dl, value){
	if(length(value)!=length(varnames(dl))) stop("number of names must equal number of variables")
	names(attr(dl, "match.dimids")) <- value
	attr(dl,"names") <- value
	return(dl)
}

names.data.list <- function(x) names(unclass(x))

`names<-.data.list` <- function(x, value) `varnames<-`(x, value)

dim.data.list <- function(x) attr(x,"repdim")

repdim <- function(x) attr(x,"repdim")

`dim<-.data.list` <-
function(x,value) stop("replication dimensions cannot be set in this fashion")

dimnames.data.list <- function(x) dimnames(bm(x))

"dimnames<-.data.list" <- function(x,value){
	x <- unclass(x)
	for(i in seq_along(x)){
		attr(x[[i]],"dimnames") <- value[attr(x[[i]],"subsetdim")]
	}
	class(x) <- "data.list"
	return(x)
}

