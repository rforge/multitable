bm <- function(x) x[[attr(x,"bm")]]

nvar <- function(dl) length(dl)

varnames <- function(dl) names(dl)

`varnames<-` <-
function(dl,value){
	if(length(value)!=length(varnames(dl))) stop("number of names must equal number of variables")
	attr(dl,"names") <- value
	return(dl)
}

dim.data.list <- function(x) attr(x,"repdim")

repdim <- function(x) attr(x,"repdim")

`dim<-.data.list` <-
function(x,value) stop("replication dimensions cannot be set in this fashion")

dimnames.data.list <- function(x) dimnames(bm(x))

"dimnames<-.data.list" <- function(x,value){
	for(i in seq_along(x)){
		dimnames(x[[i]]) <- value[attr(x[[i]],"subsetdim")]
	}
	return(x)
}

