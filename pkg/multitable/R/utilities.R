# multivariate version of outer (base)
#
# used in: dlcast, data.list.mold
#
mouter <- function(x,...){
	spouter <- function(x,y) outer(x,y,...)
	Reduce("spouter",x)
}

# recursive version of the kronecker product (base)
#
# used in: variablize
# previously used in: data.list.mold
#
big.kronecker <- function(x,...){
	if(length(x)==1) return(x[[1]])
	x[[1]] <- kronecker(x[[1]],x[[2]],...)
	x[[2]] <- NULL
	Recall(x,...)
}



# get the dimids from a data list
#
# used in: dims_to_vars
#
get.dimids <- function(dl)
	attr(dl, "match.dimids")[[attr(dl, "bm")]]
