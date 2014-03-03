# multivariate version of outer (base)
#
# used in: dlcast, data.list.mold
#
mouter <- function(x, ...){
	spouter <- function(x, y) outer(x, y, ...)
	Reduce("spouter", x)
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

##' more intuitive version of reorder
##'
##' @param x a factor
##' @param X character vector of levels in the correct order
reorder2 <- function(x, X){
	X <- rev(X)
	for(i in seq_along(X)) x <- relevel(x, X[i])
	x
}

#' Matrix with identical rows or columns
#' 
#' Combining the \code{\link{rbind}} and \code{\link{cbind}} functions 
#' with the \code{\link{rep}} functions.
#' 
#' @param x A vector
#' @param times Number of times to replicate the vector
#' 
#' @return A matrix.
#' 
rbind.rep <- function(x, times) matrix(x, times, length(x), byrow = TRUE)

cbind.rep <- function(x, times) matrix(x, length(x), times, byrow = FALSE)
