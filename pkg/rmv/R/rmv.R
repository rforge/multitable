#' Multivariate random number generation
#' 
#' Generates correlated random variables by the matrix square-root
#' method.
#'
#' The number, \code{m}, of variables generated per each of the \code{n} 
#' observations is equal to the number of rows (or columns) of 
#' \code{covmat}.  \code{rfunc} is used to fill an \code{n} by \code{m} 
#' matrix of uncorrelated observations.  This matrix is multiplied by
#' the matrix square-root (\code{\link{msr}}) to obtain a matrix of 
#' \code{m} correlated random variables.
#'
#' @param n Number of random multivariate draws.
#' @param covmat The covariance matrix with which to obtain correlations.
#' @param rfunc Function that returns a vector of random draws from a 
#'	particular distribution.  The first argument of \code{rfunc} must
#'	give the number of random draws to make.
#' @param method The method for taking the matrix square-root.  Currently
#'	only \code{'chol'} and \code{'eigen'} are allowed for the Cholesky and
#'	eigen decomposition methods.  See \code{\link{msr}}.
#' @param ... Further arguments to pass to \code{rfunc}.
#' @return A matrix with rows giving observations and columns giving 
#'	correlated variables.
#' @export
#' @examples
#' set.seed(1)
#' cm <- cor(matrix(rnorm(12), 6, 2))
#' x <- rmv(100, cm, rbinom, size = 100, prob = 0.7)
#' pairs(x)
#' cm
rmv <- function(n, covmat, rfunc = rnorm, method = c('chol', 'eigen'),...){
	m <- nrow(covmat)
	msr <- msr(covmat, method = method)
	x <- matrix(rfunc(n*m, ...), n, m)
	x %*% msr
}


#' Square matrix square-root
#' 
#' Calculate the square-root of a square matrix.
#' 
#' A square-root, \code{S}, of a square matrix, \code{M}, satisfies,
#' \code{M = t(S) \%*\% S}.
#'
#' @param sqrmat A square matrix.
#' @param method A method for calculating the matrix square-root.
#' @return The matrix square-root.
#' @export
msr <- function(sqrmat, method = c('chol', 'eigen')){
	m <- nrow(sqrmat)
	method <- match.arg(method)
	if(method == 'chol'){
		msr <- chol(sqrmat)
	}
	else if(method == 'eigen'){
		ed <- eigen(sqrmat)
		msr <- diag(sqrt(ed$values), m, m) %*% t(ed$vectors)
	}
	else{
		stop('method not recognised')
	}
	return(msr)
}
