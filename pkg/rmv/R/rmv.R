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


#' Correlated random number generation from factor analysis models
#'
#' Simulate correlated random variables by supplying the parameters of 
#' a linear factor analysis model.
#'
#' The population covariance matrix for the multivariate observations is 
#' given by:  \code{Lambda %*% t(Lambda) + diag(psi)}.  The idea here is
#' that Lambda are coefficients relating a set of 
#' An alternative
#' interpretation of this function is that Lambda is a matrix square-root 
#' of a singular covariance matrix TODO
#' 
#' be understood similarly to \code{\link{rmv}}
#'
#' @param n Number of random multivariate draws.
#' @param Lambda Variables-by-factors matrix of loadings for the
#'  factor analysis model.
#' @param psi Vector of uniquenesses (i.e. residual variances).  One
#'  variance for each variable.
#' @return A matrix with rows giving observations and columns giving 
#'	correlated variables.
#' @export
rfa <- function(n, Lambda, psi){
	m <- nrow(Lambda) # number of variables
	d <- ncol(Lambda) # number of factors
	if(m != length(psi)) stop('Lambda and psi are incompatible')
	X <- matrix(rnorm(n*d), n, d)
	E <- replicate(n, rnorm(m, sd = sqrt(psi)))
	if(is.null(dim(E))) E <- matrix(E, n, m)
	else E <- t(E)
	Y <- (X %*% t(Lambda)) + E
	return(Y)
}


#' Random covariance matrices
#' 
#' Simulate random covariance matrices by samping an \code{n}
#' by \code{m} matrix of iid variates from \code{rfunc} and then
#' passing that matrix to \code{\link{cov}}.
#' 
#' @param n Number of multivariate samples.
#' @param m Number of variables.
#' @param rfunc Function that returns a vector of random draws from a 
#'	particular distribution.  The first argument of \code{rfunc} must
#'	give the number of random draws to make.
#' @param ... Additional arguments to \code{rfunc}.
#' @return A covariance matrix.
#' @export
rcov <- function(n, m, rfunc = rnorm, ...)
	cov(matrix(rfunc(n * m, ...), n, m))

##' Bivariate random number generation
##'
##' @param n Number of bivariate samples.
##' @param rho Correlation between variables.
##' @param mu Means for each variable.
##' @param sigma standard deviations for each variable.
##' @param rfunc Function that returns a vector of random draws from a
##'      particular distribution.  The first argument of \code{rfunc} must
##'      give the number of random draws to make.
##' @return A bivariate sample.
##' @export
rbv <- function(n, rho, mu = c(0,0),
                sigma = c(1,1), ...){
    
    cm <- diag(sigma^2)
    cm[1,2] <- cm[2,1] <- rho
    sweep(rmv(n, cm, ...), 2, mu)
}    
