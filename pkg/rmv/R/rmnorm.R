#' Matrix normal distribution
#'
#' Simulate random matrix normal variates.
#' 
#' \code{rmnorm} generates a list of \code{n} matrices, 
#' whereas \code{rmnorm1} generates one matrix.
#'
#' @param M Mean matrix.
#' @param Omega Covariance matrix for the columns.
#' @param Sigma Covariance matrix for the rows.
#' @param n Number of random matrices to generate.
#' @param ... Arguments to be passed to \code{rmnorm1}.
#' @return Random matrix or a list of i.i.d. random matrices.
#' @export
rmnorm <- function(n, ...) lapply(1:n, function(i) rmnorm1(...))

#' @rdname rmnorm
#' @export
rmnorm1 <- function(
	M, 
	Omega = diag(1, ncol(M), ncol(M)),
	Sigma = diag(1, nrow(M), nrow(M))){
	
	
	n <- nrow(M)
	m <- ncol(M)
	
	OxS <- Omega %x% Sigma
	E <- rmv(1, OxS) # can be very inefficient if OxS is big!
                   # there's got to be a kronecker product trick here.

	vecM <- as.numeric(M)
	vecE <- as.numeric(E)
	vecY <- vecM + vecE

	matrix(vecY, n, m)	
}

#' Random uncorrelated matrix
#' 
#' @param p Number of rows
#' @param q Number of columns
#' @param rfunc Random function
#' @return A random matrix
#' @export
rmat <- function(p, q, rfunc = rnorm, ...) matrix(rfunc(p*q, ...), p, q)
