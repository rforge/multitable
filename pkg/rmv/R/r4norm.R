#' Fourth-corner normal distribution
#'
#' Simulate random data sets from the fourth-corner normal
#' distribution.
#' 
#' \code{r4norm} generates a list of \code{n} matrices, 
#' whereas \code{r4norm1} generates one matrix.
#'
#' @param C The fourth-corner matrix from a bilinear model.
#' @param U The mean of the X matrix from a bilinear model.
#' @param V The mean of the Z matrix from a bilinear model.
#' @param Omega The covariance matrix for the columns of the 
#'  error matrix.
#' @param Sigma The covariance matrix for the rows of the error
#'  matrix.
#' @param Phi The covariance matrix for the columns of the X
#'  matrix.
#' @param Psi The covariance matrix for the columns of the Z
#'  matrix.
#' @param n Number of random matrices to generate.
#' @param ... Arguments to be passed to \code{r4norm1}.
#' @return Random matrix or a list of i.i.d. random matrices.
#' @export
r4norm <- function(n, ...) lapply(1:n, function(i) r4norm1(...))

#' @rdname r4norm
#' @export
r4normstd <- function(n, rowsResp, colsResp, rowsPred, colsPred){
    C <- matrix(rnorm((rowsPred+1)*(colsPred+1)), rowsPred+1, colsPred+1)
    U <- matrix(rnorm(rowsResp*rowsPred), rowsResp, rowsPred)
    V <- matrix(rnorm(colsResp*colsPred), colsResp, colsPred)
    r4norm(n, C = C, U = U, V = V)
}    
    

#' @rdname r4norm
#' @export
r4norm1 <- function(C, U, V, Omega, Sigma, Phi, Psi){
	
	p <- nrow(C) - 1
	q <- ncol(C) - 1
	n <- nrow(U)
	m <- nrow(V)
	
	if(ncol(U) != p) stop('no good')
	if(ncol(V) != q) stop('no good')
	
	if(missing(Omega)) Omega <- diag(1, nrow = m, ncol = m)
	if(missing(Sigma)) Sigma <- diag(1, nrow = n, ncol = n)
	if(missing(Phi)) Phi <- diag(1, nrow = p, ncol = p)
	if(missing(Psi)) Psi <- diag(1, nrow = q, ncol = q)
	
	X <- cbind(1, rmnorm1(U, Phi, diag(1, n, n)))
	Z <- cbind(1, rmnorm1(V, Psi, diag(1, m, m)))
	E <- rmnorm1(matrix(0, n, m), Omega, Sigma)
	
	M <- X %*% C %*% t(Z)
	Y <- M + E
	
	X <- X[,-1, drop = FALSE]
	Z <- Z[,-1, drop = FALSE]
	
	rownames(Y) <- rownames(X) <- paste('n', 1:n, sep = '')
	colnames(Y) <- rownames(Z) <- paste('m', 1:m, sep = '')
	colnames(X) <- paste('x', 1:p, sep = '')
	colnames(Z) <- paste('z', 1:q, sep = '')

	list(Y = Y, X = as.data.frame(X), Z = as.data.frame(Z))
	
}
