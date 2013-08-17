#' Block matrix
#'
#' Create matrices out of blocks.
#'
#' @docType package
#' @name bmatrix
#' @aliases bmatrix package-bmatrix bmatrix-package
NULL

#' Block matrix
#' 
#' Create a matrix out of blocks.
#' 
#' @param data A list of matrices.
#' @param nrow Number of block rows.
#' @param ncol Number of block columns.
#' @param byrow Fill matrix by block rows?
#' @return A matrix
#' @author Steve Walker
#' @export
bmatrix <- function(data, nrow = 1, ncol = 1, byrow = FALSE){
 

  # TODO: what is the meaning of firstbind, secondbind
  if(byrow){
    firstbind <- cbind
    secondbind <- rbind
  }
  else{
    firstbind <- rbind
    secondbind <- cbind
  }

  ind <- rep(1:ncol, rep(nrow, ncol))
  n <- ncol
  
  collist <- list()
  for(i in 1:n) collist[[i]] <- do.call(firstbind, data[ind == i])
  return(do.call(secondbind, collist))
  
}

#' Block diagonal matrix
#' 
#' Create a block diagonal matrix from blocks.
#' 
#' @param data A list of matrices.
#' @return A matrix
#' @author Steve Walker
#' @export
bdmatrix <- function(data){
  dims <- sapply(data, dim)
  ends <- apply(dims, 1, cumsum)
  starts <- sweep(ends, c(1, 2), - 1 + t(dims))

  out <- matrix(0, sum(dims[1, ]), sum(dims[2, ]))
  for(i in seq_along(data)){
    out[starts[i, 1]:ends[i, 1], starts[i, 2]:ends[i, 2]] <- data[[i]]
  }
  
  return(out)
}
