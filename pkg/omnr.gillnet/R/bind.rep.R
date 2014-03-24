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
#' @rdname bind.rep
#' @export
rbind.rep <- function(x, times) matrix(x, times, length(x), byrow = TRUE)

#' @rdname bind.rep
#' @inheritParams rbind.rep
#' @export
cbind.rep <- function(x, times) matrix(x, length(x), times, byrow = FALSE)


#' Create consistent length and mesh size vectors
#' 
#' @param l Lengths.
#' @param m Mesh sizes.
#' @param perimeter.factor Factor by which \code{m} is multiplied.
#' @return A list of matrices with two components:
#' @return l A lengths vector.
#' @return m A mesh sizes vector.
#' @family l.m.combine
#' @export
l.m.vecs <- function(l, m, perimeter.factor = 1){
  #FIXME:  names for output is probably inconsistent in this function
  m <- m * perimeter.factor
  em <- 'l and m do not match'
  
  if(is.matrix(l) & is.matrix(m)){
    if(all(dim(l) == dim(m))) stop(em)
    return(list(l=l[,1], m=m[1,]))
  }
  else if(is.matrix(l)){
    p <- nrow(l)
    q <- ncol(l)
    if(q != length(m)) stop(em)
    return(list(l=l[,1], m = m))
  }
  else if(is.matrix(m)){
    p <- nrow(m)
    q <- ncol(m)
    if(p != length(l)) stop(em)
    return(list(l = l, m=m[1,]))
  }
  else{
    return(list(l = l, m = m))
  }
}

#' Create consistent length and mesh size matrices
#' 
#' @param l Lengths.
#' @param m Mesh sizes.
#' @param perimeter.factor Factor by which \code{m} is multiplied.
#' @return A list of matrices with two components:
#' @return l A lengths matrix.
#' @return m A mesh sizes matrix.
#' @family l.m.combine
#' @export
l.m.mats <- function(l, m, perimeter.factor = 1){
  
  m <- m * perimeter.factor
  em <- 'l and m do not match'
  
  if(is.matrix(l) & is.matrix(m)){
    if(any(dim(l) != dim(m))) stop(em)
    return(list(l=l, m=m))
  }
  else if(is.matrix(l)){
    p <- nrow(l)
    q <- ncol(l)
    if(q != length(m)) stop(em)
    mout <- rbind.rep(m, p)
    dimnames(mout) <- dimnames(l)
    return(list(l=l, m = mout))
  }
  else if(is.matrix(m)){
    p <- nrow(m)
    q <- ncol(m)
    if(p != length(l)) stop(em)
    lout <- cbind.rep(l, q)
    dimnames(lout) <- dimnames(m)
    return(list(l = lout, m=m))
  }
  else{
    p <- length(l)
    q <- length(m)
    mout <- rbind.rep(m, p)
    lout <- cbind.rep(l, q)
    dimnames(mout) <- dimnames(lout) <- list(names(l), names(m))
    return(list(l = lout, m = mout))
  }
}
