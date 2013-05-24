#'Get fitted selection curve
#'
#'Calculate a selection curve estimated under a fitted selectivity model (i.e.
#'\code{fit.catch} object).
#'
#'
#'@aliases getSelcurve getTotalSelcurve
#'@param x a \code{\link{fit.catch}} object (for \code{modelaverageTotalSelcurve} a
#'\code{\link{fit.catch.compare}} object).
#'@param l a vector of fish lengths the same size and dimensions as \code{m};
#'defaults to x$l
#'@param m a vector of mesh sizes the same size and dimensions as \code{l};
#'defaults to x$m
#'@param tangle selection curve for model with a tangle parameter?
#'@param ... arguments to pass to \code{getSelcurve}
#'@return For \code{getSelcurve} a vector of relative selection probabilities
#'corresponding to the fish length and mesh data in \code{l} and \code{m}, with
#'identical dimensions to \code{l} and \code{m}.  For \code{getSelcurve} a
#'vector of total relative selection probabilities (i.e. over all mesh sizes)
#'corresponding to each fish length class in \code{x}.
#'@author Steve Walker
#'@export
#'@seealso \code{\link{sel.curve}}
getSelcurve <- function(x, l = get.lens(x), m = get.mesh(x), tangle = TRUE){
	
  curvfn <- x$sel.curve()$curv
  peakfn <- x$sel.curve()$peak
  mbl <- min(get.mesh(x)) # required because lognorm meshes are normalized by a baseline mesh.
                          # shouldn't influence other selcurves.
  
  l.m <- l.m.mats(l, m)
  l <- l.m$l
	m <- l.m$m
  
	if(tangle){
		if(!x$tangle) stop("no tangle parameter in fitted model, set tangle = FALSE")
		else{
			theta <- x$theta$tangle
        	omega <- theta[1]
        	theta <- theta[-1]
        	r <- curvfn(l, m, theta, mbaseline = mbl)
 	       	r[(r < omega) & (l > peakfn(m, theta, mbaseline = mbl))] <- omega
 	       	return(r)
		}
	}
	else{
		if(!x$tangle) return(x$sel.curve()$curv(l,m,x$theta))
		else return(x$sel.curve()$curv(l,m,x$theta$notangle))
	}
}

#' @rdname getSelcurve
#' @export
getTotalSelcurve <- function(x, ...){
	sc <- getSelcurve(x, ...)			 	# selection curve
	tsc <- rowSums(sc)						# total selection curve
	ntsc <- tsc / max(tsc)					# normalized total selection curve
	ntsc
}

#' @rdname getSelcurve
#' @export
modelaverageTotalSelcurve <- function(x, l = get.lens(x), m = get.mesh(x), ...){
  l.m <- l.m.mats(l, m)
  summary(x, l = l.m$l, m = l.m$m, ...)$ma.selcurve
}
