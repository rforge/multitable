getSelcurve <- function(x, l, m, tangle = TRUE){
	if(missing(l)) l <- x$l
	if(missing(m)) m <- x$m
	if(tangle){
		if(!x$tangle) stop("no tangle parameter in fitted model, set tangle = FALSE")
		else{
			theta <- x$theta$tangle
        	omega <- theta[1]
        	theta <- theta[-1]
        	r <- x$sel.curve()$curv(l, m, theta)
 	       	r[(r < omega) & (l > x$sel.curve()$peak(m, theta))] <- omega
 	       	return(r)
		}
	}
	else{
		if(!x$tangle) return(x$sel.curve()$curv(l,m,x$theta))
		else return(x$sel.curve()$curv(l,m,x$theta$notangle))
	}
}

getTotalSelcurve <- function(x, ...){
	sc <- getSelcurve(x, ...)			 	# selection curve
	tsc <- rowSums(sc)						# total selection curve
	ntsc <- tsc / max(tsc)					# normalized total selection curve
	ntsc
}

modelaverageTotalSelcurve <- function(x) summary(x)$ma.selcurve
