phi <-
function(theta,l,m,sel.curve,effort=NULL){
	
	#######################################################################
	#
	# phi: calculates the multinomial probability vector (Millar and Fryer 
	#	1999)
	# 
	# theta: vector of parameters for selection curve.  first entry 
	# 	is always a (logit-scale) tangle parameter
	# l: length matrix
	# m: mesh matrix
	# sel.curve: selection curve
	#
	# omega: (probability scale) tangle parameter
	# r: selection curve
	# 
	#
	#######################################################################
	
	omega <- exp(theta[1])/(1+exp(theta[1]))
	theta <- theta[-1]
	r <- sel.curve()$curv(l,m,theta)
	r[(r<omega)&(l>sel.curve()$peak(m,theta))] <- omega
        if(!is.null(effort)) r <- r*cbind.rep(effort,nrow(r))
	sweep(r,1,rowSums(r,na.rm=TRUE),"/")
}
