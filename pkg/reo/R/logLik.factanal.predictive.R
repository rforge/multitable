logLik.factanal.predictive <-
function(object, ...){

	n <- object$n.obs
	p <- object$n.vars

	Cov <- object$Cov
	Sig <- cov(object$x)
	ll <- -(n/2)*(
		(p*log(2*pi)) + 
		log(det(Cov)) + 
		sum(solve(Cov)*t(Sig))
	)

	return(ll)

}

