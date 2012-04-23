AIC.factanal.predictive <- function(object, ..., k = 2, corrected = TRUE){

	p <- object$n.vars
	d <- object$factors

	dev <- deviance(object)
	if(d=="full"){
		nu <- 0.5*p*(p+1)
	}
	else{
		nu <- (p*(d+1)) - (0.5*d*(d-1))
	}
	params <- nu + p
	if(!corrected){
		output <- dev + (2*params)
		names(output) <- "AIC"
	}
	if(corrected){
		n <- object$n.obs
		output <- dev + (2*params) + (
			2*((params*(params+nu))/
			((n*p)-params-nu))
		)
		names(output) <- "MAICc"
	}
	
	return(output)

}
