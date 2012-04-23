AIC.fit.catch <-
function(object, corrected=TRUE, ..., k=2){
	
	# get number of parameters, but only keep the first
	# if only the no tangle model was fitted
	p <- c(2,3)+length(object$catch$lens)
	if(!object$tangle) p <- p[1]
	
	# calculate AIC and (maybe) correct for small sample sizes
	output <- deviance(object) + (2*p)
	if(corrected) # correction STRONGLY recommended
		output <- output + ((2*(p+1)*(p+2))/(object$n-p-2))

	return(output)
}
