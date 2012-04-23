deviance.fit.catch <-
function(object,...){
	if(object$tangle){
		output <- sapply(object$dev,function(x)x)
		names(output) <- c("no tangle","tangle")
	}
	else{
		output <- object$dev$notangle
	}
	return(output)
}

