simple.scale <- function(x, center = TRUE, scale = TRUE, simplify = TRUE){
	
	# simplify x into an array (or atmoic vector or matrix)
	if(simplify) x <- simplify2array(x)
	
	# prelim:  calculate mean and squared deviations
	mean.x <- mean(x, na.rm = TRUE)
	sqrdev.x <- (x - mean.x)^2
	
	# center and scale
	if(center) x <- x - mean.x
	if(scale) x <- x/sqrt(mean(sqrdev.x, na.rm = TRUE))
	
	return(x)
}
