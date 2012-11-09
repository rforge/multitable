is.4th.corner <- function(x){
	
	if(!is.data.list(x)) return(FALSE)
	
	sx <- summary(x)
	
	# only two dimensions allowed
	if(nrow(sx) != 2L) return(FALSE)
	
	# only one fully replicated variable allowed
	if(sum(apply(sx, 2, all)) != 1L) return(FALSE)
	
	# both dims must be missing some variables
	if(!all(apply(!sx, 1, any))) return(FALSE)
	
	return(TRUE)
}
