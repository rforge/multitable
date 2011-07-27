aperm.factor <-
function(a, perm, ...){
	# a stupid workaround for the behaviour of aperm to convert to numeric
	a.dim <- dim(a)
	a <- as.character(a)
	dim(a) <- a.dim
	a <- aperm(a,perm,...)
	a <- as.factor(a)
	dim(a) <- a.dim
	return(a)
}

