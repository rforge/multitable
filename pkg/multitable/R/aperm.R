aperm.factor <-
function(a, perm, ...){
	# a stupid workaround for the behaviour of aperm to convert to numeric
	a.dim <- dim(a)
	a <- as.character(a)
	dim(a) <- a.dim
	a <- aperm(a,perm,...)
	a <- as.factor(a)
	dim(a) <- a.dim[perm]
	return(a)
}

aperm.data.list <- 
function(a, perm, ...){
	if(missing(perm)) perm <- rev(seq_along(dim(a)))
	bm.a <- attr(a,"bm")
	match.dnames <- attr(a,"match.dnames")
	match.dnames[[bm.a]] <- match.dnames[[bm.a]][perm]
	l <- as.list(a)
	l[[bm.a]] <- aperm(l[[bm.a]],perm)
	as.data.list(l,match.dnames=match.dnames)
}

t.data.list <- function(x) aperm(x)