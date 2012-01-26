# TODO:  test for behaviour with ordered levels, probably some problems here.

aperm.factor <-
function(a, perm, ...){
	# a stupid workaround for the behaviour of aperm to unclass factors
	a.dim <- dim(a)
	a.names <- dimnames(a)
	a.levels <- attr(a,"levels")

	# this is very important...it makes sure that factor() makes an 
	# appropriate comparison between the variable and its levels.
	a <- as(a, mode(a.levels))

	a <- structure(a,dim=a.dim)
	a <- aperm.default(a,perm,...)
	
	# very important to set levels inside factor() rather than
	# in structure!!!!
	a <- structure(factor(a, levels = a.levels),
		dim=a.dim[perm],dimnames=a.names[perm])
	return(a)
}

aperm.data.list <- 
function(a, perm, ...){
	if(missing(perm)) perm <- rev(seq_along(dim(a)))
	bm.a <- attr(a,"bm")
	match.dimids <- attr(a,"match.dimids")
	match.dimids[[bm.a]] <- match.dimids[[bm.a]][perm]
	l <- as.list(a)
	l[[bm.a]] <- aperm(l[[bm.a]],perm)
	as.data.list(l,match.dimids=match.dimids)
}

t.data.list <- function(x) aperm.data.list(x)