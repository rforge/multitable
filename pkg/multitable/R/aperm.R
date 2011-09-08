aperm.factor <-
function(a, perm, ...){
	# a stupid workaround for the behaviour of aperm to convert to numeric
	a.dim <- dim(a)
	a.names <- dimnames(a)
	a <- structure(as.character(a),dim=a.dim)
	a <- aperm(a,perm,...)
	a <- structure(as.factor(a),
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

t.data.list <- function(x) aperm(x)