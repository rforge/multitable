as.data.list <-
function(x) UseMethod("as.data.list")

as.data.list.list <-
function(x){
	if(!all(lapply(x,length)==length(x[[1]]))) stop("all list elements must be the same length for coersion to a data list")
	for(i in seq_along(x)) dim(x[[i]]) <- NULL
	data.list(list(x),list("reps"))
}

as.data.list.data.frame <-
function(x) data.list(list(as.list(x)),list("reps"))

as.list.data.list <-
function(x, drop.attr=TRUE, ...){
	out <- unclass(x)
	if(drop.attr){
		for(i in seq_along(x)) attr(out[[i]],"subsetdim") <- NULL
		attr(out,"bm") <- NULL
		attr(out,"repdim") <- NULL
	}
	return(out)
}

as.data.frame.data.list <-
function(x, row.names = NULL, optional = FALSE, scheme = "repeat", mold, ...){
	if(missing(mold)) mold <- data.list.mold(x)
	out <- lapply(seq_along(x),function(i)x[[i]][mold[[i]]])
	names(out) <- varnames(x)
	as.data.frame(out, row.names = row.names, optional = optional,...)
}

data.list.mold <-
function(x){
	repdims <- dim(x)
	xmold <- list()
	for(i in seq_along(x)){
		xmold[[i]] <- seq_along(x[[i]])
		dms <- attr(x[[i]],"subsetdim")
		prms <- c(which(dms),which(!dms))
		xmold[[i]] <- aperm(array(xmold[[i]],repdims[prms]),order(prms))
		dim(xmold[[i]]) <- NULL
	}
	return(xmold)
}

