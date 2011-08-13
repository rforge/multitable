as.data.list <-
function(x,...) UseMethod("as.data.list")

as.data.list.list <-
function(x,dnames,match.dnames,...){
	if(!missing(dnames)) x$dnames <- dnames
	if(!missing(match.dnames)) x$match.dnames <- match.dnames
	do.call(data.list,x)
}

as.data.list.data.frame <-
function(x,...) data.list(x,...)

as.list.data.list <-
function(x, drop.attr=TRUE, ...){
	out <- unclass(x)
	if(drop.attr){
		for(i in seq_along(x)) attr(out[[i]],"subsetdim") <- NULL
		attr(out,"match.dnames") <- NULL
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

