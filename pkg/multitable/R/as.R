as.data.list <-
function(x,...) UseMethod("as.data.list")

as.data.list.default <-
function(x,dnames,match.dnames,check = TRUE,...){
	if(!is.list(x)) x <- list(x)
	if(missing(match.dnames)) match.dnames <- make.match.dnames(x,dnames)
	if(!is.list(match.dnames)) stop("match.dnames must be a list")
	if(length(x)!=length(match.dnames)) stop("match.dnames not the right length")
	if(check.full.rep(match.dnames) && check) stop(
		"at least one variable must be replicated along all dimensions"
	)
	x <- split.dfs(x,match.dnames)
	bm <- which.fully.replicated(x$x)
	repdim <- dim(x$x[[bm]])
	if(is.null(names(x$x))) names(x$x) <- paste("V",seq_along(x$x),sep="")
	if(check) check.dims(x,bm,repdim)
	x <- subsetdim(x,bm,repdim)
	match.dnames <- x$match.dnames
	x <- structure(x$x, bm = bm, match.dnames = match.dnames,
		repdim = repdim, class = "data.list")
	make.dimnames.consistent(x,bm)
	return(x)
}

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

as.matrix.data.list <- 
function(x, ...){
	fac <- mapply("||",sapply(x,is.factor),sapply(x,is.character))
	if(all(!fac)||all(fac))
		return(as.matrix(as.data.frame(x)))
	x.attr <- attributes(x)
	x <- unclass(x)
	x.num <- structure(x[!fac],class="data.list",
		match.dnames = x.attr$match.dnames[!fac],
		repdim = x.attr$repdim)
	x.fac <- structure(x[fac],class="data.list",
		match.dnames = x.attr$match.dnames[fac],
		repdim = x.attr$repdim)
	list(x.num=as.matrix(as.data.frame(x.num)),
		x.fac=as.matrix(as.data.frame(x.fac)))
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

