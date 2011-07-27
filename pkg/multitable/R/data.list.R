data.list <-
function(x,rep.dim.names,check=TRUE){
	if(!is.list(x)) stop("x must be a list")
	if(!is.list(rep.dim.names)) stop("rep.dim.names must be a list")
	if(length(x)!=length(rep.dim.names)) stop("x and rep.dim.names must be the same length")
	if(check.full.rep(rep.dim.names)) stop(
		"at least one array must be replicated along all dimensions"
	)
	x <- split.dfs(x,rep.dim.names)
	bm <- which.fully.replicated(x$x)
	repdim <- dim(x$x[[bm]])
	if(check) check.dims(x,bm,repdim)
	x <- subsetdim(x,bm,repdim)
	x <- x$x
	x <- make.dimnames.consistent(x,bm)
	attr(x,"bm") <- bm
	attr(x,"repdim") <- repdim
	class(x) <- "data.list"
	return(x)
}

which.fully.replicated <-
function(x) which.max(sapply(x,function(xi)length(dim(xi))))

check.full.rep <-
function(rep.dim.names){
	mt <- rep.dim.names[[which.max(sapply(rep.dim.names,length))]]
	dims <- lapply(rep.dim.names,match,table=mt)
	any(sapply(dims,function(x)any(is.na(x))))
}

check.dims <-
function(x,bm,repdim){
	rdn.bm <- x$rep.dim.names[[bm]]
	for(i in seq_along(x$x)[-bm]){
		for(j in seq_along(x$rep.dim.names[[i]])){
			repdimij <- repdim[which(rdn.bm==x$rep.dim.names[[i]][j])]
			dimij <- dim(x$x[[i]])[j]
			if(repdimij!=dimij) stop(paste("incompatible dimensions in",names(x$x)[i]))
		}
	}
}

subsetdim <-
function(x,bm,repdim){
	inds <- lapply(x$rep.dim.names,match,table=x$rep.dim.names[[bm]])
	for(i in seq_along(x$x)){
		ord.inds <- order(inds[[i]])
		x$x[[i]] <- aperm(x$x[[i]],ord.inds)
		x$rep.dim.names[[i]] <- x$rep.dim.names[[i]][ord.inds]
	}
	inds <- lapply(x$rep.dim.names,match,table=x$rep.dim.names[[bm]])
	nd <- length(repdim)
	for(i in seq_along(x$x)){
		attr(x$x[[i]],"subsetdim") <- rep(FALSE,nd)
		attr(x$x[[i]],"subsetdim")[inds[[i]]] <- TRUE
		names(attr(x$x[[i]],"subsetdim")) <- x$rep.dim.names[[bm]]
	}
	return(x)
}

make.dimnames.consistent <-
function(x,bm){
	bm.dimnames <- attr(x[[bm]],"dimnames")
	for(i in seq_along(x)){
		attr(x[[i]],"dimnames") <- bm.dimnames[attr(x[[i]],"subsetdim")]
	}
	return(x)
}

split.dfs <-
function(x,rep.dim.names){
	x.alt <- list()
	rep.dim.names.alt <- list()
	for(i in seq_along(x)){
		if(!is.list(x[[i]])){
			x[[i]] <- list(x[[i]])
			names(x[[i]]) <- names(x)[i]
		}
		for(j in seq_along(x[[i]])){
			if(is.character(x[[i]][[j]])){
				dx <- dim(x[[i]][[j]])
				x[[i]][[j]] <- as.factor(x[[i]][[j]])
				dim(x[[i]][[j]]) <- dx
			}
			if(is.null(dim(x[[i]][[j]]))) attr(x[[i]][[j]],"dim") <- length(x[[i]][[j]])
		}
		rep.dim.names.alt <- c(rep.dim.names.alt,rep(list(rep.dim.names[[i]]),length(x[[i]])))
		x.alt <- c(x.alt,x[[i]])
	}
	return(list(x=x.alt,rep.dim.names=rep.dim.names.alt))
}

