data.list <-
function(...,dnames,match.dnames,check=TRUE){
	x <- list(...)
	if(missing(match.dnames)) match.dnames <- make.match.dnames(x,dnames)
	if(!is.list(match.dnames)) stop("match.dnames must be a list")
	if(length(x)!=length(match.dnames)) stop("match.dnames not the right length")
	if(check.full.rep(match.dnames)) stop(
		"at least one variable must be replicated along all dimensions"
	)
	x <- split.dfs(x,match.dnames)
	bm <- which.fully.replicated(x$x)
	repdim <- dim(x$x[[bm]])
	if(is.null(names(x$x))) names(x$x) <- paste("V",seq_along(x$x),sep="")
	if(check) check.dims(x,bm,repdim)
	x <- subsetdim(x,bm,repdim)
	match.dnames <- x$match.dnames
	x <- x$x
	x <- make.dimnames.consistent(x,bm)
	attr(x,"match.dnames") <- match.dnames
	attr(x,"bm") <- bm
	attr(x,"repdim") <- repdim
	class(x) <- "data.list"
	if(is.null(dimnames(x))) dimnames(x) <- lapply(dim(x),function(di)seq_len(di))
	return(x)
}

which.fully.replicated <-
function(x) which.max(sapply(x,function(xi)length(dim(xi))))

check.full.rep <-
function(match.dnames){
	mt <- match.dnames[[which.max(sapply(match.dnames,length))]]
	dims <- lapply(match.dnames,match,table=mt)
	any(sapply(dims,function(x)any(is.na(x))))
}

check.dims <-
function(x,bm,repdim){
	if(is.null(names(x$x))){
		names(x$x) <- paste("variable",seq_along(x$x))
	}
	rdn.bm <- x$match.dnames[[bm]]
	for(i in seq_along(x$x)[-bm]){
		for(j in seq_along(x$match.dnames[[i]])){
			repdimij <- repdim[which(rdn.bm==x$match.dnames[[i]][j])]
			dimij <- dim(x$x[[i]])[j]
			if(repdimij!=dimij) stop(paste("incompatible dimensions in",names(x$x)[i]))
		}
	}
}

subsetdim <-
function(x,bm,repdim){
	inds <- lapply(x$match.dnames,match,table=x$match.dnames[[bm]])
	for(i in seq_along(x$x)){
		ord.inds <- order(inds[[i]])
		x$x[[i]] <- aperm(x$x[[i]],ord.inds)
		x$match.dnames[[i]] <- x$match.dnames[[i]][ord.inds]
	}
	inds <- lapply(x$match.dnames,match,table=x$match.dnames[[bm]])
	nd <- length(repdim)
	for(i in seq_along(x$x)){
		attr(x$x[[i]],"subsetdim") <- rep(FALSE,nd)
		attr(x$x[[i]],"subsetdim")[inds[[i]]] <- TRUE
		names(attr(x$x[[i]],"subsetdim")) <- x$match.dnames[[bm]]
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
function(x,match.dnames){
	x.alt <- list()
	match.dnames.alt <- list()
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
		match.dnames.alt <- c(match.dnames.alt,rep(list(match.dnames[[i]]),length(x[[i]])))
		x.alt <- c(x.alt,x[[i]])
	}
	return(list(x=x.alt,match.dnames=match.dnames.alt))
}

make.match.dnames <- function(x,dnames){
	indims <- lapply(x,get.input.dims)
	wfr <- which.max(sapply(indims,length))
	indims.wfr <- indims[[wfr]]
	match.dnames <- list()
	if(length(unique(indims.wfr)) < length(indims.wfr)){
		stop("Some dimensions are of same length and therefore require specification of match.dnames. Type ?data.list and see the details section of the help file for data.list.")
	}
	if(missing(dnames)) dnames <- paste("D",seq_along(indims.wfr),sep="")
	for(i in seq_along(indims)){
		match.dnames[[i]] <- dnames[match(indims[[i]],indims.wfr)]
	}
	return(match.dnames)
}

get.input.dims <- function(xi){
	if(is.null(dim(xi)) & is.atomic(xi)) return(length(xi))
	else if(is.recursive(xi) & is.atomic(xi[[1]])){
		if(is.null(dim(xi[[1]]))) return(length(xi[[1]]))
		else return(dim(xi[[1]]))
	}
	else if(is.recursive(xi)) stop("recursive tables must contain only atomic elements")
	else return(dim(xi))
}
