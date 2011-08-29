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
	x <- make.dimnames.consistent(x,bm)
	return(x)
}

make.match.dnames <- function(x,dnames){
	match.dnames <- list()
	check <- FALSE
	innames <- lapply(x,get.input.names)
	ulinnames <- unlist(innames,recursive=FALSE)
	notnullnames <- !sapply(ulinnames,is.null)
	if(all(notnullnames)){
		unique.dimnames <- unique(ulinnames)
		mat.ndims <- lapply(innames,match,unique.dimnames)
		indims.wfr <- sapply(unique.dimnames,length)
		check <- TRUE
	}
	else{
		indims <- lapply(x,get.input.dims)
		wfr <- which.max(sapply(indims,length))
		indims.wfr <- indims[[wfr]]
		if(length(unique(indims.wfr)) < length(indims.wfr)){
			stop("Some dimensions are unnamed and some are of the same length and therefore require specification of match.dnames. Type ?data.list and see the details section of the help file for data.list.")
		}
		mat.ndims <- lapply(indims,match,indims.wfr)
		check <- FALSE
	}
	if(missing(dnames)) dnames <- paste("D",seq_along(indims.wfr),sep="")
	match.dnames <- lapply(mat.ndims,function(ii)dnames[ii])
	if(check.full.rep(match.dnames) && check){
		names(x[[1]]) <- dimnames(x[[1]]) <- NULL
		match.dnames <- make.match.dnames(x,dnames)
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

get.input.names <- function(xi){
	if(is.null(dim(xi)) & is.atomic(xi)) return(list(names(xi)))
	else if(is.recursive(xi) & is.atomic(xi[[1]])){
		if(is.null(dim(xi[[1]]))) return(list(names(xi[[1]])))
		else return(dimnames(xi[[1]]))
	}
	else if(is.recursive(xi)) stop("recursive tables must contain only atomic elements")
	else return(dimnames(xi))	
}

check.full.rep <-
function(match.dnames){
	mt <- match.dnames[[which.max(sapply(match.dnames,length))]]
	dims <- lapply(match.dnames,match,table=mt)
	any(sapply(dims,function(x)any(is.na(x))))
}

### NOT USED OR EVEN DONE YET...MAY NOT EVER BE USED
make.varnames <- function(x){
	#vnames <- sapply(as.list(substitute(list(...)))[-1L],as.character)
	deparse.names <- sapply(as.list(substitute(list(...)))[-1L],deparse)
	object.names <- names(x)
	#if(is.null(onames))
	for(i in seq_along(x)){
		if(object.names[i] == "") object.names[i] <- deparse.names[i]
		if(is.recursive(x[[i]])){
			namesi <- names(x[[i]])
			for(j in seq_along(x[[i]])){
				
			}
		}
	}
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
		else if(is.null(names(x[[i]])))
			names(x[[i]]) <- paste(names(x)[i],seq_along(x[[i]]),sep=".")
		for(j in seq_along(x[[i]])){
			dx <- dim(x[[i]][[j]])
			if(is.null(dx)) dnx <- names(x[[i]][[j]])
			else dnx <- dimnames(x[[i]][[j]])
			if(is.character(x[[i]][[j]])){
				x[[i]][[j]] <- as.factor(x[[i]][[j]])
				dim(x[[i]][[j]]) <- dx
			}
			if(is.null(dx)){
				attr(x[[i]][[j]],"dim") <- length(x[[i]][[j]])	
				names(x[[i]][[j]]) <- dnx
			}
			else dimnames(x[[i]][[j]]) <- dnx
		}
		match.dnames.alt <- c(match.dnames.alt,rep(list(match.dnames[[i]]),length(x[[i]])))
		x.alt <- c(x.alt,x[[i]])
	}
	return(list(x=x.alt,match.dnames=match.dnames.alt))
}

which.fully.replicated <-
function(x) which.max(sapply(x,function(xi)length(dim(xi))))

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
	#bm.dimnames <- attr(x[[bm]],"dimnames")
	#for(i in seq_along(x)){
	#	attr(x[[i]],"dimnames") <- bm.dimnames[attr(x[[i]],"subsetdim")]
	#}
	#return(x)
	dimnames(x) <- dimnames(bm(x))
	if(is.null(dimnames(x))) dimnames(x) <- lapply(dim(x),function(di)seq_len(di))
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
	if(is.null(row.names)) row.names <- attr(mold,"df.rownames")
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
	df.rownames <- as.vector(mouter(dimnames(x),FUN=paste,sep="."))
	repdims <- dim(x)
	xmold <- list()
	for(i in seq_along(x)){
		xmold[[i]] <- seq_along(x[[i]])
		dms <- attr(x[[i]],"subsetdim")
		prms <- c(which(dms),which(!dms))
		xmold[[i]] <- aperm(array(xmold[[i]],repdims[prms]),order(prms))
		dim(xmold[[i]]) <- NULL
	}
	structure(xmold,df.rownames=df.rownames)
}

