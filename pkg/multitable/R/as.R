is.data.list <- 
function(x) inherits(x, "data.list")

as.data.list <-
function(x,...) UseMethod("as.data.list")

as.data.list.default <-
function(x,dimids,match.dimids,check = TRUE,drop=TRUE,...){

	# x really needs to be a list!
	if(is.data.list(x)) return(x)
	if((!is.list(x))||is.data.frame(x)) x <- list(x)

	# the make.match.dimids function is an algorithm
	# for automatically generating the pattern of
	# dimension sharing between the objects to be
	# combined into a data list.
	if(missing(match.dimids)) match.dimids <- make.match.dimids(x,dimids)
		
	if(!is.list(match.dimids)) stop("match.dimids must be a list")
	if(length(x)!=length(match.dimids)) stop(
"match.dimids must have the same
number of elements as the list of
objects to be combined into a data
list")
	if(check.full.rep(match.dimids) && check) stop(
"at least one variable must be
replicated along all dimensions")
	
	# some elements of x may themselves be lists of
	# variables (e.g. data frames). but data lists are
	# lists of atomic vectors, matrices, and arrays.
	# therefore, we must first `split up' these lists
	# within x into their constituent variables (i.e.
	# their variables must become elements in x just 
	# like all of the others). the split.dfs performs
	# this spliting. it then combines the result with
	# the match.dimids list in a two-element list of
	# lists (1) x and (2) match.dimids. 
	x <- split.dfs(x,match.dimids)
	
	# one of the rules of data lists is that each data
	# list must have at least one variable that is
	# replicated along all dimensions. we store the
	# index of this variable in bm (for benchmark) so
	# that we can compare the other variables with it.
	# the benchmark `holds together' the other
	# variables.
	bm <- which.fully.replicated(x$x)
	
	# the dimensions of a data list (or repdim's) are
	# simply the dimensions of the benchmark variable.
	repdim <- dim(x$x[[bm]])
	
	# all data list variables MUST have names, so if
	# you won't do it...i will. its just good practice
	# to name your variables. and the check.dims function
	# that comes next requires it in order to give 
	# informative error messages.
	if(is.null(names(x$x))) names(x$x) <- paste("V",seq_along(x$x),sep="")

	# if match.dimids have been specified such that 
	# the lengths of matched dimensions are not 
	# identical, then an invalid data list will result.
	# the check.dims function will give an informative
	# error message about which variable might be
	# causing such a problem.
	if(check) check.dims(x,bm,repdim)

	# all variables in a data list must have an 
	# attribute called "subsetdim", which is a
	# logical vector the same length as repdim
	# that has TRUE for dimensions along which
	# it is replicated and FALSE otherwise. the
	# subsetdim function adds this attribute.
	x <- subsetdim(x,bm,repdim)
	
	# split up match.dimids and x and prepare them
	# for output.
	match.dimids <- x$match.dimids
	names(match.dimids) <- names(x$x)
	x <- structure(x$x, bm = bm, match.dimids = match.dimids,
		repdim = repdim, class = "data.list")
	
	# it is important to make sure that all variables
	# have the same dimnames for shared dimensions.
	x <- make.dimnames.consistent(x,bm)
	
	# data lists with only one dimension of replication
	# are conceptually equivalent to data frames.
	# therefore, the default behaviour is to formally
	# convert such data lists to data frames but this
	# behaviour can be changed via the `drop' argument.
	if((length(repdim)==1) && drop) return(as.data.frame(x))

	return(x)
}

make.match.dimids <- function(x,dimids){
	match.dimids <- list()
	check <- FALSE
	innames <- lapply(x,get.input.names)
	ulinnames <- unlist(innames,recursive=FALSE)
	notnullnames <- !sapply(innames,is.null)
	if(all(notnullnames) && !is.null(ulinnames)){
		unique.dimnames <- unique(ulinnames)
		allunique <- length(unique.dimnames)==length(ulinnames)
		morethanonevar <- length(x) > 1
		if(allunique && morethanonevar)
			stop(
"Resulting data list invalid:
some variables do not share any
dimensions with other variables")
		mat.ndims <- lapply(innames,match,unique.dimnames)
		indims.wfr <- sapply(unique.dimnames,length)
		check <- TRUE
	}
	else{
		indims <- lapply(x,get.input.dims)
		wfr <- which.max(sapply(indims,length))
		indims.wfr <- indims[[wfr]]
		if(length(unique(indims.wfr)) < length(indims.wfr)){
			stop(
"Some dimensions are unnamed and some are 
of the same length and therefore require
specification of match.dimids. Type
?data.list and see the details section
of the help file for data.list.")
		}
		mat.ndims <- lapply(indims,match,indims.wfr)
		check <- FALSE
	}
	if(missing(dimids)) dimids <- paste("D",seq_along(indims.wfr),sep="")
	match.dimids <- lapply(mat.ndims,function(ii)dimids[ii])
	if(check.full.rep(match.dimids) && check){
		names(x[[1]]) <- dimnames(x[[1]]) <- NULL
		match.dimids <- make.match.dimids(x,dimids)
	}
	return(match.dimids)
}

get.input.dims <- function(xi){
	if(is.null(dim(xi)) & is.atomic(xi)) return(length(xi))
	else if(is.recursive(xi) & is.atomic(xi[[1]])){
		if(is.null(dim(xi[[1]]))) return(length(xi[[1]]))
		else return(dim(xi[[1]]))
	}
	else if(is.recursive(xi)) stop(
"recursive tables must contain only
atomic elements")
	else return(dim(xi))
}

get.input.names <- function(xi){
	#if(!all(dim(xi))) stop("some variables ")
	if(is.null(dim(xi)) & is.atomic(xi)) return(list(names(xi)))
	else if(is.recursive(xi) & is.atomic(xi[[1]])){
		if(is.null(dim(xi[[1]]))) return(list(names(xi[[1]])))
		else return(dimnames(xi[[1]]))
	}
	else if(is.recursive(xi)) stop(
"recursive tables must contain
only atomic elements")
	else return(dimnames(xi))	
}

check.full.rep <-
function(match.dimids){
	mt <- match.dimids[[which.max(sapply(match.dimids,length))]]
	dims <- lapply(match.dimids,match,table=mt)
	any(sapply(dims,function(x)any(is.na(x))))
}

split.dfs <-
function(x,match.dimids){
	x.alt <- list()
	match.dimids.alt <- list()
	for(i in seq_along(x)){
		if(!is.list(x[[i]])){
			x[[i]] <- list(x[[i]])
			names(x[[i]]) <- names(x)[i]
		}
		else if(is.null(names(x[[i]])))
			names(x[[i]]) <- paste(names(x)[i],seq_along(x[[i]]),sep=".")
		for(j in seq_along(x[[i]])){
			if(length(x[[i]][[j]])==0)
				stop(
"variables with zero length are
not allowed in data lists")
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
		match.dimids.alt <- c(match.dimids.alt,rep(list(match.dimids[[i]]),length(x[[i]])))
		x.alt <- c(x.alt,x[[i]])
	}
	return(list(x=x.alt,match.dimids=match.dimids.alt))
}

which.fully.replicated <-
function(x) which.max(sapply(x,function(xi)length(dim(xi))))

check.dims <-
function(x,bm,repdim){
	rdn.bm <- x$match.dimids[[bm]]
	for(i in seq_along(x$x)[-bm]){
		for(j in seq_along(x$match.dimids[[i]])){
			repdimij <- repdim[rdn.bm==x$match.dimids[[i]][j]]
			dimij <- dim(x$x[[i]])[j]
			if(repdimij!=dimij) stop(paste("incompatible dimensions in",names(x$x)[i]))
		}
	}
}

subsetdim <-
function(x,bm,repdim){
	inds <- lapply(x$match.dimids,match,table=x$match.dimids[[bm]])
	for(i in seq_along(x$x)){
		ord.inds <- order(inds[[i]])
		x$x[[i]] <- aperm(x$x[[i]],ord.inds)
		x$match.dimids[[i]] <- x$match.dimids[[i]][ord.inds]
	}
	inds <- lapply(x$match.dimids,match,table=x$match.dimids[[bm]])
	nd <- length(repdim)
	for(i in seq_along(x$x)){
		attr(x$x[[i]],"subsetdim") <- rep(FALSE,nd)
		attr(x$x[[i]],"subsetdim")[inds[[i]]] <- TRUE
		names(attr(x$x[[i]],"subsetdim")) <- x$match.dimids[[bm]]
	}
	return(x)
}

make.dimnames.consistent <-
function(x,bm){
	dimnames(x) <- dimnames(bm(x))
	if(is.null(dimnames(x))) dimnames(x) <- lapply(dim(x),function(di)seq_len(di))
	return(x)
}

as.list.data.list <-
function(x, drop.attr=TRUE, factorsTOstrings=FALSE,...){
	if(factorsTOstrings)
		out <- lapply(x,function(xx)
			if(is.factor(xx)) as.character(xx) else xx)
	else out <- unclass(x)
	if(drop.attr){
		for(i in seq_along(x)) attr(out[[i]],"subsetdim") <- NULL
		attr(out,"match.dimids") <- NULL
		attr(out,"bm") <- NULL
		attr(out,"repdim") <- NULL
	}
	return(out)
}

as.data.frame.data.list <-
function(x, row.names = NULL, optional = FALSE, scheme = "repeat", mold, ...){
	if(missing(mold)) mold <- data.list.mold(x)
	out <- lapply(seq_along(x),function(i)as.vector(x[[i]][mold[[i]]]))
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
	x.df <- as.data.frame(x)
	x.num <- x.df[!fac]
	x.fac <- x.df[fac]
	list(x.num=as.matrix(x.num),x.fac=as.matrix(x.fac))
}

data.list.mold <-
function(x){
	df.rownames <- as.vector(mouter(dimnames(x),FUN=paste,sep="."))
	# an alternative to the above line (that reverses the order of the 
	# dimensions) is:
	# df.rownames <- big.kronecker(rev(dimnames(x)),FUN=paste,sep=".")
	
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

