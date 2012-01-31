Ops.data.list <- function(e1, e2){
	
	unary <- nargs() == 1L
	FUN <- .Generic
	
	if(FUN == "+"){
		
		# make sure e1 is the larger data list
		if(length(dim(e1)) < length(dim(e2))){
			e3 <- e1
			e1 <- e2
			e2 <- e3
		}

		# put the dims of e2 (the smaller data list) in the same order as the dims
		# in e1 so they can be compared more easily
		e2 <- aperm(e2, as.vector(na.omit(match(dimids(e1), dimids(e2)))), drop = FALSE)

		# find shared dims
		sd1 <- dimids(e1) %in% dimids(e2)
		sd2 <- dimids(e2) %in% dimids(e1)

		dim.length.matching <- mapply(`==`, dim(e1)[sd1], dim(e2)[sd2])

		if(!all(dim.length.matching))
			stop("some shared dimensions do not have the same length in both data lists")

		# names of shared dims
		dn1 <- dimnames(e1)[sd1]
		dn2 <- dimnames(e2)[sd2]
		
		# get permutation vector to rearrange e2 to match the order of dimnames 
		# between e1 and e2
		perml <- mapply(match, dn1, dn2, SIMPLIFY = FALSE)
		
		# find what dimnames in e1 are found nowhere in e2
		find.mismatches <- lapply(perml, is.na)
		complete.mismatches <- sapply(find.mismatches, all)
		
		# if the dims of e2 can be permuted so that its dimnames will align with e1,
		# then do so.  otherwise, trust the user to have them in the correct order.
		if(!any(complete.mismatches))
			e2 <- do.call('[.data.list', c(list(e2), perml, list(vextract = FALSE)), quote = TRUE)
		
		e1 <- unclass(e1)
		e2 <- unclass(e2)
		
		toadd <- !(e2 %in% e1)
		
		l <- c(e1, e2[toadd])
		match.dimids <- c(attr(e1, "match.dimids"), attr(e2, "match.dimids")[toadd])
		
		dl <- as.data.list(l, match.dimids = match.dimids, drop = FALSE)
		names(dl) <- make.names(names(dl), unique = TRUE)
		return(dl) 
	}
	
	if(FUN == "-"){
		tokeep <- !(unclass(e1) %in% unclass(e2))
		if(!any(tokeep)) stop("resulting data list has no variables")
		return(e1[tokeep])
	}
	
	else
		stop(paste(FUN, "method for class data.list not yet writen"))
}

variable <- function(x, dimids, name){
	if(is.data.frame(x))
		x <- as.matrix(x)
	if(is.recursive(x))
		stop("lists not allowed -- perhaps try using variableGroup?")
	out <- data.list(x, match.dimids = list(dimids), drop = FALSE)
	if(!missing(name))
		names(out) <- name[1]
	else
		names(out) <- paste(substitute(x), collapse = ".")
	return(out)
}

variableGroup <- function(x, dimids){
	if(is.data.list(x)) 
		return(x)
	if(is.list(x) && !is.list(x[[1]]))
		x <- list(x)
	if(!is.list(x))
		x <- list(list(x))
	
	as.data.list(x, match.dimids = list(dimids), drop = FALSE)
}

dimids <- function(dl)
	attr(dl, "match.dimids")[[attr(dl, "bm")]]
