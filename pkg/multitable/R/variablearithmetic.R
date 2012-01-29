Ops.data.list <- function(e1, e2){
	
	unary <- nargs() == 1L
	FUN <- .Generic
	
	if(FUN == "+"){
		e1 <- unclass(e1)
		e2 <- unclass(e2)
		
		toadd <- !(e2 %in% e1)
		
		l <- c(e1, e2[toadd])
		match.dimids <- c(attr(e1, "match.dimids"), attr(e2, "match.dimids")[toadd])
		
		dl <- as.data.list(l, match.dimids = match.dimids)
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
		names(out) <- as.character(substitute(x))
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
