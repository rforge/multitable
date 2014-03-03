# SIMPLE ALGORITHM:  only accepts and returns data lists -- otherwise an error is thrown
#
# 1. only allow data lists as input
# 2. loop over each variable
# 2a.	calculate margin for this variable
# 2b.	skip variable if not replicated along any dimension in MARGIN
# 2c.	pass this variable to apply
# 2d.	skip variable if there is an error in apply
# 2e.	calculate dimids and dimnames for marginalised variable
# 2f.	convert variable to a data list (with the variable function)
# 2g.	skip variable if there is an error in variable
# 2h.	add variable to an output list
# 3. put all variables into a single data list
# 4. return the marginalised data list unless its not possible, in which case throw an error

dlapply <- function(X, MARGIN, FUN, ...){ #, LFUN, VMODE){

	# maybe try to coerce to data list????
	if(!is.data.list(X))
		stop("X must be a data list")
	
	# allow dimensions to be specified by dim ids -- i.e. names of dimensions
  # (suggested by ben bolker)
	if(is.character(MARGIN)) MARGIN <- pmatch(MARGIN, dimids(X))
  
	# what to call a new dimension that could
	# be induced by FUN
	newdimname <- as.character(substitute(FUN))
	
	ans <- list()
	
	# these lines are now in variable_margins()
	#dimids.in <- dimids(X)
	#dimids.out <- dimids.in[MARGIN]
	#match.dimids <- attr(X, "match.dimids")
	
	# figure out the margins for each variable
	margins <- variable_margins(X, MARGIN)
	
	Xnames <- names(X)
	
	for(i in seq_along(X)){
		
		# these lines are now in variable_margins()
		#margini.logical <-  match.dimids[[i]] %in% dimids.out
		#margini <- seq_along(margini.logical)[margini.logical]
		
		# remove variable i if not replicated 
		# along any dimensin in MARGIN
		if(replication.problem(margins[[i]], Xnames[i])) next
		
		# apply FUN to variable i, but remove 
		# the variable if an error occurs (w some 
		# factor massaging as usual)
		if(is.factor(X[[i]]))
			Xi.info <- factor.info(X[[i]])
		Xi <- try(apply(X[[i]], margins[[i]], FUN, ...), silent = TRUE)
		if(apply.problem(Xi, Xnames[i])) next
		if(is.factor(X[[i]])){
			Xi <- structure(factor(Xi, levels = Xi.info$a.levels, ordered = Xi.info$a.ord), 
				dim = dim(Xi), dimnames = dimnames(Xi))
			for(j in seq_along(Xi.info$other.attr))
				attr(Xi, names(Xi.info$other.attr)[[j]]) <- Xi.info$other.attr[[j]]
		}
		

		# calculate the dimids and dimnames for 
		# the new variable
		dimidsi <- attr(X, 'match.dimids')[[i]][margins[[i]]]
		if(length(dim(Xi)) > length(margins[[i]])){
			dimidsi <- c(newdimname, dimidsi)
			if(is.null(dimnames(Xi)[[1]]))
				dimnames(Xi)[[1]] <- 1:(dim(Xi))[1]
		}
		
		# create new variable (data list variables can only be or certain classes as of now)
		#if(!(is.recursive(Xi)) && inherits(Xi, c("array", "matrix", "factor", "ordered", "numeric", "integer")))
		#	ans[[i]] <- variable(Xi, dimidsi, Xnames[i])
		#else
		#	ans[[i]] <- Xi
		Xi <- try(variable(Xi, dimidsi, Xnames[i]), silent = TRUE)
		if(data.list.problem(Xi, Xnames[i])) next

		ans[[i]] <- Xi
	}

	# if there is nothing to return, just exit
	if(length(ans) == 0)
		return()

	# remove any element in ans that isn't a data list and
	# re-order so that the most highly replicated comes first
	# (important when adding data lists to get the order right)
	#ans <- ans[sapply(ans, is.data.list)]
	#if(length(ans) == 0)
	#	return()
	ans <- ans[order(-sapply(lapply(ans, dim), length))]
	
	# try to add the variables together to create a data list, 
	# and if an error occurs return the list of data lists in 
	# out instead (with warning)
	out <- try(Reduce('+', ans), silent = TRUE)
	if(class(out) == "try-error")
		stop("results could not be combined into a data list")
	return(out)
}

sdlapply <- function(X, MARGIN, FUN, simplify = TRUE, ...){
	
	out <- dlapply(X, MARGIN, FUN, ...)
	
	if(simplify)
		out <- as.data.frame(out)
	
	#if(simplify){
	#	if(is.data.list(out))
	#		out <- as.data.frame(out)
	#	else
	#		out <- lapply(out, as.data.frame)
	#}
	
	return(out)
}


variable_margins <- function(X, MARGIN){
	
	dimids.in <- dimids(X)
	dimids.out <- dimids.in[MARGIN]
	match.dimids <- attr(X, "match.dimids")
	
	matched.vars <- lapply(lapply(match.dimids, function(md) match(dimids.out, md)), na.omit)
	var.dim.id <- lapply(apply(summary(X), 2, sum), seq_len)
	out <- mapply('[', var.dim.id, matched.vars, SIMPLIFY = FALSE)
	
	#out <- lapply(seq_along(X), function(i) {
	#	margini.logical <-  match.dimids[[i]] %in% dimids.out
	#	return(seq_along(margini.logical)[margini.logical])
	#})

	names(out) <- names(X)
	
	return(out)
}


##### Three functions for detecting when variables should be removed from dlapply output:

replication.problem <- function(margini, Xnamesi){
	if(length(margini) == 0){
		message(paste("omitting", Xnamesi,"because it is not replicated along MARGIN"))
		return(TRUE)
	}
	else
		return(FALSE)
}

apply.problem <- function(Xi, Xnamesi){
	if(class(Xi) == "try-error"){
		message(paste("omitting", Xnamesi,"because of the following error:\n",Xi[1]))
		return(TRUE)
	}
	else
		return(FALSE)
}

data.list.problem <- function(Xi, Xnamesi){
	if(class(Xi) == "try-error"){
		message("omitting", Xnamesi,"because it can't be added to a data list")
		return(TRUE)
	}
	else
		return(FALSE)
}
