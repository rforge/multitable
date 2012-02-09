dlapply <- function(X, MARGIN, FUN, ...){ #, LFUN, VMODE){
	
	# what to call a new dimension that could
	# be induced by FUN
	newdimname <- as.character(substitute(FUN))
	
	ans <- list()
	dimids.in <- multitable:::dimids(X)
	dimids.out <- dimids.in[MARGIN]
	match.dimids <- attr(X, "match.dimids")
	Xnames <- names(X)
	
	for(i in seq_along(X)){
		margini.logical <-  match.dimids[[i]] %in% dimids.out
		margini <- seq_along(margini.logical)[margini.logical]
		
		# remove variable i if not replicated 
		# along any dimensin in MARGIN
		if(length(margini) == 0){
			message(paste("omiting variable",Xnames[i],"because it is not replicated along dimensions specified by MARGIN"))
			next
		}
		
		# apply FUN to variable i, but remove 
		# the variable if an error occurs
		Xi <- try(apply(X[[i]], margini, FUN, ...), silent = TRUE)
		if(class(Xi) == "try-error"){
			message(paste("omiting variable",Xnames[i],"because of the following error:\n",Xi[1]))
			next
		}

		# calculate the dimids and dimnames for 
		# the new variable
		dimidsi <- match.dimids[[i]][margini]
		if(length(dim(Xi)) > length(margini)){
			dimidsi <- c(newdimname, dimidsi)
			if(is.null(dimnames(Xi)[[1]]))
				dimnames(Xi)[[1]] <- 1:(dim(Xi))[1]
		}
		
		# create new variable
		ans[[i]] <- variable(Xi, dimidsi, Xnames[i])
	}

	# remove any element in ans that isn't a data list and
	# re-order so that the most highly replicated comes first
	# (important when adding data lists to get the order right)
	ans <- ans[sapply(ans, is.data.list)]
	ans <- ans[order(-sapply(lapply(ans, dim), length))]
	
	# try to add the variables together to create a data list, 
	# and if an error occurs return the list of data lists in 
	# out instead (with warning)
	out <- try(Reduce('+', ans), silent = TRUE)
	if(class(out) == "try-error"){
		message("resulting variables could not be combined into a data list:\n returned a list of data lists instead")
		out <- ans
	}
	return(out)
}
