print.data.list <-
function(x, ...){
	
	repdim <- dim(x)
	x <- unclass(x)
	
	# print each variable in x
	for(i in seq_along(x)){
		
		# get information about the variable
		subsetdim <- attr(x[[i]],"subsetdim")
		attr(x[[i]], "subsetdim") <- NULL
		attr(x[[i]], "contrasts") <- NULL
		#varname <- varnames(x)[i]	# could be out of the loop?
		varname <- names(x)[i]
		
		if(is.null(varname)) varname <- i
		
		# print underlined variable name
		cat(paste(varname,":\n",sep=""))
		cat(rep("-",nchar(varname)),"\n",sep="")
		
		# print the variable (the call to [ is intended to remove attributes)
		print(do.call(`[`, c(list(x[[i]]), lapply(dim(x[[i]]), seq.int))))
		#print(x[[i]]) # older version of the above line
		
		# print replication dimensions for variable
		cat("Replicated along:  || ",
			paste(names(subsetdim)[subsetdim]," || ",
			sep=""),sep="")
		cat("\n\n\n")
	}
	
	# print overall replication dimensions
	cat("REPLICATION DIMENSIONS: \n")
	#repdim <- dim(x)
	names(repdim) <- names(subsetdim)
	print(repdim)
}


