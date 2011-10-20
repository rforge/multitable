print.data.list <-
function(x, ...){
	
	# simpler to work with x as a 
	# list rather than a data frame
	# for some computations
	xl <- unclass(x)
	
	# print each variable in x
	for(i in seq_along(x)){
		
		# get information about the variable
		subsetdim <- attr(x[[i]],"subsetdim")
		attr(xl[[i]],"subsetdim") <- NULL
		varname <- varnames(x)[i]	# could be out of the loop?
		
		if(is.null(varname)) varname <- i
		
		# print underlined variable name
		cat(paste(varname,":\n",sep=""))
		cat(rep("-",nchar(varname)),"\n",sep="")
		
		# print the variable
		print(x[[i]])
		
		# print replication dimensions for variable
		cat("Replicated along:  || ",
			paste(names(subsetdim)[subsetdim]," || ",
			sep=""),sep="")
		cat("\n\n\n")
	}
	
	# print overall replication dimensions
	cat("REPLICATION DIMENSIONS: \n")
	repdim <- dim(x)
	names(repdim) <- names(subsetdim)
	print(repdim)
}

