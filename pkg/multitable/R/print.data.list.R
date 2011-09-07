print.data.list <-
function(x, ...){
	for(i in seq_along(x)){
		subsetdim <- attr(x[[i]],"subsetdim")
		attr(x[[i]],"subsetdim") <- NULL
		varname <- varnames(x)[i]	# could be out of the loop?
		if(is.null(varname)) varname <- i
		cat(paste(varname,":\n",sep=""))
		cat(rep("-",nchar(varname)),"\n",sep="")
		print(x[[i]])
		cat("Replicated along:  || ",
			paste(names(subsetdim)[subsetdim]," || ",
			sep=""),sep="")
		cat("\n\n\n")
	}
	cat("REPLICATION DIMENSIONS: \n")
	repdim <- dim(x)
	names(repdim) <- names(subsetdim)
	print(repdim)
}

