read.multitable <- function(files, dim.cols, tnames, ..., fill = NULL, drop = TRUE){
	x <- lapply(files, read.table, header = TRUE, ...)
	out <- list()
	#x.names <- lapply(x,names)
	#dim.cols <- lapply(seq_along(x.names),function(dci)as.list(intersect))
	match.dnames <- list()
	for(i in seq_along(x)){
		namesi <- names(x[[i]])
		dim.colsi <- as.list(intersect(namesi,dim.cols))
		var.colsi <- setdiff(namesi,dim.colsi)
		match.dnames[[i]] <- unlist(dim.colsi)
		if(length(var.colsi)>1){
			out[[i]] <- list()
			for(j in seq_along(var.colsi)){
				out[[i]][[j]] <- acast(x[[i]],dim.colsi,value_var=var.colsi[[j]],
					fill=fill[i],drop=drop)
			}
		}
		else out[[i]] <- acast(x[[i]],dim.colsi,value_var=var.colsi,
				fill=fill[i],drop=drop)
	}
	names(out) <- if(missing(tnames)) make.names(files) else tnames
	#try(check.dims(out))
	return(as.data.list(out,match.dnames=match.dnames))
}

#fill.multitable <- function(x,fill){
#	
#}
