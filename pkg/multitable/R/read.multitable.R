read.multitable <- function(files, dim.cols, tnames, ..., fill = NULL, drop = TRUE){
	x <- lapply(files, read.table, header = TRUE, ...)
	out <- list()
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
	return(as.data.list(out,match.dnames=match.dnames))
}
