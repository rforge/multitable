read.multitable <- function(files, dnames, tnames, fill = rep(NA,length(files)), ...){
	x <- lapply(files, read.table, header = TRUE, ...)
	names(x) <- if(missing(tnames)) make.names(files) else tnames
	dl <- dlcast(x,dnames,fill)
	return(dl)
}

read.multicsv <- function(files, dnames, tnames, fill = rep(NA,length(files)),...)
	read.multitable(files, dnames, tnames, fill, sep=",", ...)
	
read.multidelim <- function(files, dnames, tnames, fill = rep(NA,length(files)),...)
	read.multitable(files, dnames, tnames, fill, sep="\t", ...)
