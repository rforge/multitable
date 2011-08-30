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

read.fourthcorner <- function(Y, X, Z, dnames=c("sites","species"), Yname="abundance",...){
	Y <- as.matrix(read.table(Y, ...))
	X <- read.table(X, ...)
	Z <- read.table(Z, ...)
	match.dnames <- list(dnames,dnames[1],dnames[2])
	dl <- data.list(Y,X,Z,match.dnames=match.dnames)
	names(dl)[1] <- Yname
	return(dl)
}
