read.multitable <- function(files, dimids, tnames, fill = rep(NA,length(files)), ...){
	x <- lapply(files, read.table, header = TRUE, ...)
	names(x) <- if(missing(tnames)) make.names(files) else tnames
	dl <- dlcast(x,dimids,fill)
	return(dl)
}

read.multicsv <- function(files, dimids, tnames, fill = rep(NA,length(files)),...)
	read.multitable(files, dimids, tnames, fill, sep=",", ...)
	
read.multidelim <- function(files, dimids, tnames, fill = rep(NA,length(files)),...)
	read.multitable(files, dimids, tnames, fill, sep="\t", ...)

read.fourthcorner <- function(community, environment, traits, dimids=c("sites","species"), community.name="abundance",...){
	community <- as.matrix(read.table(community, ...))
	environment <- read.table(environment, ...)
	traits <- read.table(traits, ...)
	match.dimids <- list(dimids,dimids[1],dimids[2])
	dl <- data.list(community,environment,traits,match.dimids=match.dimids)
	names(dl)[1] <- community.name
	return(dl)
}

multifile.choose <- function(n){
	sapply(rep(FALSE,n),file.choose)
}
