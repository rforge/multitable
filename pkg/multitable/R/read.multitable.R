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

read.fourthcorner <- function(community, environment, traits, dnames=c("sites","species"), community.name="abundance",...){
	community <- as.matrix(read.table(community, ...))
	environment <- read.table(environment, ...)
	traits <- read.table(traits, ...)
	match.dnames <- list(dnames,dnames[1],dnames[2])
	dl <- data.list(community,environment,traits,match.dnames=match.dnames)
	names(dl)[1] <- community.name
	return(dl)
}

multifile.choose <- function(n){
	sapply(rep(FALSE,n),file.choose)
}
