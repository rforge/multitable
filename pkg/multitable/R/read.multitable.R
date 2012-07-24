read.multitable <- function(files, dimids, fill = rep(NA,length(files)), ...){
	x <- lapply(files, read.table, header = TRUE, ...)
	dl <- dlcast(x,dimids,fill)
	return(dl)
}

read.multicsv <- function(files, dimids, fill = rep(NA,length(files)),...)
	read.multitable(files, dimids, fill, sep=",", ...)
	
read.multidelim <- function(files, dimids, fill = rep(NA,length(files)),...)
	read.multitable(files, dimids, fill, sep="\t", ...)

read.fourthcorner <- function(community, environment, traits, dimids=c("sites","species"), community.name="abundance",...){
	
	# community data must be a matrix because its got two
	# dimensions of replication (sites by species)
	community <- as.matrix(read.table(community, ...))
	
	# but environment and traits have only a single 
	# dimension respectively (sites for the former and
	# species for the latter), therefore they should
	# stay as data frames
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

read.matrix <- function(...) as.matrix(read.table(...))
