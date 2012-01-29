 #\name{dlcast}
#\description{Cast a list of molten data frames into a data list.}
#  \item{x}{A list of molten data frames}
#  \item{dimids}{See \code{\link{data.list}}}
#  \item{fill}{A vector the same \code{\link{length}} as \code{x}, giving the 
#				value to use for structural missing values for each of the data 
#				frames in \code{x}.}
#
# For the purposes of \code{dlcast}, a molten data frame is a data frame
# with one named column for each variable, one named column for each dimension 
# of replication (called \code{idvar}s in the \code{reshape2} package), and one 
# row for each (possibly multivariate) sample.  Several data frames will allow 
# variables to differ in their dimensions of replication.  However,  in order 
# to combine them into a valid data list:
#	- each data frames must share at least one dimension of replication with at 
#	  least one other data frame and
#	- at least one data frame must contain all of the dimensions of replication 
#	  in all of the data frames.

dlcast <- function(x, dimids, fill=rep(NA,length(x)), placeholders){
	# if x is a data list already, do nothing
	if(is.data.list(x)) return(x)

	# x really has to be a list of data frames!
	if(!is.list(x) || is.data.frame(x)) x <- list(x)
	x <- lapply(x,as.data.frame)
	
	# figure out the names of columns that are dimensions
	# of replication (dims) and the names of columns that
	# are variables (vars)
	xnames <- lapply(x,names)
	dims <- lapply(xnames,intersect,dimids)
	vars <- setdiff(unlist(xnames),dimids)
	vars <- lapply(xnames,intersect,vars)
	
	# cast each data frame into an array with appropirate
	# dimensions
	out <- list()
	for(i in seq_along(x)){
		
		# skip any data frames with no variables
		if(is.null(vars[[i]])) next
		
		# create an array (full) with the correct dimensions
		# for the output array, but fill it with a 
		# concatenation of the dimnames for each cell.
		# (note: the idea behind this implementation here
		# was due to both levi waldron and i through
		# discussions during a trip he took to montreal.)
		dim.namesi <- lapply(dims[[i]], get.dim.names, x)
		full <- mouter(dim.namesi,FUN=paste,sep=".")
		if(is.null(full)) stop("some tables lack identified replication dimensions")
		dfull <- dim(full)
		if(is.null(dfull)) dfull <- length(full)
		
		# concatenate the replication dimension entries 
		# in each row of the data frame. these are the
		# names of the cells that have observations (obs)
		# in the data frame.
		obs <- do.call(paste,c(x[[i]][dims[[i]]],sep="."))
		
		# find the indices (in full) that correspond to
		# actual observed values (in obs)
		mtchs <- match(obs,full)
		
		# fill one array for each variable (they each
		# have the same dimensions and same fill indices)
		out[[i]] <- list()
		for(j in seq_along(vars[[i]]))
			out[[i]][[j]] <- make.arrays(vars[[i]][j],x[[i]],
				mtchs,dfull,dim.namesi,fill[i])
		names(out[[i]]) <- vars[[i]]
	}
	
	out <- as.data.list(out, match.dimids = dims)
	
	if(!missing(placeholders))
		for(i in seq_along(placeholders))
			out <- remove.placeholders(out, placeholders[i])
	
	return(out)
}

# return a vector with the unique names in the columns of x
# with names in dimsi
get.dim.names <- function(dimsi, x){
	#dim.name.list <- lapply(lapply(x,'[[',dimsi),as.character)	# old version doesn't work with zombies
	dim.name.list <- lapply(lapply(lapply(x, '[[', dimsi), as.factor), levels) # works with zombie factors
	#dim.name.list <- lapply(lapply(x, '[[', dimsi), levels) # doesn't work unless dimids columns are factors
	unique(unlist(dim.name.list))
}

remove.placeholders <- function(dl, placeholder){
	dnames <- dimnames(dl)
	ss <- lapply(dnames, `!=`, placeholder)
	do.call(`[.data.list`, c(bquote(dl), ss), quote = FALSE)
}

# vr: a character vector of names of variables
# xi: a data frame with named columns (which should make some names in vr)
# mtchs: a vector of indices for each value in xi[[vr]]
# dfull: dimensions of the output array
# nms: dimnames for the array
# fill: default value for structural missings in the output array
make.arrays <- function(vr,xi,mtchs,dfull,nms,fill){

	# create an array with the correct dimensions that is filled
	# with the default value
	a <- array(fill,dim=dfull)

	# the values of the variable to be put in matching (mtchs) indices
	xv <- xi[[vr]]
	
	# replace the values in the array with those that correspond to
	# values found in xv
	a[mtchs] <- if(is.factor(xv)) as.character(xv) else xv
	
	if(length(dfull)==1) names(a) <- nms[[1]]
	else dimnames(a) <- nms
	
	return(a)
}
