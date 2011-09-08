dlcast <- function(x,dimids,fill=rep(NA,length(x))){
	if(!is.list(x) || is.data.frame(x)) x <- list(x)
	xnames <- lapply(x,names)
	dims <- lapply(xnames,intersect,dimids)
	vars <- setdiff(unlist(xnames),dimids)
	vars <- lapply(xnames,intersect,vars)
	out <- list()
	for(i in seq_along(x)){
		if(is.null(vars[[i]])) next
		dim.namesi <- lapply(dims[[i]],get.dim.names,x)
		full <- mouter(dim.namesi,FUN=paste,sep=".")
		dfull <- dim(full)
		if(is.null(dfull)) dfull <- length(full)
		obs <- do.call(paste,c(x[[i]][dims[[i]]],sep="."))
		mtchs <- match(obs,full)
		out[[i]] <- list()
		for(j in seq_along(vars[[i]]))
			out[[i]][[j]] <- make.arrays(vars[[i]][j],x[[i]],
				mtchs,dfull,dim.namesi,fill[i])
		names(out[[i]]) <- vars[[i]]
	}
	out <- as.data.list(out,match.dimids=dims)
	return(out)
}

mouter <- function(x,...){
	spouter <- function(x,y) outer(x,y,...)
	Reduce("spouter",x)
}

get.dim.names <- function(dimsi,x){
	dim.name.list <- lapply(lapply(x,'[[',dimsi),as.character)
	dim.names <- unique(unlist(dim.name.list))
	dim.names
}

make.arrays <- function(vr,xi,mtchs,dfull,nms,fill){
	a <- array(fill,dim=dfull)
	xv <- xi[[vr]]
	a[mtchs] <- if(is.factor(xv)) as.character(xv) else xv
	if(length(dfull)==1) names(a) <- nms[[1]]
	else dimnames(a) <- nms
	return(a)
}