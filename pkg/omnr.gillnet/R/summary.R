#' @param l Lengths
#' @param m Mesh sizes
#' @method summary fit.catch.compare
#' @S3method summary fit.catch.compare
#' @rdname fit.catch.compare
#' @export summary.fit.catch.compare
summary.fit.catch.compare <- function(object, l = get.lens(object), m = get.mesh(object), ...){
	
  l.m <- l.m.mats(l, m)
  l <- l.m$l
  m <- l.m$m
  
	### QAICc ANALYSIS
	
	aics <- aicalt <- AIC(object)
	attr(aicalt, "flat") <- NULL
	vecaics <- c(attr(aics, "flat"), aicalt)
	dvecaics <- vecaics - min(vecaics)
	
	daicalt <- array(dvecaics[-1], dim = dim(aics), dimnames = dimnames(aics))
	daicflat <- dvecaics[1]
	names(daicflat) <- "flat model: "
	
	chat <- attr(object, "chatlist")$chat
	names(chat) <- "dispersion parameter: "
	
	
	### MODEL AVERAGED SELECTION CURVE (i.e. ma.selcurve)
	
	aics <- c(daicflat, t(daicalt))	
	modlikes <- exp(-0.5*aics)
	aweights <- modlikes/sum(modlikes)

	scs <- cbind(1, sapply(object, getTotalSelcurve, l = l, m = m, tangle = FALSE),
		sapply(object, getTotalSelcurve, l = l, m = m, tangle = TRUE))
	
	ma.selcurve <- rowSums(sweep(scs, 2, aweights, FUN = "*"))
	ma.selcurve <- ma.selcurve/max(ma.selcurve)
	
	dim(ma.selcurve) <- c(length(ma.selcurve), 1)
	rownames(ma.selcurve) <- l[,1] # object[[1]]$catch$l
	colnames(ma.selcurve) <- ""
	
	aweightsout <- matrix(aweights[-1], 2,5, byrow = TRUE)
	attr(aweightsout, "flat") <- aweights[1]
	
	out <- list(data.name = object[[1]]$catch$data.name,
		flat.aic = daicflat,
		alt.aic = daicalt,
		chat = chat,
		ma.selcurve = ma.selcurve,
		aweights = aweightsout)
	class(out) <- "summary.fit.catch.compare"
	return(out)
}

print.summary.fit.catch.compare <- function(x, digits = max(3, getOption("digits") - 3), ...){

	chat.print <- paste("over-dispersion parameter: ", format(x$chat, digits = digits))
	daicflat.print <- paste("flat (no selectivity) model dQAICc: ",
		format(x$flat.aic, digits = digits))

	cat("\nSELECTIVITY ANALYSIS OF", x$data.name, "\n")
	cat(rep("-", nchar(x$data.name) + 24),"\n\n",sep="")
	

	cat("QUASI CORRECTED AIC ANALYSIS:\n\n")
	cat(chat.print,"\n\n")
	cat(daicflat.print,"\n\n")
	cat("dQAICc statistics for the selectivity models:\n")
    print.default(format(x$alt.aic, digits = digits), print.gap = 2, 
    	quote = FALSE)
    	
    cat("\n\nMODEL AVERAGED SELECTIVITY CURVE:\n")
    print.default(format(x$ma.selcurve, digits = digits), print.gap = 2,
    	quote = FALSE)
    
    invisible(x)
}
