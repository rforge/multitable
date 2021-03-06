#'Fit catch data to several size-selectivity models
#'
#'Compare the fit of catch data to several size-selectivity models fitted by
#'\code{\link{fit.catch}}.
#'
#'
#'@aliases fit.catch.compare plot.fit.catch.compare
#'deviance.fit.catch.compare AIC.fit.catch.compare print.fit.catch.compare
#'summary.fit.catch.compare print.summary.fit.catch.compare
#'@param catch an object of class \code{catch} created using
#'\code{\link{make.catch}}.
#'@param perimeter.factor Factor by which to multiply the inputted mesh sizes
#'to obtain mesh perimeters.
#'@param x an object of class \code{fit.catch.compare} or
#'\code{summary.fit.catch.compare}.
#'@param object an object of class \code{fit.catch.compare}.
#'@param y not used. only for consistency with S3 plot method.
#'@param k not used.  only for consistency with default S3 method.
#'@param tol tolerance used to decide on whether fitted values are numerically
#'zero. used to correct AIC values. this correction depends on sample size, and
#'the standard way to calculate sample size is as the number of mesh sizes
#'times the number of fish length categories and then to subtract from this
#'number the number of zero fitted counts
#'@param omega0 initial value for the tangle parameter
#'@param digits number of digits to round to
#'@param effort TODO (sorry!)
#'@param \dots additional arguments to be passed
#'@return An object of class \code{fit.catch.compare} with components:
#'@return norm.loc \code{fit.catch} object using the \code{norm.loc} selection curve
#'@return norm \code{fit.catch} object using the \code{norm} selection curve
#'@return lognorm \code{fit.catch} object using the \code{lognorm} selection curve
#'@return gamm \code{fit.catch} object using the \code{gamm} selection curve
#'@return inv.gau \code{fit.catch} object using the \code{inv.gau} selection curve
#'@author Steve Walker
#'@export
#'@references J.F. Bromaghin (2005) A versatile net selectivity model, with
#'application to Pacific salmon and freshwater species of the Yukon River,
#'Alaska. Fisheries Research 74: 157-168.
#'
#'R.B. Millar & R.J. Fryer (1999) Estimating the size-selection curves of towed
#'gears, traps, nets and hooks. Reviews in Fish Biology and Fisheries 9:
#'89-116.
#'@examples
#'
#'np <- make.catch(north.pike$MESH, north.pike$FLEN)
#'fms <- fit.catch.compare(np)
#'fms
#'plot(fms)
#'
fit.catch.compare <- function(catch, perimeter.factor = 1, tol = 1e-8, omega0 = 0.1, effort = NULL){
	
	data(sel.curves, package = 'omnr.gillnet')
	
	# fit all models in sel.curves
	fits <- lapply(sel.curves, function(x)
		fit.catch(catch, eval(x),
			perimeter.factor = perimeter.factor, omega0 = omega0, effort = effort
		)
	)
	names(fits) <- sapply(sel.curves, deparse)

	class(fits) = "fit.catch.compare"
	attr(fits, "comparisonresults") <- comparemodels(fits)
	
	# calculate dispersion parameter
	chatlist <- dispparam(fits, tol)
	chat <- chatlist$chat
	
	# adjust residuals and deviances by dispersion parameter
	for(j in seq_along(fits)){
		fits[[j]]$res <- lapply(fits[[j]]$res, "/", chat)
		fits[[j]]$dev <- lapply(fits[[j]]$res, sum)
	}
	
	# fit a null version with flat selection curves

	n.lens <- length(catch$lens)
	n.mesh <- length(catch$mesh)

	y <- vecy <- catch$counts
	vecy <- as.vector(y)
	vecl <- as.vector(catch$lens %x% t(rep(1,n.mesh)))
	f0 <- as.factor(vecl)
	vecy.glmnull <- glm(vecy ~ -1 + f0, family = poisson)
	
	flat <- list()
	flat$mu <- matrix(vecy.glmnull$fitted.values, n.lens, n.mesh, byrow = FALSE)
	flat$res <- (2/chat)*(dpois(y,y,log=TRUE)-dpois(y,flat$mu,log=TRUE))
	flat$dev <- sum(flat$res)
	flat$p <- n.lens
	flat$n <- n.lens * n.mesh
	
	attr(fits, "comparisonresults") <- comparemodels(fits)
	attr(fits, "chatlist") <- chatlist
	attr(fits, "flat") <- flat
	return(fits)
}

dispparam <- function(fits, tol){

	# get the model with the lowest deviance (i.e. global model)
	# for estimating the dispersion parameter
	globalmodel <- fits[[which.min(deviance(fits)[2,])]]

	# number of parameters
	p <- 3 + nrow(globalmodel$catch$counts)


	# run through a few values for the j parameter, which 
	# measures the degree of binning.  we want it to be
	# as close to 5 as possible, unless the resulting df
	# are less than one -- high j unfortunately means low
	# df, so there's a trade-off here
	for(j in seq(5,2,-0.1)){

		# obtain expected counts
		E <- globalmodel$mu$tangle
		
		# obtain observed counts and remove cases with
		# very low expected counts
		O <- globalmodel$catch$counts[E > tol]
		E <- E[E > tol]

		# sort O and E counts to prepare for pooling,
		# which is necessary because chi-square theory
		# works best when each E isn't too small
		O <- O[order(E)]
		E <- E[order(E)]
	
		# separate the count vectors into those with
		# less than j (low counts that should be binned)
		# and those with more than j (counts that are
		# fine)
		#tobin <- E < j
		#Otobin <- O[tobin]
		#Etobin <- E[tobin]
		#Okeep <- O[!tobin]
		#Ekeep <- E[!tobin]

		# create bins...
		#cumE <- cumsum(Etobin)
		#bins <- seq(j, max(cumE) + j, j)
		#binsearch <- sapply(bins ,">", cumE)
		#upperbounds <- apply(binsearch,2,function(x)max(which(x)))
		#lowerbounds <- c(0,upperbounds[-length(upperbounds)]) + 1

	
		Enew <- 0
		Onew <- 0
		Ecum <- 0
		for(k in seq_along(E)){
			if(Ecum > j){
				Ecum <- 0
				Enew <- c(Enew, E[k])
				Onew <- c(Onew, O[k])
			}
			else{
				Enew[length(Enew)] <- Enew[length(Enew)] + E[k]
				Onew[length(Onew)] <- Onew[length(Onew)] + O[k]
			}
			Ecum <- Ecum + E[k]
		}
		
		# ...do the pooling using these bins...
		#Obinned <- mapply(function(u,l)sum(Otobin[l:u]),upperbounds,lowerbounds)
		#Ebinned <- mapply(function(u,l)sum(Etobin[l:u]),upperbounds,lowerbounds)
	
		# ...and create the updated count vectors
		#O <- c(Obinned,Okeep)
		#E <- c(Ebinned,Ekeep)

		O <- Onew
		E <- Enew

		# chi-square effective sample size (number of categories)
		ne <- length(E)

		# use this binning scheme if the sample size is large enough
		if(ne > (p + 1)) break
	
	}
	
	# if the sample size is still not large enough, give up
	if(ne < (p + 1)) stop("insufficient sample size -- more fish needed!")
	
	chi <- sum(((O-E)^2)/E)	# chi square
	df <- ne - p			# degrees of freedom
	chat <- max(1, chi/df)	# dispersion parameter estimate
	return(list(chat = chat, chi = chi, df = df))
}

#' @S3method deviance fit.catch.compare
#' @method deviance fit.catch.compare
#' @rdname fit.catch.compare
#' @export deviance.fit.catch.compare
deviance.fit.catch.compare <- function(object, ...) sapply(object, deviance, ...)

#' @S3method AIC fit.catch.compare
#' @method AIC fit.catch.compare
#' @rdname fit.catch.compare
#' @export AIC.fit.catch.compare
AIC.fit.catch.compare <- function(object, ..., k = 2){
	
	# this method always uses correction...user has no choice
	
	aics <- sapply(object, AIC, corrected = TRUE)
	
	flatmodel <- attr(object,"flat")
	p <- flatmodel$p
	dev <- flatmodel$dev
	n <- flatmodel$n
	nullaic <- dev + (2*p)
	attr(aics, "flat") <- nullaic + ((2*(p+1)*(p+2))/(n-p-2))
	
	return(aics)
}

#' @S3method print fit.catch.compare
#' @method print fit.catch.compare
#' @rdname fit.catch.compare
#' @export print.fit.catch.compare
print.fit.catch.compare <- function(x, digits = max(3, getOption("digits") - 3), ...){
	
	comparisonresults <- attr(x, "comparisonresults")
	devs <- deviance(x)
		
	cat("\nSELECTED MODEL(S): ")
	cat(comparisonresults$bestmodels)
	
	cat("\n\nMODEL DEVIANCES:\n")
    print.default(format(devs, digits = digits), print.gap = 2, 
            quote = FALSE)
    cat("\nCORRECTED AIC DIFFERENCES:\n")
    print.default(format(comparisonresults$AICs, digits = digits), print.gap = 2, 
            quote = FALSE)
	
	cat("\n\nBEST SELECTION CURVE(S): ")
	print(get.best(x))
	
	invisible(x)
}

#' @S3method plot fit.catch.compare
#' @method plot fit.catch.compare
#' @rdname fit.catch.compare
#' @export plot.fit.catch.compare
plot.fit.catch.compare <- function(x, y, ...){
	comparisonresults <- attr(x, "comparisonresults")
	x <- x[comparisonresults$curveindicator]
	if(length(x) > 1){
		opar <- par()
		on.exit(par(opar))
		par(ask = TRUE)
	}
	lapply(x, plot, ...)
}

comparemodels <- function(x){

	AICs <- AIC(x)
	AICs <- AICs - min(AICs)
	AICindicator <- AICs == 0

	modelnames <- outer(rownames(AICs),colnames(AICs),paste)
	bestmodels <- modelnames[AICindicator]
	curveindicator <- colSums(AICindicator) != 0

	out <- list(AICs = AICs, bestmodels = bestmodels, curveindicator = curveindicator)
	return(out)
}

