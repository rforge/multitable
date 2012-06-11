fit.catch.compare <- function(catch, perimeter.factor = 2, tol = 1e-8, omega0 = 0.1){
	
	data(sel.curves, package = 'omnr.gillnet')
	
	# fit all models in sel.curves
	fits <- lapply(sel.curves, function(x)
		fit.catch(catch, x, perimeter.factor = perimeter.factor, omega0 = omega0))

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

deviance.fit.catch.compare <- function(object, ...) sapply(object, deviance, ...)

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
	print(x[comparisonresults$curveindicator])
	
	invisible(x)
}

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

