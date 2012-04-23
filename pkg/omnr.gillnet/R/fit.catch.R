fit.catch <-
function(catch, sel.curve = norm.loc, tangle = TRUE, perimeter.factor = 2, 
	tol = 1e-8, omega0 = 0.1){
	
	#######################################################################
	#
	# fit.catch(): fits catch data to size-selectivity models.  the
	#		strategy is to first fit the curves by log-linear fitting
	#		using glm(), and then (if tangle is TRUE) use the 
	#		SELECT method (of Millar 1992) to fit a slightly modified
	#		model with a tangle parameter. (see Bromaghin 2005 for the 	
	#		concept of tangle parameters).  because both the SELECT 
	#		and log-linear approach yields identical estimates for the
	#		parameters of the selection curve, 
	#
	#
	# catch: an object of class "catch"
	# sel.curve: one of a number of selection curve functions.  these
	#     functions create a range of functions that are required to 
	#     make various calculations.  in effect, the selection curve
	#     chosen defines the model to be fitted.
	# tangle: if TRUE, a tangle parameter is included in the fitting
	#     process.  See Bromaghin (2005).
	# perimeter.factor: factor by which to multiply the inputted mesh 
	#     sizes to obtain mesh perimeter, which are required by the
	#	  analyses (if using bar mesh perimeter.factor = 4; if using
	#	  stretch mesh perimeter.factor = 2, the default) 
	#
	#
	# y: matrix of fish counts
	# vecy: vectorized version of y (the response of the log-linear models)
	# lens: vector of *unique* fish length class midpoints
	# mesh: vector of *unique* mesh sizes
	# n.lens: number of unique fish length classes
	# n.mesh: number of unique mesh sizes
	# l: matrix of lengths (corresponding to y)--n.lens by n.mesh
	# m: matrix of mesh sizes (corresponding to y)--n.lens by n.mesh
	# vecl: vectorized l
	# vecm: vectorized m
	#
	# f: a list with two components (f1 and f2), each giving a vector
	#	with the first two predictors for the log linear model
	# f0: vecl as a factor (for the 'intercept' terms)
	# f1/f2: components of f (see f above)
	# f3: the offset term for the log linear model
	#
	# vecy.glm: the fitted log linear model object
	#
	# 
	# b1/b2: the coefficients for f1 and f2 in the log-linear model
	# theta: a list with one or two (if tangle==TRUE) vectors each
	#	containing the parameters of the selection curve.
	# fit.values: a list with one or two (if tangle==TRUE) matrices of
	#	the fitted values for each model.
	# 
	#
	# 
	# output: a list (of class "fit.catch") to return with fitting
	#	information.
	#
	#
	#######################################################################
	
	
	# store the command used to call fit.catch()
	mc <- match.call()
	
	# organize data
	y <- vecy <- catch$counts	
	lens <- catch$lens
	mesh <- perimeter.factor*catch$mesh
	n.lens <- length(lens)
	n.mesh <- length(mesh)	
	l <- vecl <- lens%x%t(rep(1,n.mesh))
	m <- vecm <- rep(1,n.lens)%x%t(mesh)
	dimnames(l) <- dimnames(m) <- list(lens, mesh)
	
	# put counts in vector form, which is required by glm
	attr(vecy,"dim") <- attr(vecl,"dim") <- attr(vecm,"dim") <- NULL 
	
	### DON'T USE THIS LITTLE CODE BIT ANYMORE ##########################
	# calculate saturated model deviance terms (i.e. deviances for which
	# fitted values = observed values) with which to compare with the fit
	# of the actual models.
	# simply: this will allow correct calculation of model fit statistics	
	#sat.dev.terms <- matrix(0,n.lens,n.mesh)
	#y.cond <- y[y!=0]	# keeps the log on the next line from complaining
						# furthermore, note that when y == 0 it shouldn't
						# contribute to the deviance anyways at least for
						# a poisson model
	#sat.dev.terms[y!=0] <- -2*((y.cond*log(y.cond))-y.cond)
	#sat.dev <- sum(sat.dev.terms)
	#####################################################################
	
	# get log-linear model terms (i.e. transform predictors to conform with
	# millar's 1999 scheme for fitting selection curves using glms)
	f <- sel.curve()$loglinpredictors(vecl,vecm)
	f0 <- as.factor(vecl)
	f1 <- f$f1; f2 <- f$f2
	f3 <- sel.curve()$off(vecl,vecm)
	
	# fit the log-linear model
	vecy.glm <- glm(vecy ~ -1 + f1 + f2 + f0 + offset(f3), family = poisson)
	vecy.qglm <- glm(vecy ~ -1 + f1 + f2 + f0 + offset(f3), family = quasipoisson)

	# store information about the fitted log-linear model
	b1 <- vecy.glm$coefficients[1]  # correspond to millar's (1999) beta 1
	b2 <- vecy.glm$coefficients[2]	# and beta 2
	names(b1) <- names(b2) <- NULL

	theta <- list()	# parameters
	mu <- list()	# fitted values
	res <- list()	# residuals 
	dev <- list()	# deviances 

	theta$notangle <- sel.curve()$selcurvparams(b1,b2)
	mu$notangle <- matrix(vecy.glm$fitted.values,n.lens,n.mesh,byrow=FALSE)
	res$notangle <- 2*(dpois(y,y,log=TRUE)-dpois(y,mu$notangle,log=TRUE))
	dev$notangle <- sum(res$notangle)	
	
	# fit model with tangle parameter if tangle==TRUE
	if(tangle){
		# the zero is the initial logit-scale tangle parameter
		theta0 <- c(log(omega0/(1-omega0)),theta$notangle)
		y.optim <- optim(
			theta0,multdev,l=l,m=m,sel.curve=sel.curve,y=y,hessian=FALSE
		) # hessian = FALSE b/c it isn't used and can cause numerical problems
		theta$tangle <- y.optim$par
		# tangle in 'omega form' which runs from -inf to inf.
		# can be rescaled to be on (0,1), which gives the minimum height of
		# the selection curves to the right of the peaks.
		# !!!! TO DO:  clarify how omega relates to tangle
		phi.hat <- phi(theta$tangle,l,m,sel.curve)
		
		mu$tangle <- sweep(phi.hat,1,rowSums(y,na.rm=TRUE),"*")		
		res$tangle <- 2*(dpois(y,y,log=TRUE)-dpois(y,mu$tangle,log=TRUE))
		dev$tangle <- sum(res$tangle)
		
		theta$tangle[1] <- exp(theta$tangle[1])/(1+exp(theta$tangle[1]))
		names(theta$tangle)[1] <- "omega"
	}
	
	# calculate sample sizes for fitted models, corrected
	# for fitted counts of zero 
	n <- length(y) - sapply(mu, function(x) sum(x < tol))

	output <- list(
		catch=catch,
		sel.curve=sel.curve,
		theta=theta,
		mu=mu,
		res=res,
		dev=dev,
		tangle=tangle,
		perimeter=perimeter.factor, # part of a BUG FIX
								# to pass to plot.fit.catch so that
								# correct perimeter mesh sizes are used
								# to calculate the selection curves
								# to be plotted
		call=mc,
		l=l,
		m=m,
		n=n,
		qglm=vecy.qglm
	)
	class(output) <- "fit.catch"
	return(output)
}

