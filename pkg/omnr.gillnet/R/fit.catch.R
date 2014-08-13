#'Fit catch data to size-selectivity models
#'
#'First fits selection curves by log-linear fitting using \code{\link{glm}} and
#'then (if tangle is TRUE) uses the SELECT method (of Millar 1992) to fit a
#'slightly modified model with a tangle parameter (see Bromaghin 2005 for the
#'concept of tangle parameters).
#'
#'
#'@aliases fit.catch plot.fit.catch deviance.fit.catch AIC.fit.catch
#'print.fit.catch coef.fit.catch coefficients.fit.catch
#'@param catch an object of class \code{catch} created using
#'\code{\link{make.catch}}.
#'@param sel.curve one of a number of selection curve function including
#'norm.loc, norm, lognorm, gamm and inv.gau.  These functions create a range of
#'functions that are required to make various calculations.  In effect, the
#'selection curve chosen defines the model to be fitted.
#'@param tangle If TRUE, a tangle parameter is included in the fitted process
#'(Bromaghin (2005).
#'@param perimeter.factor Factor by which to multiply the inputted mesh sizes
#'to obtain mesh perimeters, which are required by the analyses.  (if using bar
#'mesh perimeter.factor = 2; if using stretch mesh perimeter.factor = 1, the
#'default).
#'@param x an object of class \code{fit.catch}.
#'@param object an object of class \code{fit.catch}.
#'@param y not used. only for consistency with S3 plot method.
#'@param xlab x-axis label
#'@param ylab y-axis label
#'@param resolution higher numbers make smoother selection curves
#'@param plot.type type of plot to produce, either "selcurve", "totalselcurve",
#'or "residuals" for selection curves, total selection curve, or residual plot;
#'in this last type, the sizes of the circles are proportional to deviance
#'@param max.cex maximum size of the deviance circles
#'@param min.cex minimum size of the deviance circles
#'@param leg.pos position of the legend if necessary, see \code{\link{legend}}.
#'@param data.name name of the data set to be plotted
#'@param justtangle should only the tangle model be plotted?
#'@param corrected if TRUE, the small-sample-size corrected version of AIC is
#'returned (recommended)
#'@param k not used.  only for consistency with default S3 method.
#'@param tol tolerance used to decide on whether fitted values are numerically
#'zero. used to correct AIC values. this correction depends on sample size, and
#'the standard way to calculate sample size is as the number of mesh sizes
#'times the number of fish length categories and then to subtract from this
#'number the number of zero fitted counts
#'@param omega0 initial value for the tangle parameter
#'@param digits number of digits to round to
#'@param \dots additional arguments to be passed
#'@param effort TODO (sorry!)
#'@return An object of class \code{fit.catch} with components:
#'@return catch the original \code{catch} object
#'@return sel.curve the original selection curve
#'@return theta the fitted parameters of the selection curve
#'@return mu the fitted values
#'@return res the deviance residuals (see Millar and Fryer 1999)
#'@return dev model deviance(s)
#'@return tangle TRUE if tangle parameter fitted, FALSE otherwise
#'@return call the matched call
#'@return l the 'length matrix'.
#'@return m the 'mesh matrix' (with input mesh sizes multiplied by \code{perimeter.factor}).
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
#'data(north.pike)
#'np <- make.catch(north.pike$MESH, north.pike$FLEN)
#'plot(np)
#'(np.fit <- fit.catch(np, gamm))
#'plot(np.fit)
#'
fit.catch <-
function(catch, sel.curve = norm.loc, tangle = TRUE, perimeter.factor = 1, 
	tol = 1e-8, omega0 = 0.1, effort = NULL){
	
	# store the command used to call fit.catch()
	mc <- match.call()
        
	# organize data
	y <- vecy <- catch$counts	
	lens <- catch$lens
	mesh <- perimeter.factor*catch$mesh
	n.lens <- length(lens)
	n.mesh <- length(mesh)	
	l <- vecl <- cbind.rep(lens, n.mesh) # lens%x%t(rep(1,n.mesh))
	m <- vecm <- rbind.rep(mesh, n.lens) # rep(1,n.lens)%x%t(mesh)
	dimnames(l) <- dimnames(m) <- list(lens, mesh)

        # process effort if necessary
        if(!is.null(NULL)){
          if(any(effort<0)) stop("effort must be non-negative")
          veceffort <- cbind.rep(effort/sum(effort), n.mesh)
        }
        else{
          veceffort <- matrix(1/n.mesh,n.lens,n.mesh)
        }
        
	# put counts in vector form, which is required by glm
	attr(vecy,"dim") <- attr(vecl,"dim") <- attr(vecm,"dim") <- attr(veceffort,"dim") <- NULL 
	
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
	vecy.glm <- glm(vecy ~ -1 + f1 + f2 + f0 + offset(f3 + log(veceffort)), family = poisson)
	vecy.qglm <- glm(vecy ~ -1 + f1 + f2 + f0 + offset(f3 + log(veceffort)), family = quasipoisson)

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

#' @S3method AIC fit.catch
#' @method AIC fit.catch
#' @rdname fit.catch
#' @export AIC.fit.catch
AIC.fit.catch <-
  function(object, corrected=TRUE, ..., k=2){
    
    # get number of parameters, but only keep the first
    # if only the no tangle model was fitted
    p <- c(2,3)+length(object$catch$lens)
    if(!object$tangle) p <- p[1]
    
    # calculate AIC and (maybe) correct for small sample sizes
    output <- deviance(object) + (2*p)
    if(corrected) # correction STRONGLY recommended
      output <- output + ((2*(p+1)*(p+2))/(object$n-p-2))
    
    return(output)
  }

#' @S3method coef fit.catch
#' @method coef fit.catch
#' @rdname fit.catch
#' @export coef.fit.catch
coef.fit.catch <- function(object, ...){
  object$theta
}

#' @method coefficients fit.catch
#' @rdname fit.catch
#' @export coefficients.fit.catch
coefficients.fit.catch <- function(object, ...){
  object$theta 
}

#' @S3method print fit.catch
#' @method print fit.catch
#' @rdname fit.catch
#' @export print.fit.catch
print.fit.catch <-
  function(x,digits = max(3, getOption("digits") - 3),...){
    
    theta <- coef(x)
    
    cat("\nCall:\n", deparse(x$call), "\n\n", sep = "")
    
    cat("SELECTION CURVE: ",x$sel.curve()$name(),"\n\n")
    
    cat("SELECTION CURVE PARAMETERS:\n(without tangle parameter):\n")
    print.default(format(theta$notangle, digits = digits), print.gap = 2, 
                  quote = FALSE)
    if(x$tangle){
      cat("(with tangle parameters):\n")
      print.default(format(theta$tangle, digits = digits), 
                    print.gap = 2,quote = FALSE)
    }
    
    cat("\nMODEL DEVIANCE:\n")
    print.default(format(deviance(x), digits = digits), print.gap = 2, 
                  quote = FALSE)
    cat("\nCORRECTED AIC:\n")
    print.default(format(AIC(x), digits = digits), print.gap = 2, 
                  quote = FALSE)
    
    invisible(x)
  }

#' @S3method plot fit.catch
#' @method plot fit.catch
#' @rdname fit.catch
#' @export plot.fit.catch
plot.fit.catch <- function(x, y, xlab = "Fork length (mm)", 
                           ylab = "Relative selection probability", 
                           resolution = 500, 
                           plot.type = c("selcurve", "totalselcurve", "residuals", "observedvrsexpected"), 
                           max.cex = 2, min.cex = 0.5, leg.pos = "topleft", data.name = NULL, 
                           justtangle = TRUE, ...){
  
  if(justtangle && (!x$tangle)){
    warning("no fitted tangle parameter, changing justtangle = FALSE")
    justtangle <- FALSE
  }
  
  if(is.null(data.name)) data.name <- x$catch$data.name
  
  plot.type <- match.arg(plot.type)
  
  switch(plot.type,
         selcurve = {
           n.mesh <- length(x$catch$mesh)
           
           up <- max(x$catch$lens)
           lw <- min(x$catch$lens)
           l <- seq(lw,up,(up-lw)/(resolution-1))
           l <- l%x%t(rep(1,n.mesh))
           m <- rep(1,resolution)%x%t(x$catch$mesh*x$perimeter)  # BUG FIX:
           # added the perimeter factor
           # multiplication
           
           if(x$tangle) par(mfrow=c(2,1),mar=c(4,4,2,1))
           else par(mfrow=c(1,1),mar=c(4,4,2,1))
           xx <- range(x$catch$lens)
           yy <- c(0,1)
           plot(xx,yy,type="n",xlab=xlab,ylab=ylab,las=1,...)
           title(main=paste(data.name,"(",x$sel.curve()$name(),", no tangle parameter)"))
           
           r <- getSelcurve(x, l, m, tangle = FALSE)
           
           for(i in 1:n.mesh){
             lines(l[,1],r[,i],col=i)
           }
           if(x$tangle){
             plot(xx,yy,type="n",xlab=xlab,ylab=ylab,las=1,...)
             title(main=paste(data.name,"(",x$sel.curve()$name(),", with tangle parameter)"))
             
             r <- getSelcurve(x, l, m, tangle = TRUE)
             for(i in 1:n.mesh){
               lines(l[,1],r[,i],col=i)
             }
           }
           
         },
         totalselcurve = {
           selcurve.notangle <- getTotalSelcurve(x, tangle = FALSE)
           
           plot(x$catch$lens, selcurve.notangle, ylim = c(0,1),
                type="l", xlab = xlab, ylab = ylab, las = 1, ...)
           title(main =
                   paste(data.name," total selection curve (",x$sel.curve()$name(), ")", 
                         sep = ""))
           
           if(x$tangle){
             selcurve.tangle <- getTotalSelcurve(x, tangle = TRUE)
             lines(x$catch$lens, selcurve.tangle, lty = 2)
             legend(leg.pos, legend = c("no", "yes"), 
                    title = "tangle?",
                    lty = c(1,2), bty = "n")
           }
         },
         residuals = {
           
           if(justtangle){
             par(mfrow=c(1,1))
             grandmax <- max(x$res$tangle)
             scaleddevs <- x$res$tangle/grandmax
             cexs <- (max.cex*scaleddevs) + (min.cex*(1-scaleddevs))
           }
           else if(x$tangle){
             par(mfrow=c(2,1),mar=c(4,4,2,1))
             grandmax <- max(c(x$res$notangle,x$res$tangle))
             scaleddevs <- x$res$notangle/grandmax
             cexs <- (max.cex*scaleddevs) + (min.cex*(1-scaleddevs))
           }
           else{
             par(mfrow=c(1,1))
             grandmax <- max(c(x$res$notangle))
             scaleddevs <- x$res$notangle/grandmax
             cexs <- (max.cex*scaleddevs) + (min.cex*(1-scaleddevs))
           }
           plot(x$l,x$m,col=grey(1-cexs),pch=16,#cex=cexs,
                xlab="Fish length classes",ylab="Mesh sizes")
           title(main=paste(data.name,"(",x$sel.curve()$name(),", no tangle parameter)"))
           
           if(x$tangle && (!justtangle)){
             scaleddevs <- x$res$tangle/grandmax
             cexs <- (max.cex*scaleddevs) + (min.cex*(1-scaleddevs))
             plot(x$l,x$m,col=grey(1-cexs),pch=16,#cex=cexs,
                  xlab="Fish length classes",ylab="Mesh sizes")
             title(main=paste(data.name,"(",x$sel.curve()$name(),", with tangle parameter)"))
           }
         },
         observedvrsexpected = {
           if(x$tangle) par(mfrow=c(2,1),mar=c(4,4,2,1))
           else par(mfrow=c(1,1),mar=c(4,4,2,1))
           
           plot(x$mu$notangle, x$catch$counts,
                xlab = "Fitted values",
                ylab = "Observed values")
           title(main=paste(data.name,"(",x$sel.curve()$name(),", no tangle parameter)"))
           xr <- range(x$mu$notangle)
           xs <- seq(xr[1],xr[2],1/resolution)
           pe <- cbind(qpois(0.05/2,xr), qpois(1-0.05/2,xr))
           lines(xr,pe[,1], lty = 2)
           lines(xr,pe[,2], lty = 2)
           abline(a=0,b=1)
           
           if(x$tangle){
             plot(x$mu$tangle, x$catch$counts,
                  xlab = "Fitted values",
                  ylab = "Observed count data")
             title(main=paste(data.name,"(",x$sel.curve()$name(),", with tangle parameter)"))
             xr <- range(x$mu$tangle)
             xs <- seq(xr[1],xr[2],1/resolution)
             pe <- cbind(qpois(0.05/2,xr), qpois(1-0.05/2,xr))
             lines(xr,pe[,1], lty = 2)
             lines(xr,pe[,2], lty = 2)
             abline(a=0,b=1)
           }
         }
  )
}

#' @S3method deviance fit.catch
#' @method deviance fit.catch
#' @rdname fit.catch
#' @export deviance.fit.catch
deviance.fit.catch <-
  function(object,...){
    if(object$tangle){
      output <- sapply(object$dev,function(x)x)
      names(output) <- c("no tangle","tangle")
    }
    else{
      output <- object$dev$notangle
    }
    return(output)
  }
