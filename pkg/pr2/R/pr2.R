#' Process data
#' 
#' Function for processing data in \code{pr2}.
#' 
#' @param Y Response matrix
#' @param X Design matrix.
#' @param form Optional one-sided formula for specifying Design matrix
#' @param trans Transformation of the response matrix
#' @param ... More parameters
#' @return List with components:
#' \item{Y}{Transformed response matrix}
#' \item{X}{Model matrix}
#' @export
prDataPro <- function(Y, X, form, trans = 'hellinger', ...){
  
  if(missing(form)) form <- ~ .
  
  Y <- as.matrix(Y)
  n <- nrow(Y)
  m <- ncol(Y)
  
  if(!is.null(trans)) Y <- decostand(Y, method = trans)
  X <- model.frame(form, as.data.frame(X), na.action = NULL)
  X <- model.matrix(form, X)[,-1,drop=FALSE] # form design and drop intercept
  
  cc <- complete.cases(Y, X)
  Y <- Y[cc, , drop = FALSE]
  X <- X[cc, , drop = FALSE]

  # compute intercept
  intercept <- apply(Y, 2, mean)
  
  # remove the need for an intercept
  Y <- sweep(Y, 2, intercept)
  
  # put all predictors on same scale
  X <- sweep(X, 2, apply(X, 2, mean))
  X <- sweep(X, 2, apply(X, 2, sd), '/')
  
  return(list(Y = Y, X = X))

}

#' Ridge redundancy analysis
#' 
#' Redundancy analysis with a ridge penalty.
#' 
#' @param lambda regularization parameter
#' @param Y Response matrix
#' @param X Design matrix.
#' @param form Optional one-sided formula for specifying Design matrix
#' @param trans Transformation of the response matrix
#' @param ... More parameters
#' @rdname ridgeRDA
#' @return List with components:
#' \item{Y}{Transformed response matrix}
#' \item{X}{Model matrix}
#' \item{beta}{Estimated coefficients}
#' @export
ridgeRDA <- function(lambda, Y, X, form, trans = 'hellinger', ...){
  
  if(missing(form)) form <- ~ .
  
  Y <- as.matrix(Y)
  n <- nrow(Y)
  m <- ncol(Y)
  
  if(!is.null(trans)) Y <- decostand(Y, method = trans)
  X <- model.frame(form, as.data.frame(X), na.action = NULL)
  X <- model.matrix(form, X)[,-1,drop=FALSE] # form design and drop intercept
  
  cc <- complete.cases(Y, X)
  Y <- Y[cc, , drop = FALSE]
  X <- X[cc, , drop = FALSE]
  
  # compute intercept
  intercept <- apply(Y, 2, mean)
  
  # remove the need for an intercept
  Y <- sweep(Y, 2, intercept)
  
  # put all predictors on same scale
  X <- sweep(X, 2, apply(X, 2, mean))
  X <- sweep(X, 2, apply(X, 2, sd), '/')
  
  # compute cross-product matrix
  cp <- cpreg <- (t(X) %*% X)
  
  # regularize the cross-product matrix
  diag(cpreg) <- diag(cpreg) + lambda
  
  # invert regularized cross-product matrix
  cpinv <- solve(cpreg)
  
  # calculate coefficient estimates and hat matrix
  cpinvXt <- cpinv %*% t(X)
  beta <- cpinvXt %*% Y
  hat <- X %*% cpinvXt
  
  # compute predictions
  Yhat <- hat%*%Y
  
  # compute the effective degrees of freedom
  # (+1 for sweeping out the mean)
  df <- sum(diag(hat)) + 1
  
  # compute residuals
  E <- Y - Yhat
  SS.E <- sum(E^2)
  
  # compute variances
  sigma2 <- SS.E/(m*(n-df))
  sigma_beta2 <- sigma2 * cpinv %*% cp %*% cpinv
  
  out <- list(Y = Y, X = X, Yhat = Yhat, beta = beta, sigma_beta2 = sigma_beta2, sigma2 = sigma2, intercept = intercept, df = df, hat = hat)
  class(out) <- 'ridgeRDA'
  return(out)  
}

#' @param object \code{ridgeRDA} object
#' @S3method predict ridgeRDA
#' @method predict ridgeRDA
#' @rdname ridgeRDA
#' @export predict.ridgeRDA
predict.ridgeRDA <- function(object, ...) with(object, sweep(Yhat, 2, intercept, '+'))

#' @param x \code{ridgeRDA} object
#' @S3method print ridgeRDA
#' @method print ridgeRDA
#' @rdname ridgeRDA
#' @export print.ridgeRDA
print.ridgeRDA <- function(x, ...) print(x$beta)

#' Simple ridge RDA
#' 
#' Simple redundancy analysis (RDA) using a ridge penalty.
#' 
#' @param lambda Regularization parameter
#' @param Y Response matrix
#' @param X Design matrix
#' @param ... Dots
#' @return A vector with the R^2 and effective degrees of freedom
#' @export
simpleridgeRDA <- function(lambda, Y, X, ...){
  
	# remove the need for an intercept
	Y <- sweep(Y, 2, apply(Y, 2, mean))
	
	# put all predictors on same scale
	X <- sweep(X, 2, apply(X, 2, mean))
	X <- sweep(X, 2, apply(X, 2, sd), '/')
	
	# compute cross-product matrix
	cp <- cpreg <- (t(X) %*% X)
	
	# regularize the cross-product matrix
	diag(cpreg) <- diag(cpreg) + lambda
	
	# compute hat matrix
        cpinv <- solve(cpreg)
	hat <- X %*% cpinv %*% t(X)
	
	# compute the effective degrees of freedom
	# (+1 for sweeping out the mean)
	df <- sum(diag(hat)) + 1

	# compute fitted values and residuals
	Yhat <- hat %*% Y
	E <- Y - Yhat
	
	# compute sum of squares
	SS.E <- sum(E^2)
	SS.T <- sum(Y^2)
	
	# compute R squared
	Rsquare <- 1 - (SS.E/SS.T)
	
	return(c(Rsquare = Rsquare, df = df))
}

#' @export
simpleridgeRDAvalid <- function(lambda, Yt, Yv, Xt, Xv, ...){
  
  Yt <- as.matrix(Yt)
  Yv <- as.matrix(Yv)
  Xt <- as.matrix(Xt)
  Xv <- as.matrix(Xv)
  
  intercept <- apply(Yt, 2, mean)
  
  # remove the need for an intercept
  Yt <- sweep(Yt, 2, intercept)
  Yv <- sweep(Yv, 2, intercept)
  
  # put all predictors on same scale
  Xmean <- apply(Xt, 2, mean)
  Xsd <- apply(Xt, 2, sd)
  Xt <- sweep(Xt, 2, Xmean)
  Xt <- sweep(Xt, 2, Xsd, '/')
  Xv <- sweep(Xv, 2, Xmean)
  Xv <- sweep(Xv, 2, Xsd, '/')
  
  # compute cross-product matrix
  cp <- cpreg <- (t(Xt) %*% Xt)
  
  # regularize the cross-product matrix
  diag(cpreg) <- diag(cpreg) + lambda
  
  # compute hat matrix
  cpinv <- solve(cpreg)
  hat <- Xv %*% cpinv %*% t(Xt)
  
  # compute the effective degrees of freedom
  # (+1 for sweeping out the mean)
  df <- sum(diag(hat)) + 1
  
  # compute fitted values and residuals
  Yhat <- hat %*% Yt
  E <- Yv - Yhat
  
  # compute sum of squares
  SS.E <- sum(E^2)
  SS.T <- sum(Yv^2)
  return(c(SS.E, SS.T))
  
  
  
  # compute R squared
  Rsquare <- 1 - (SS.E/SS.T)
  
  return(Rsquare)
}

#' @export
CVsimpleridgeRDA <- function(lambda, Y, X, method = 'leaveOneRowOut', ...){
  
  if(method != 'leaveOneRowOut') stop('leaveOneRowOut is currently the only available method')
  
  n <- nrow(Y)
  p <- ncol(X)
  SS.ET <- matrix(0, n, 2)
  for(i in 1:nrow(Y))
    SS.ET[i,] <- simpleridgeRDAvalid(0, Y[-i,], Y[i,,drop=FALSE], X[-i,], X[i,,drop=FALSE])
  
  keep <- apply(!is.nan(SS.ET), 1, all)
  MS.ET <- apply(SS.ET, 2, mean, na.rm = TRUE)
  out <- 1 - (MS.ET[1]/MS.ET[2])
  #out <- 1 - (((n-1)/(n-p-1))*((sum(SS.ET[keep, 1])/sum(SS.ET[keep, 2]))))
  dimnames(SS.ET) <- list(rownames(Y), c('error','total'))
  attr(out, 'SS') <- SS.ET
  return(out)
  
  out <- 1 - apply(SS.ET, 1, function(xx) xx[1]/xx[2])
  names(out) <- rownames(Y)
  return(out)
}

#' R^2 regularization path for ridge RDA
#' 
#' @param Y Response matrix
#' @param X Design matrix
#' @param form Optional one-sided formula for specifying Design matrix
#' @param trans Transformation of the response matrix
#' @param lambdas Vector of regularization parameters defining the path
#' @param ... Additional parameters
#' @return A data frame with one row per lambda and columns:
#' \item{lambdas}{lambdas}
#' \item{df}{Effective degrees of freedom (i.e. trace of the hat matrix)}
#' \item{rel.eff.n.params}{Relative effective number of parameters}
#' \item{Rsquare}{Unadjusted R^2}
#' \item{Rsquare.adj}{Adjusted R^2}
#' \item{Rsquare.pred}{Doubly adjusted (i.e. prediction) R^2}
#' @export
Rsqr.regpath <- function(Y, X, form, trans = 'hellinger',
	lambdas = exp(-seq(0, ncol(X) + 1, length.out = 30)), ...){
		
	if(missing(form)) form <- ~ .
	
	Y <- as.matrix(Y)
	n <- nrow(Y)
        m <- ncol(Y)
	
	if(!is.null(trans)) Y <- decostand(Y, method = trans)
	X <- model.frame(form, as.data.frame(X), na.action = NULL)
	X <- model.matrix(form, X)[,-1,drop=FALSE] # make matrix and drop intercept
	
	cc <- complete.cases(Y, X)
	Y <- Y[cc, , drop = FALSE]
	X <- X[cc, , drop = FALSE]
	
	rp <- sapply(lambdas, simpleridgeRDA, Y, X)
	regpath <- as.data.frame(t(rp))
	regpath$lambdas <- lambdas
	regpath <- within(regpath, {
		rel.eff.n.params <- (df*m)/((n*m)-1)
		Rsquare.adj <- vegan:::RsquareAdj.default(Rsquare, n*m, df*m)
		#Rsquare.pred <- vegan:::RsquareAdj.default(Rsquare.adj, n*m, df*m)
                Rsquare.pred <- 1 - ((n*m)/((n*m)-(df*m)))*(1-Rsquare.adj)
	})
	
	regpath <- regpath[c(3, 2, 6, 1, 5, 4)]
	
	class(regpath) <- c('Rsqr.regpath', 'data.frame')
	
	return(regpath)
}

#' Merge two formulas
#' 
#' @param form1 Formula object.
#' @param form2 Formula object.
#' @param func Function for merging the terms in \code{form1} and \code{form2}.
#' @param ... Addition params.
#' @references Formula object.
#' @export
merge.formula <- function(form1, form2, func = union, ...){
  
  # treat double and single sided formulas differently
  if(length(form1) == 2 || length(form2) == 2){
    rhs.index <- 2		
    lhs <- NULL
  }
  else{
    rhs.index <- 3
    
    # get character strings of the names for the responses 
    # (i.e. left hand sides, lhs)
    lhs1 <- deparse(form1[[2]])
    lhs2 <- deparse(form2[[2]])
    if(lhs1 != lhs2) stop('both formulas must have the same response')
    lhs <- lhs1
  }
  
  # get character strings of the right hand sides
  rhs1.char <- deparse(form1[[rhs.index]])
  rhs2.char <- deparse(form2[[rhs.index]])
  
  # split at the plus signs
  rhs1 <- strsplit(rhs1.char, " \\+ ")[[1]]
  rhs2 <- strsplit(rhs2.char, " \\+ ")[[1]]
  
  # merge the right-hand sides in character form
  rhs <- func(rhs1, rhs2)
  
  # put the two sides together with the amazing 
  # reformulate function
  out <- reformulate(rhs, lhs)
  
  # set the environment of the formula (i.e. where should
  # R look for variables when data aren't specified?)
  environment(out) <- parent.frame()
  
  return(out)
}

#' @export
Ops.formula <- function(e1, e2){
  FUN <- .Generic
  if(FUN == '+') out <- merge(e1, e2)
  else if(FUN == '-') out <- merge(e1, e2, setdiff)
  else stop(paste('no', FUN, 'method defined for formula objects yet'))
  
  environment(out) <- parent.frame()
  return(out)
}


#' Variation partitioning regularization path
#' 
#' @param Y Response matrix.
#' @param X1 First predictor matrix.
#' @param X2 Second predictor matrix.
#' @param form1 Formula object for selecting variables from \code{X1}.
#' @param form2 Formula object for selecting variables from \code{X2}.
#' @param trans Transformation (see \code{\link{decostand}}).
#' @param lambdas Vector of regularization parameters.
#' @return Some list or somethng??  FIXME
#' @export
varpart.regpath <- function(Y, X1, X2, form1, form2, trans = 'hellinger', lambdas){
	
  if(missing(lambdas)) lambdas <- c(exp(-seq(0, ncol(X1) + ncol(X2) + 1, length.out = 30)), 0)
  
	if(missing(form1)) form1 <- ~ .
	if(missing(form2)) form2 <- ~ .
	
	ab <- as.matrix(Rsqr.regpath(Y, X1, form1, trans, lambdas)[, 4:6])
	bc <- as.matrix(Rsqr.regpath(Y, X2, form2, trans, lambdas)[, 4:6])
	abc <- as.matrix(Rsqr.regpath(Y, cbind(X1, X2), form1 + form2, trans, lambdas)[, 4:6])
	
	a <- abc - bc
	b <- ab + bc - abc
	c <- abc - ab
	
	out <- abind(a, b, c, ab, bc, abc, along = 0.5)
	dimnames(out)[[1]] <- c('a','b','c','ab','bc','abc')
	out <- aperm(out, c(2, 1, 3))
	attr(out, 'lambdas') <- lambdas
	return(out)
}

#' Plot regularization path of R-squares
#' 
#' @inheritParams plot.Rsqr.regpath
#' @S3method autoplot Rsqr.regpath
autoplot.Rsqr.regpath <- function(object, ...){
  object.melt <- melt(object,
                      id.vars = c('lambdas', 'df', 'rel.eff.n.params'),
                      variable.name = 'Adjustment', value.name = 'Rsquare')
  levels(object.melt$Adjustment) <- c('unadjusted', 'adjusted', 'doubly adjusted')
  
  te <- paste(deparse(substitute(object)), ' regularization paths')
  
  p <- ggplot(object.melt) + 
    geom_line(aes(df, Rsquare, colour = Adjustment)) + 
    labs(x = 'Effective number of parameters') + 
    labs(y = expression(R^2)) +
    labs(title = te) + 
    theme_bw()
  
  return(p)
}


#' Plot regularization path of R-squares
#' 
#' @param x object of class Rsqr.regpath
#' @param y unused.
#' @param ... More stuff.
#' @S3method plot Rsqr.regpath
plot.Rsqr.regpath <- function(x, y, ...){
  xrange <- range(x$df)
  yrange <- with(x, range(c(Rsquare, Rsquare.adj, Rsquare.pred)))
  te <- paste(deparse(substitute(x)), ' regularization paths')
  
  plot(xrange, yrange, type = 'n', 
       xlab = 'Effective number of parameters',
       ylab = expression(R^2), ...)
  title(main = te, adj = 0)
  lines(x$df, x$Rsquare, col = 'red')
  lines(x$df, x$Rsquare.adj, col = 'green')
  lines(x$df, x$Rsquare.pred, col = 'blue')
  
  xl <- grconvertX(1, from = 'ndc')
  yl <- grconvertY(1, from = 'ndc')
  legend(xl, yl, 
         legend = c('unadjusted', 'adjusted', 'doubly adjusted'), 
         col = c('red', 'green', 'blue'), 
         lty = 1, xpd = TRUE, xjust = 1, bty = 'n')
}

#' rbind with identifiers
#' 
#' @param ... objects to bind
#' @return binded object
#' @export
rbind_id <- function(...){
  xnames <- sapply(as.list(match.call())[-1], deparse)
  xs <- mapply(cbind, table = xnames, list(...), SIMPLIFY = FALSE)
  do.call(rbind, xs)
}


#' Character apply
#'
#' Pass several objects through a function where the objects
#' are refered to by character strings.
#'
#' @param objects Character vector with object names
#' @param FUN Function to apply
#' @param ... Additional arguments to pass to FUN
#' @export
capply <- function(objects, FUN, simplify = TRUE, ...){
  
  # differentiate between names (obj.names) and strings (objects)
  # (currently only necessary for naming the elements of the output)
  obj.names <- lapply(objects, as.name)
  
  # apply the function
  out <- lapply(obj.names, function(xx) FUN(eval(xx), ...))
  
  # put characters back in the names attribute
  names(out) <- objects
  
  if(simplify) out <- simplify2array(out)
  return(out)
}

#' Simulate community data for testing R squares
#' 
#' @param n Number of sites
#' @param m Number of species
#' @param p Number of environmental variables
#' @param sigmaE Standard deviation of the error term
#' @param sigmaB Standard deviation of the coefficients relating species to environment
#' @param mu Mean of the coefficients relating species to environment
#' @param cmt Correlation matrix of the environmental variables in the training set
#' @param cmv Correlation matrix of the environmental variables in the validation set
#' @param B \code{p} by \code{m} matrix of coefficients relating species  to environment
#' @param hell Should the Hellinger transformed results be returned?
#' @param untr Should untransformed results be returned?
#' @return A vector with the sample, adjusted, predictive, and validation set R-squares.
#' If both \code{hell} and \code{untr} are \code{TRUE}, then the vector contains 8 values.
#' @family sims
#' @export
Rsqr.sims <- function(
  n = 100, m = 10, p = 10, sigmaE = 1, sigmaB = 1, mu = 0,
  cmt = cor(matrix(rnorm(p*(p+1)), p + 1, p)), 
  cmv = cmt,
  B = matrix(rnorm(m*p, mean = mu, sd = sigmaB), p, m),
  hell = TRUE, untr = TRUE
){
  Xt <- rmv(n, cmt)
  Xv <- rmv(n, cmv)
  
  Et <- matrix(rnorm(n*m, sd = sigmaE), n, m)
  Ev <- matrix(rnorm(n*m, sd = sigmaE), n, m)
  
  Yt <- scale(Xt%*%B + Et)
  Yv <- scale(Xv%*%B + Ev)
  
  if(hell){
    
    Yt.ab <- round(1.2*exp(Yt - 0.5))
    Yv.ab <- round(1.2*exp(Yv - 0.5))
    
    Ht <- vegan:::decostand(Yt.ab, method = 'hellinger')
    Hv <- vegan:::decostand(Yv.ab, method = 'hellinger')
    Ht[is.na(Ht)] <- 0
    Hv[is.na(Hv)] <- 0
    
    Ht <- scale(Ht)
    Hv <- scale(Hv,
                center = attr(Ht, 'scaled:center'),
                scale = attr(Ht, 'scaled:scale'))
    
    R2.0 <- vegan:::simpleRDA2(Ht, Xt)$Rsquare
    R2.1 <- vegan:::RsquareAdj.default(R2.0, n*m, p*m)
    #R2.2 <- vegan:::RsquareAdj.default(R2.1, n*m, p*m)
    R2.2 <- 1 - ((n*m)/((n*m)-(p*m)))*(1-R2.1)
    
    R2.valid <- validatedRDA2(Ht, Xt, Hv, Xv)$Rsquare
    
    out.hell <- c(R2.0, R2.1, R2.2, R2.valid)
  }
  else{
    out.hell <- NULL
  }
  
  if(untr){
    R2.0 <- vegan:::simpleRDA2(Yt, Xt)$Rsquare
    R2.1 <- vegan:::RsquareAdj.default(R2.0, n*m, p*m)
    #R2.2 <- vegan:::RsquareAdj.default(R2.1, n*m, p*m)
    R2.2 <- 1 - ((n*m)/((n*m)-(p*m)))*(1-R2.1)
    
    R2.valid <- validatedRDA2(Yt, Xt, Yv, Xv)$Rsquare
    
    out.untr <- c(R2.0, R2.1, R2.2, R2.valid)
  }
  else{
    out.untr <- NULL
  }
  
  out <- c(out.hell, out.untr)
  
  return(out)
}

#' Replicate community simlations for testing R squares
#' 
#' @param iters Number of replicate communities
#' @param ... Additional arguments to pass to \code{\link{Rsqr.sims}}
#' @return List (or is it a data frame) of class \code{Rsqr.sims} of replicate results.
#' @family sims
#' @export
rep.Rsqr.sims <- function(iters, ...){
  #mc <- match.call()
  #mc[[1]] <- quote(Rsqr.sims)
  #mc$iters <- NULL
  #out <- replicate(iters, eval(mc))
  ar <- list(...)
  out <- replicate(iters, do.call(Rsqr.sims, ar))
  class(out) <- 'Rsqr.sims'
  return(out)
}

#' Summary method for Rsqr.sims
#' 
#' @param object \code{Rsqr.sims} object
#' @S3method summary Rsqr.sims
#' @export
summary.Rsqr.sims <- function(object, ...){
  
  means <- apply(object, 1, mean)
  sds <- apply(object, 1, sd)
  out <- data.frame(means, sds)
  colnames(out) <- c('mean', 'sd')
  rns <- c('R2.0', 'R2.1', 'R2.2', 'R2.valid')
  if(nrow(object) == 8){
    rownames(out) <- paste(rep(c('hell','untr'), rep(4, 2)), rns)
  }
  else{
    rownames(out) <- rns
  }
  
  return(out)
}

#' Print method for Rsqr.sims
#' 
#' @param x \code{Rsqr.sims} object
#' @S3method print Rsqr.sims
#' @export
print.Rsqr.sims <- function(x, ...)
  print(summary(x))
