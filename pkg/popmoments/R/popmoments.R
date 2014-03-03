#' Population-level moments
#'
#' Simple functions for calculating the population-level moments.
#' 
#' The expectation (\code{\link{E.}}), variance (\code{\link{var.}}), 
#' covariance (\code{\link{cov.}}), correlation (\code{\link{cor.}}), 
#' and regression (\code{\link{reg.}}) operators are provided. Note 
#' that each function ends in a \code{.} to distinguish them from
#' other standard functions with similar names. Convenience functions
#' for other quantities are also provided (e.g. \code{\link{skew.}},
#' \code{\link{coskew.}}, \code{\link{cm.}}, \code{\link{int.}},
#' \code{\link{z.}}).  All of these functions act on vectors
#' without regard to dimension information or any other attributes
#' (including S3 class information). Two functions are special in
#' that they do not act on vectors: \code{\link{Edist}} and
#' \code{\link{cm.}}. \code{\link{Edist}} acts on \code{d-functions}
#' (e.g. \code{\link{dnorm}}), to give (or approximate) the expected
#' value of a (univariate) distribution.  \code{\link{cm.}} acts on
#' data frames to give the mixed central moment of the columns of a
#' data frame.
#'
#' @docType package
#' @name popmoments
#' @aliases popmoments package-popmoments popmoments-package
NULL

#' Population-level expectation
#'
#' Expectation of a vector giving the elements of a population.
#'
#' @param x A numeric vector giving the population.
#' @param p An optional vector of weights.  If missing, equal 
#'  weights are used.
#' @return The expectation of \code{x}.
#' @export
#' @examples
#'  E.(1:10)
#'  E.(1:10, 10:1)
E. <- function(x, p){
	if(missing(p)) return(mean(x))
	if(any(p < 0)) stop('p must contain positive numbers only')
	if(length(x) != length(p)) stop('p must be same length as x')
	p <- p/sum(p)
	return(sum(x*p))
}

#' Population-level variance
#'
#' Variance of a vector giving the elements of a population.
#'
#' @param x A numeric vector giving the population.
#' @param p An optional vector of weights.  If missing, equal 
#'  weights are used.
#' @return The variance of \code{x}.
#' @export
#' @examples
#'  # Note the difference between population and sample variance
#'  var.(1:10)
#'  var(1:10)
var. <- function(x, p) E.(x^2, p) - (E.(x, p)^2)

#' Population-level standard deviation
#'
#' Standard deviation of a vector giving the elements of a population.
#'
#' @param x A numeric vector giving the population.
#' @param p An optional vector of weights.  If missing, equal 
#'  weights are used.
#' @return The standard deviation of \code{x}.
#' @export
sd. <- function(x, p) sqrt(E.(x^2, p) - (E.(x, p)^2))

#' Population-level z-score
#'
#' Transform a vector into z-scores using population mean and standard deviation.
#'
#' @param x A numeric vector giving the population.
#' @param p An optional vector of weights.  If missing, equal 
#'  weights are used.
#' @return The z-scores of \code{x}.
#' @export
z. <- function(x, p) dev.(x, p)/sd.(x, p)

#' Population-level deviation
#'
#' Deviations of the elements of a vector from their population mean.
#'
#' @param x A numeric vector giving the population.
#' @param p An optional vector of weights.  If missing, equal 
#'  weights are used.
#' @return The deviations of \code{x}.
#' @export
dev. <- function(x, p) x - E.(x, p)

#' Population-level squared deviation
#'
#' Squared deviations of the elements of a vector from their population mean.
#'
#' @param x A numeric vector giving the population.
#' @param p An optional vector of weights.  If missing, equal 
#'  weights are used.
#' @return The deviations of \code{x}.
#' @export
dev2. <- function(x, p) dev.(x, p)^2

#' Population-level mean squared deviation
#'
#' Mean squared deviations of the elements of a vector from their population mean.
#'
#' @param x A numeric vector giving the population.
#' @param p An optional vector of weights.  If missing, equal 
#'  weights are used.
#' @return The deviations of \code{x}.
#' @export
Edev2. <- function(x, p) E.(dev2.(x, p), p)


#' Population-level R-squared
#'
#' R-squared
#'
#' @param x A numeric vector giving the population.
#' @param y A numeric vector giving the population.
#' @param p An optional vector of weights.  If missing, equal 
#'  weights are used.
#' @return The R-squared of \code{x} and \code{y}.
#' @export
R2. <- function(x, y, p) cor.(x, y, p)^2

#' Population-level covariance
#'
#' Covariance of two vectors giving the elements of a bivariate
#' population.
#' 
#' @param x A numeric vector giving the first variable of the population.
#' @param y A numeric vector giving the second variable of the population.
#' @param p An optional vector of weights.  If missing, equal 
#'  weights are used.
#' @return The covariance of \code{x} and \code{y}.
#' @export
cov. <- function(x, y, p){
	if(length(x) != length(y)) stop('x must be same length as y')
	return(E.(x*y, p) - (E.(x, p)*E.(y, p)))
} 

#' Population-level coskewness
#'
#' Coskewness of three vectors giving the elements of a trivariate
#' population.  Note that this is the unstandardized third mixed central
#' moment. 
#' 
#' @param x A numeric vector giving the first variable of the population.
#' @param y A numeric vector giving the second variable of the population.
#' @param z A numeric vector giving the third variable of the population.
#' @param p An optional vector of weights.  If missing, equal 
#'  weights are used.
#' @return The coskewness of \code{x} and \code{y} and \code{z}.  This 
#'	is \code{E.((x - E.(x))*(y - E.(y))*(z - E.(z)))}
#' @export
coskew. <- function(x, y, z, p){
	if(length(x) != length(y)) stop('x must be same length as y')
	if(length(x) != length(z)) stop('x must be same length as z')
	if(length(y) != length(z)) stop('y must be same length as z')
	return(E.(dev.(x, p)*dev.(y, p)*dev.(z, p), p))
}

#' Population-level skewness
#'
#' Skewness of a vector giving the elements of a population. Note that this
#' is the unstandardized third central moment.
#' 
#' @param x A numeric vector giving the first variable of the population.
#' @param p An optional vector of weights.  If missing, equal 
#'  weights are used.
#' @return The skewness of \code{x}.  This 
#'  is \code{E.((x - E.(x))^3)}
#' @export
skew. <- function(x, p) E.(dev.(x, p)^3, p)

#' Population-level correlation
#'
#' Correlation of two vectors giving the elements of a bivariate
#' population.
#' 
#' @param x A numeric vector giving the first variable of the population.
#' @param y A numeric vector giving the second variable of the population.
#' @param p An optional vector of weights.  If missing, equal 
#'  weights are used.
#' @return The correlation of \code{x} and \code{y}.
#' @export
cor. <- function(x, y, p) cov.(x, y, p)/sqrt(var.(x, p)*var.(y, p))

#' Population-level regression
#'
#' Regression of one vector on another in a bivariate population.
#' 
#' @param x A numeric vector giving the independent variable of 
#'  the population.
#' @param y A numeric vector giving the dependent variable of the 
#'  population.
#' @param p An optional vector of weights.  If missing, equal 
#'  weights are used.
#' @return The regression of \code{y} and \code{x}.
#' @export
reg. <- function(y, x, p) cov.(x, y, p)/var.(x, p)

#' Population-level regression intercept
#'
#' Intercept in the regression of one vector on another in a bivariate population.
#' 
#' @param x A numeric vector giving the independent variable of 
#'  the population.
#' @param y A numeric vector giving the dependent variable of the 
#'  population.
#' @param p An optional vector of weights.  If missing, equal 
#'  weights are used.
#' @return The intercept associated with \code{x} and \code{y}.
#' @export
int. <- function(y, x, p) E.(y, p) - reg.(y, x, p)*E.(x, p)

#' General population-level mixed central moment
#'
#' Mixed central moment of an arbitrary number of vectors in a multivariate population.
#' 
#' @param df A data frame of numeric vectors of the same length.
#' @param p An optional vector of weights.  If missing, equal 
#'  weights are used.
#' @return The mixed central moment of the variables in \code{df}.
#' @export
cm. <- function(df, p){
  if(missing(p)) p <- rep(1, length(df[[1]]))
  if(any(length(df[[1]]) != sapply(df, length))) stop('all variables must be same length')
  df <- sweep(df, 2, apply(df, 2, E., p = p), '-')
  return(E.(apply(df, 1, prod), p))
}

#' Expected value of a distribution
#'
#' Calculate or approximate the expected value of a distribution.
#' If \code{support} is missing, it is assumed that the support is
#' continuous and \code{\link{integrate}} is used to approximate
#' the expected value.  If the support is countably infinite, then
#' \code{support} should be chosen to reduce the error associated
#' numerically approximating infinite sums.
#'
#' @param dfunc Function that takes a random variable as its first
#'  argument and returns its probability (if \code{support} in not
#'  missing or probability density (if \code{support} is missing).
#' @param lower Lower limit of the continuous \code{support} of the
#'  distribution (only has an effect if \code{support} is missing).
#' @param upper Upper limit of the continuous \code{support} of the
#'  distribution (only has an effect if \code{support} is missing).
#' @param support Values at which to evaluate a distribution with
#'  discrete support.
#' @param ... Additional arguments to pass to \code{dfunc}.
#' @return The value of the expected value of \code{dfunc} (if
#'  \code{support} is not missing) or an approximation to the
#'  expected value supplied by \code{\link{integrate}} (if
#'  \code{support} is missing).
#' @export
#' @examples
#'   Edist(dnorm) # exactly 0
#'   Edist(dpois, lambda = 1, support = 0:3) # bad approximation to 1
#'   Edist(dpois, lambda = 1, support = 0:100) # (numerically) exactly 1
#'   Edist(dbinom, size = 4, prob = 0.51, support = 0:4)
#'   Edist(dgamma, lower = 0, shape = 2.3, scale = 0.2)
#'   Edist(sin, lower = -1, upper = 1)
Edist <- function(dfunc, lower = -Inf, upper = Inf, support, ...){
  integrand <- function(x.){x. * dfunc(x. , ...)}
  if(missing(support)) out <- integrate(integrand, lower, upper)
  else out <- sum(integrand(support))
  return(out)
}
