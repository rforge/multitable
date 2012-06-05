#' Population-level moments
#'
#' Simple functions for calculating the population-level moments.
#' 
#' The expectation (\code{\link{E.}}), variance (\code{\link{var.}}), 
#' covariance (\code{\link{cov.}}), correlation (\code{\link{cor.}}), 
#' and regression (\code{\link{reg.}}) operators are provided. Note 
#' that each function ends in a \code{.} to distinguish them from
#' other standard functions with similar names.
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
var. <- function(x, p) E.(x^2, p) - (E.(x, p)^2)

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

#' Population-level three-variable 'covariance'
#'
#' Covariance of three vectors giving the elements of a trivariate
#' population.
#' 
#' @param x A numeric vector giving the first variable of the population.
#' @param y A numeric vector giving the second variable of the population.
#' @param z A numeric vector giving the third variable of the population.
#' @param p An optional vector of weights.  If missing, equal 
#'  weights are used.
#' @return The 'covariance' of \code{x} and \code{y} and \code{z}.  This 
#'	is \code{E.((x - E.(x))*(y - E.(y))*(z - E.(z)))}
#' @export
cov3. <- function(x, y, z, p){
	if(length(x) != length(y)) stop('x must be same length as y')
	if(length(x) != length(z)) stop('x must be same length as z')
	if(length(y) != length(z)) stop('y must be same length as z')
	return(E.((x - E.(x))*(y - E.(y))*(z - E.(z))))
}

#' Population-level correlation
#'
#' Correlation of two vectors giving the elements of a bivariate
#' population.
#' 
#' @param x A numeric vector giving the first variable of the population.
#' @param y A numeric vector giving the second variable of the population.
#' @param p An optional vector of weights.  If missing, equal 
#'  weights are used.
#' @return The correlation of \code{x} \code{y}.
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
#' @return The correlation of \code{x} \code{y}.
#' @export
reg. <- function(y, x, p) cov.(x, y, p)/var.(x, p)
