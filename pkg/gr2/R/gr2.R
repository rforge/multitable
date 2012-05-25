#' Graphical adjustment of R$^2$
#'
#' Graphically adjust sample R$^2$ values
#' 
#' Important function:  \code{\link{graphical.AdjRsqr}}
#'
#' @docType package
#' @name gr2
#' @aliases gr2 package-gr2 gr2-package
NULL

#' Baseline R$^2$
#'
#' Expected R$^2$ under a c-hypothesis
#'
#' @param p Number of parameters
#' @param n Number of samples
#' @param c Adjustment exponent
#' @param R2c c-hypothesis
#' @return Expected sample R$^2$
#' @export
R2.baseline <- function(p, n, c, R2c = 0)
	1 - ((1 - (p/(n-1)))^c)*(1-R2c)

#' Graphical adjusted R$^2$
#' 
#' Diagram for graphically adjusting sample R$^2$ values
#'
#' @param c Adjustment exponent
#' @param R2c Vector of c-hypotheses
#' @return NULL
#' @export
graphical.AdjRsqr <- function(c = 1, R2c = rep(0, length(c))){

	if(length(c) != length(R2c)) stop('each line needs one c and one R2c')

	# we need large left margins to fit the fraction in the y-axis title
	par(mar = c(5, 6, 1, 1))

	# draw the plot region with aspect ratio = 1
	plot(c(0, 1), c(0, 1), 
		type = 'n', bty = 'n', asp = 1, xaxt = 'n', yaxt = 'n',
		xlab = expression(paste('Relative number of predictors, ', italic(r == p/(n-1)))),
		ylab = expression('Unadjusted R'^2 == 1 - over(SS[error], SS[total])))
	
	# draw axes
	axis(1, at = seq(0, 1, 0.2), labels = seq(0, 1, 0.2))
	axis(2, at = seq(0, 1, 0.2), labels = seq(0, 1, 0.2), las = 1)
	
	# make the unit box
	rect(0, 0, 1, 1)
	
	# c exponents must be positive
	R2c <- R2c[c > 0]
	c <- c[c > 0]
	
	# sample size really just controls the smoothness of the lines (so should be large)
	n <- 1000
	
	# draw the lines but don't plot anything that dips below the x-axis (not possible!)
	for(i in seq_along(c)){
		x <- ((1:n)-1)/(n-1)
		y <- R2.baseline((1:n)-1, n, c[i], R2c[i])
		lines(x[y > 0], y[y > 0])
	}
	
}
