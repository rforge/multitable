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
#' @param col Line colour
#' @param lwd Line width
#' @param lty Line type
#' @return NULL
#' @export
#' @examples
#' ##########################
#' #
#' # basic idea
#' #
#' ##########################
#' 
#' graphical.AdjRsqr()
#' 
#' text(0.25, 0.75, expression(paste('positive adjusted ', R^2)), adj = 0.5)
#' text(0.75, 0.25, expression(paste('negative adjusted ', R^2)), adj = 0.5)
#' 
#' ##########################
#' #
#' # illustrative example (positive case)
#' #
#' ##########################
#' 
#' graphical.AdjRsqr()
#'
#' R2 <- 0.8
#' R02 <- 0.4
#'
#' d <- 0.03
#' 
#' segments(R02, 0, R02, R02, lty = 3, col = grey(0.7))
#' segments(0, R2, R02, R2, lty = 3, col = grey(0.7))
#' points(R02, R2, pch = 16, cex = 1.5)
#' segments(R02, R02, R02, 1, lwd = 3, lend = 1)
#' segments(R02-d, R02, R02-d, R2, col = grey(0.6), lwd = 3, lend = 1)
#'
#' text(R02-d-d-d-d-0.5*d, (R2+R02)/2, 
#' 	expression('R'^2 - 'R'[0]^2), 
#' 	col = grey(0.6))
#' text(R02+d+d+d, (R2+1)/2, 
#' 	expression(1 - 'R'[0]^2), 
#' 	col = grey(0))		
#'
#' ##########################
#' #
#' # illustrative example (negative case)
#' #
#' ##########################
#' 
#' graphical.AdjRsqr()
#'
#' R2 <- 0.3
#' R02 <- 0.4
#'
#' d <- 0.03
#'
#' points(R02, R2, pch = 16, cex = 1.5)
#' segments(R02, 0, R02, R2, lty = 2)
#' segments(0, R2, R02, R2, lty = 2)
#' segments(R02, R2, R02, 1, lwd = 3, lend = 1)
#' segments(R02-d, R02, R02-d, 1, col = grey(0.6))
#' segments(R02-d-d-d, R02, R02-d-d-d, R2, col = grey(0.6))
#'
#' text(R02-d-d-d-d-d-0.5*d, (R2+R02)/2, 
#' 	expression('R'^2 - 'R'[0]^2), 
#' 	col = grey(0.6))
#' text(R02-d-d-d, (R02+1)/2, 
#'	expression(1 - 'R'[0]^2), 
#'	col = grey(0.6))
#'
#' ##########################
#' #
#' # predictive adjustments 
#' #
#' ##########################
#'
#' cexs <- 0.8
#' ydisp <- 0.06
#' graphical.AdjRsqr(c = c(1, 2))
#' 
#' ypos <- 0.3
#' text(0.7, ypos, expression('R'['adj']^2 < 0), cex = cexs)
#' text(0.7, ypos - ydisp, expression('R'['pred']^2 < 0), cex = cexs)
#' 
#' ypos <- 0.65
#' text(0.5, ypos, expression('R'['adj']^2 > 0), cex = cexs)
#' text(0.5, ypos - ydisp, expression('R'['pred']^2 < 0), cex = cexs)
#' 
#' ypos <- 0.8
#' text(0.2, ypos, expression('R'['adj']^2 > 0), cex = cexs)
#' text(0.2, ypos - ydisp, expression('R'['pred']^2 > 0), cex = cexs) 
graphical.AdjRsqr <- function(c = 1, R2c = rep(0, length(c)),
	col = 'black', lwd = 1, lty = 1){

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
		lines(x[y > 0], y[y > 0], col = col, lwd = lwd, lty = lty)
	}
	
}

#' Simple validated redundancy analysis
#'
#' Calculate the proportion of variation explained in a validation sample 
#' by a redundancy analysis model fitted to a training sample.  This 
#' function can also be thought of as a predictive version of the 
#' \code{simpleRDA2} function in the \code{vegan} package.
#'
#' @param Yt The response matrix in the training sample
#' @param Xt The design matrix in the training sample
#' @param Yv The response matrix in the validation sample
#' @param Xv The design matrix in the validation sample
#' @param ... Not currently used
#' @return A list with two components, \code{Rsquare} and \code{m}, which
#'	contain the predictive R$^2$ and the rank of \code{Xt} as determined
#'	by a \code{\link{qr}} decomposition.
#' @export
validatedRDA2 <- 
function (Yt, Xt, Yv, Xv, ...) 
{
    Q <- qr(Xt, tol = 1e-06)
    Ypred <- Xv %*% qr.coef(Q, Yt)
    Epred <- Yv - Ypred
    
    SSerr <- sum(Epred^2)
    SStot <- sum(Yv^2)
    
    Rsquare <- 1 - (SSerr/SStot)
    list(Rsquare = Rsquare, m = Q$rank)
}
