\name{ltm.ecol}
\alias{ltm.ecol}
\alias{ltm.ecol.fit}
\alias{print.ltm.ecol}
\alias{screeplot.ltm.ecol}
\alias{predict.ltm.ecol}
\alias{scores.ltm.ecol}
\alias{boxplot.ltm.ecol}
\alias{biplot.ltm.ecol}
\title{Ecological latent trait model}
\description{
Fits the latent trait model for species presence-absence community data described by Walker and Jackson (in press).
}
\usage{
ltm.ecol(Y, full = TRUE, cvic = FALSE, g = 10, 
	lambda = c(0.1,1.1,2.1,3.1,4.1), maxit = 200, 
	digits = 1, verbose = TRUE, B0 = NULL)

ltm.ecol.fit(Y, X, q2, Yv = Y, lambda = c(0.1,1.1,2.1,3.1,4.1),
	maxit = 200, digits = 1, verbose = TRUE, B0 = NULL,
	constr = c("none","none","neg","none","neg"), n = nrow(Y), m = ncol(Y))

\method{print}{ltm.ecol}(x, digits = 1, which.lambda = "optimum", ...)

\method{screeplot}{ltm.ecol}(x, which.lambdas, lambda.cex = 1, lambda.las = 2,
	reg.cex = 0.7, print.reg = TRUE, xlab, ylab, ...)

\method{predict}{ltm.ecol}(object, newdata, responses0 = numeric(0),
	responses1 = c(1), predictors = (1:object$m)[-c(responses0,responses1)],
	which.lambda = "optimum", ...)

\method{scores}{ltm.ecol}(x, newdata, which.lambda = "optimum", which.spp = 1:x$m, ...)

\method{boxplot}{ltm.ecol}(x, newdata = NULL, responses0 = numeric(0),
	 responses1 = 1, predictors = (1:x$m)[-c(responses0,responses1)],
	which.lambda = "optimum", ...)

\method{biplot}{ltm.ecol}(x, newdata, spp = 1:object$m,
	arrows = "none", onespp = FALSE, which.lambda = "optimum",
	g = 75, cex.sites = 1, cex.newsites = 1, ...)
}
\arguments{
  \item{Y}{Sites by species data frame (or matrix).}
  \item{full}{If TRUE, each lambda is used to fit to the full data set.}
  \item{cvic}{If TRUE, each lambda is used to fit to each data set with one observation missing in order to calculate the cross-validation information criterion (CVIC).}
  \item{g}{Number of lattice points along each axis.  For the biplot method, this should be set higher to get better resolution (usually the default of 75 is adequate).  In contrast, for \code{ltm.ecol}, we have found that in practice it is unnecessary to set \code{g} much higher than 10.}
  \item{lambda}{Vector of regularization parameters.  It is STRONGLY recommended that these be given in increasing order.}
  \item{maxit}{Maximum number of EM iterations per model.}
  \item{digits}{Number of digits to retain in the rounding of the log-likelihood when determining convergence.  For \code{print.ltm.ecol}, \code{digits} is the number of digits with which to round estimated model parameters.}
  \item{verbose}{If TRUE, information about the fitting procedure is displayed.}
  \item{B0}{Optional initial value for the coefficient matrix.  If NULL, the default, then correspondence analysis and least-squares are used to (determinstically) generate a starting value.}
  \item{X}{The matrix representing the ordination space (i.e. the lattice). Most users should not need to consider this argument because \code{ltm.ecol.fit} is not meant to be called directly.}
  \item{q2}{The marginal probability for each point in the ordination space.  Most users should not need to consider this argument because \code{ltm.ecol.fit} is not meant to be called directly.}
  \item{Yv}{Validation data.  Most users should not need to consider this argument because \code{ltm.ecol.fit} is not meant to be called directly.}
  \item{constr}{Which constraints to pass to LASSO thresholding.  Most users should not need to consider this argument because \code{ltm.ecol.fit} is not meant to be called directly.}
  \item{n}{Number of sites.  Most users should not need to consider this argument because \code{ltm.ecol.fit} is not meant to be called directly.}
  \item{m}{Number of species.  Most users should not need to consider this argument because \code{ltm.ecol.fit} is not meant to be called directly.}
  \item{x}{fitted \code{ltm.ecol} object.}
  \item{which.lambdas}{Optional vector giving subscripts indexing models to be used.  For example, \code{which.lambdas=c(1,2,4)} indicates that the first two and fourth models are to be used but not the third model.  Each model corresponds to a different value of the regularization parameter, \code{lambda}, and are numbered in the order in which they are supplied to \code{ltm.ecol}.}
  \item{lambda.cex}{Graphics parameter for the lambdas.}
  \item{lambda.las}{Graphics parameter for the lambdas.}
  \item{reg.cex}{Graphics parameter for the lambda title.}
  \item{print.reg}{If TRUE, a second horizontal axis is plotted giving the lambda values.}
  \item{xlab}{x-axis label for screeplots.  A standard label is plotted if missing.}
  \item{ylab}{y-axis label for screeplots.  A standard label is plotted if missing.}
  \item{object}{fitted \code{ltm.ecol} object.}
  \item{newdata}{A data frame containing validation data and the same species (i.e. variables) and species names (i.e. \code{colnames}).}
  \item{responses0}{Vector of indices of species to be predicted as absent.}
  \item{responses1}{Vector of indices of species to be predicted as present.}
  \item{predictors}{Vector of indices of species to be used as predictors.  If \code{predictors} is \code{NULL}, then the marginal distribution is used.}
  \item{which.lambda}{If "optimum", then the model with lowest CIVC is used for predictions, otherwise an index specifying the model to be used.} 
  \item{which.spp}{Vector of indices of species to be used as conditioning variables in calculating axis scores.}
  \item{spp}{Vector of indices of species to be represented in the biplot.}
  \item{arrows}{If "input" the user can use the mouse to draw arrows and these arrow positions will be saved in the object returned by \code{biplot.ltm.ecol}; if "none" no arrows are plotted; if a list returned by a call to \code{biplot.ltm.ecol} with \code{arrows = "input"} then the loaded arrows will be drawn.}
  \item{onespp}{If FALSE then the species in \code{spp} are represented by their 0.5 probability contour line; if TRUE then only one species will be plotted with a variety of probability contour lines, giving more detail about that single species.}
  \item{cex.sites}{Size of the site points.}
  \item{cex.newsites}{Size of the new site points.}
  \item{\dots}{Additional arguments.}
}
\details{
This function uses an EM-algorithm with LASSO (i.e. L1) regularization described by Walker and Jackson (in press).  Our algorithm is based on the ideas of Bock and Aitkin (1981); Woodruff and Hanson (1996); Rizopoulos (2006); Houseman et al. (2007); and Friedman et al. (2010).

There is no guarantee that this algorithm will converge to a global optimum, but it is guaranteed to improve the initial value, \code{B0}, which by default is the correspondence analysis approximation (due to ter Braak 1985) of the maximum likelihood estimates for a fixed-effects version of this model.

Run times can be very long for large data sets, especially when computing CVIC (i.e. \code{cvic=TRUE}).  If computing time is deemed too long, we offer three possible courses of action:  (1) reduce \code{digits} (resulting in convergence further from the local / global optimum), (2) reduce the number of lambda values, (3) simply use \code{lambda=1} (which is a value that we have found works well for a wide variety of ecological data).

\code{ltm.ecol.fit} is not meant to be called directly.

In \code{biplot}, observational units used to fit the model are plotted as circles and those provided as \code{newdata} are plotted as squares.
}
\value{
An object of class \code{ltm.ecol} with components,
\item{B}{A list with each component giving the fitted coefficient matrix for each value of lambda, for the full data.}
\item{L}{A vector with each element giving the log-likelihoods at convergence for each value of lambda, for the full data.}
\item{lambda}{The supplied lambda values.}
\item{deviance}{A vector with each element giving the deviances at convergence for each value of lambda, for the full data.}
\item{complexity}{A vector with each element giving the number of non-zero parameters per species at convergence for each value of lambda, for the full data.}
\item{instab}{A vector with each element corresponding to a particular lambda value.  If an element = 1, it indicates that there was a numerical instability in estimation corresponding to that particular lambda and that the initial values of coefficients were returned rather than the coefficients at the time the algorithm was terminated.  If an element = 0 it indicates no detected numerical instability.}
\item{cvic}{A list with components described below.}
\item{opt.B}{The fitted coefficient matrix for the optimal value of lambda.}
\item{Y}{The supplied data matrix.}
\item{n}{The number of sites.}
\item{m}{The number of species.}
\item{X}{The matrix representing the ordination space (lattice).}
\item{x}{The points at which each axis takes values.}
\item{q}{The marginal probability for each axis.}
\item{q2}{The marginal probability for each point in the ordination space.}

The \code{ltm.ecol$cvic} list has components,
\item{cvic}{A vector with each element giving the cvic at convergence for each value of lambda.}
\item{lambda}{The supplied lambda values.}
\item{instab}{See \code{instab} above.}
\item{logliks}{A matrix giving the cross-validated log likelihoods for each cross-validation iteration (rows) and each value of lambda (columns).}
\item{Bjack}{A list of lists containing the coefficient matrices for each iteration of leave-one-out cross-validation (inner list) and each value of lambda (outer list).  These lists could be useful for computing jackknife estimates of uncertainty, although this is not done here.}
\item{opt.lambda}{The lambda associated with the lowest cvic.}
}
\seealso{
For another implementation of similar latent trait models in \code{R}, see \code{ltm} function in the \code{ltm} package. For random-effects ordination of multivariate-normal data see \code{\link{factanal.predictive}}.
}
\references{
Bock, R.D. and Aitkin, M. (1981) Marginal maximum likelihood estimation of item parameters: application of an EM algorithm. Psychometrika. 46: 443-459.

Friedman, J.H., Hastie, T. and Tibshirani, R. (2010) Regularization paths for generalized linear models via coordinate descent. Journal of Statistical Software 33: 1-22.

Houseman, E.A. et al. (2007) Penalized item response theory models: Application to epigenetic alterations in bladder cancer.  Biometrics. 63: 1269-1277.

Rizopoulos, D. (2006) ltm: An R package for latent variable modeling and item response theory analyses.  Journal of Statistical Software 17: 1-25.

ter Braak, C.J.O (1985) Correspondence analysis of incidence and abundance data: properties in terms of a unimodal response model.  Biometrics. 41: 859-873.

Walker, S.C. and Jackson, D.A. (in press) Random-effects ordination: describing and predicting multivariate correlations and co-occurrences.  Ecological Monographs. 

Woodruff, D.J. and Hanson, B.A. (1996) Estimation of item response models using the EM algorithm for finite mixtures.  Technical report, ACT.
}
\author{
Steve C. Walker
}
\examples{

# fit to Black Hollow fish data
fish.ltm <- ltm.ecol(fish,lambda=1)
print(fish.ltm,which.lambda=1)

# make a biplot with point size of lakes proportional to latitude and 
# contour lines for spp = 9.
spp <- 9
lat <- as.matrix(lat)
cex.sites <- 0.1+((lat - min(lat))/(max(lat)-min(lat)))
biplot(fish.ltm,onespp=TRUE,spp=spp,which.lambda=1,
	cex.sites=cex.sites,cex.newsites=0)

# make a boxplot of fitted probabilities of occurrence for spp = 9.
boxplot(fish.ltm,responses1=spp,
	xlab=paste("Fitted probability of occurrence of species",
	colnames(fish)[spp]),which.lambda=1)

# fit to BCI tree data
data(BCI)
BCI <- (BCI>0)*1
BCI.ltm <- ltm.ecol(BCI,lambda=1)

# plot estimated axis scores on a map
xhat <- scores(BCI.ltm,which.lambda=1)
UTM.EW <- rep(seq(625754, 626654, by=100), each=5)
UTM.NS <- rep(seq(1011569,  1011969, by=100), len=50)
ax <- 1
plot(UTM.EW,UTM.NS,type="n")
points(UTM.EW[xhat[,ax]>0],UTM.NS[xhat[,ax]>0],
	pch=16,cex=xhat[xhat[,ax]>0,ax],col="red")
points(UTM.EW[xhat[,ax]<0],UTM.NS[xhat[,ax]<0],
	pch=16,cex=-xhat[xhat[,ax]<0,ax],col="blue")
title(main=paste("Axis",ax,"in space (red=pos, blue=neg, size=magnitude)"))

}

\keyword{multivariate}
