\name{factanal.predictive}
\alias{factanal.predictive}
\alias{AIC.factanal.predictive}
\alias{biplot.factanal.predictive}
\alias{deviance.factanal.predictive}
\alias{logLik.factanal.predictive}
\alias{pairs.factanal.predictive}
\alias{predict.factanal.predictive}
\alias{scores.factanal.predictive}
\alias{screeplot.factanal.predictive}
\title{
Probabilistically predictive factor analysis
}
\description{
A wrapper function for using the \code{\link{factanal}} function predictively.  Several methods are available for exploring the predictions of fitted factor analysis models.
}
\usage{
factanal.predictive(x, factors, ...)

\method{AIC}{factanal.predictive}(object, ..., k = 2, corrected = TRUE)

\method{biplot}{factanal.predictive}(x, newdata, axes=c(1,2), 
which.var=1:x$n.vars, ...)

\method{deviance}{factanal.predictive}(object, ...)

\method{logLik}{factanal.predictive}(object, ...)

\method{pairs}{factanal.predictive}(x, vars = 1:x$n.vars, ls = FALSE, ...)

\method{predict}{factanal.predictive}(object, newdata, responses = c(1), 
predictors = (1:object$n.vars)[-responses], ...)

\method{scores}{factanal.predictive}(x, newdata, which.var=1:x$n.vars, ...)

\method{screeplot}{factanal.predictive}(x, stat = "deviance", shift = TRUE, 
plot = TRUE, ylab, main, grey.int=0.8, ...)

}
\arguments{
  \item{x}{For \code{factanal.predictive}:  A data frame, numeric matrix or any object that can be coerced into a numeric matrix.  Unlike \code{\link{factanal}}, this argument cannot be a formula or missing.  For methods:  An object of class \code{factanal.predictive}.}
  \item{object}{An object of class \code{factanal.predictive}.}
  \item{factors}{Number of factors to be fitted.}
  \item{k}{Not used.  Included for consistency with the default method.}
  \item{corrected}{If \code{TRUE} the returned information criterion is corrected for small multivariate samples (called MAICc).  If \code{FALSE} the standard AIC is returned.}
  \item{axes}{Vector of length 2 containing the numbers associated with the axes to be plotted.  If the model contains only a single factor (i.e. axis), then random 'jitter' is plotted on the y-axis to improve visualisation of the plotted factor on the x-axis.  If more than two axes are supplied, the first two are used with a \code{\link{warning}}.}
  \item{vars}{Vector giving the numbers associated with the variables to be plotted.  Must contain at least two variables.}
  \item{ls}{If TRUE, the least-squares fit is also shown for comparison.}
  \item{newdata}{An optional data frame in which to look for variables with which to predict.  If omitted, the fitted values are used.}
  \item{responses}{Vector giving the numbers associated with the variables to be treated as 'responses'.}
  \item{predictors}{Vector giving the numbers associated with the variables to be treated as 'predictors'.}
  \item{which.var}{Vector giving the numbers associated with the variables to be used in axis calculation.}
  \item{stat}{Character string of the name of a function that returns a statistic quantifying the 'performance' of a fitted \code{factanal.predictive} object.  The default, deviance, results in strictly non-increasing screeplots, which is the classical behaviour of such plots.  Other options are \code{logLik}, \code{AIC}, or a user-defined function.}
  \item{shift}{If \code{TRUE} the y-axis is shifted such that the minimum model is shifted to zero.}
  \item{plot}{If \code{FALSE} the plotting is suppressed.}
  \item{ylab}{Label for the y-axis.  If omitted the label is taken to be \code{stat}.}
  \item{main}{Title for screeplot}
  \item{grey.int}{Grey colour for 'within ten units' region.}
  \item{\dots}{For \code{factanal.predictive}:  Further arguments to pass to \code{\link{factanal}}.  However, if one attempts to pass \code{covmat}, an error message will be produced.  Unlike \code{\link{factanal}}, estimation cannot take place with a covariance matrix alone.  This may be changed in the future.  For methods:  Further arguments to be passed to the default method.  Note that these don't always work.  I'll improve the documentation to provide a better explanation for this in the future.}
}
\value{
  An object of class \code{factanal.predictive} that includes the following components, in addition to those that are included in \code{factanal} objects:
  \item{factors}{Either the number of factors in the model or "full" if the number of factors exceeds the maximum number for a factor analysis.  If this number is exceeded, a "full" multivariate normal model is fitted that treats all covariances and variances as free parameters.}
  \item{n.obs}{Number of observations (i.e. rows in x).}
  \item{Cor}{The model-implied correlation matrix.}
  \item{call.pred}{The matched call to \code{factanal.predictive}.}
  \item{n.vars}{Number of variables (i.e. columns in x).}
  \item{x}{Data matrix passed to \code{factanal.predictive}.}
  \item{Cov}{The model-implied covariance matrix.}
  \item{B}{Unstandardized coefficients.}
}
\seealso{
This function is based on \code{\link{factanal}}.  For random-effects ordination of presence-absence data see \code{\link{ltm.ecol}}.
}

\author{
Steve Walker
}

\references{
Walker, S.C. and Jackson, D.A. (in press) Random-effects ordination: describing and predicting multivariate correlations and co-occurrences.  Ecological Monographs.
}

\examples{
limn.fa <- factanal.predictive(limn,2)
scores(limn.fa)
}
