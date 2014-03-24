\name{fit.catch}
\alias{AIC.fit.catch}
\alias{coef.fit.catch}
\alias{coefficients.fit.catch}
\alias{deviance.fit.catch}
\alias{fit.catch}
\alias{plot.fit.catch}
\alias{print.fit.catch}
\title{Fit catch data to size-selectivity models}
\usage{
fit.catch(catch, sel.curve = norm.loc, tangle = TRUE,
  perimeter.factor = 1, tol = 1e-08, omega0 = 0.1, effort = NULL)

\method{AIC}{fit.catch}(object, corrected = TRUE, ..., k = 2)

\method{coef}{fit.catch}(object, ...)

\method{coefficients}{fit.catch}(object, ...)

\method{print}{fit.catch}(x, digits = max(3, getOption("digits") - 3), ...)

\method{plot}{fit.catch}(x, y, xlab = "Fork length (mm)",
  ylab = "Relative selection probability", resolution = 500,
  plot.type = c("selcurve", "totalselcurve", "residuals",
  "observedvrsexpected"), max.cex = 2, min.cex = 0.5, leg.pos = "topleft",
  data.name = NULL, justtangle = TRUE, ...)

\method{deviance}{fit.catch}(object, ...)
}
\arguments{
  \item{catch}{an object of class \code{catch} created
  using \code{\link{make.catch}}.}

  \item{sel.curve}{one of a number of selection curve
  function including norm.loc, norm, lognorm, gamm and
  inv.gau.  These functions create a range of functions
  that are required to make various calculations.  In
  effect, the selection curve chosen defines the model to
  be fitted.}

  \item{tangle}{If TRUE, a tangle parameter is included in
  the fitted process (Bromaghin (2005).}

  \item{perimeter.factor}{Factor by which to multiply the
  inputted mesh sizes to obtain mesh perimeters, which are
  required by the analyses. % (if using bar mesh
  perimeter.factor = 4; if using stretch mesh
  perimeter.factor = 2, the default).}

  \item{x}{an object of class \code{fit.catch}.}

  \item{object}{an object of class \code{fit.catch}.}

  \item{y}{not used. only for consistency with S3 plot
  method.}

  \item{xlab}{x-axis label}

  \item{ylab}{y-axis label}

  \item{resolution}{higher numbers make smoother selection
  curves}

  \item{plot.type}{type of plot to produce, either
  "selcurve", "totalselcurve", or "residuals" for selection
  curves, total selection curve, or residual plot; in this
  last type, the sizes of the circles are proportional to
  deviance}

  \item{max.cex}{maximum size of the deviance circles}

  \item{min.cex}{minimum size of the deviance circles}

  \item{leg.pos}{position of the legend if necessary, see
  \code{\link{legend}}.}

  \item{data.name}{name of the data set to be plotted}

  \item{justtangle}{should only the tangle model be
  plotted?}

  \item{corrected}{if TRUE, the small-sample-size corrected
  version of AIC is returned (recommended)}

  \item{k}{not used.  only for consistency with default S3
  method.}

  \item{tol}{tolerance used to decide on whether fitted
  values are numerically zero. used to correct AIC values.
  this correction depends on sample size, and the standard
  way to calculate sample size is as the number of mesh
  sizes times the number of fish length categories and then
  to subtract from this number the number of zero fitted
  counts}

  \item{omega0}{initial value for the tangle parameter}

  \item{digits}{number of digits to round to}

  \item{\dots}{additional arguments to be passed}

  \item{effort}{TODO (sorry!)}
}
\value{
An object of class \code{fit.catch} with components:

catch the original \code{catch} object

sel.curve the original selection curve

theta the fitted parameters of the selection curve

mu the fitted values

res the deviance residuals (see Millar and Fryer 1999)

dev model deviance(s)

tangle TRUE if tangle parameter fitted, FALSE otherwise

call the matched call

l the 'length matrix'.

m the 'mesh matrix' (with input mesh sizes multiplied by
\code{perimeter.factor}).
}
\description{
First fits selection curves by log-linear fitting using
\code{\link{glm}} and then (if tangle is TRUE) uses the
SELECT method (of Millar 1992) to fit a slightly modified
model with a tangle parameter (see Bromaghin 2005 for the
concept of tangle parameters).
}
\examples{
data(north.pike)
np <- make.catch(north.pike$MESH, north.pike$FLEN)
plot(np)
(np.fit <- fit.catch(np, gamm))
plot(np.fit)
}
\author{
Steve Walker
}
\references{
J.F. Bromaghin (2005) A versatile net selectivity model,
with application to Pacific salmon and freshwater species
of the Yukon River, Alaska. Fisheries Research 74: 157-168.

R.B. Millar & R.J. Fryer (1999) Estimating the
size-selection curves of towed gears, traps, nets and
hooks. Reviews in Fish Biology and Fisheries 9: 89-116.
}

