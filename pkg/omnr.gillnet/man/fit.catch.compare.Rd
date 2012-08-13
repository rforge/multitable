\name{fit.catch.compare}
\alias{modelaverageTotalSelcurve}
\alias{fit.catch.compare}
\alias{plot.fit.catch.compare}
\alias{deviance.fit.catch.compare}
\alias{AIC.fit.catch.compare}
\alias{print.fit.catch.compare}
\alias{summary.fit.catch.compare}
\alias{print.summary.fit.catch.compare}
\title{Fit catch data to several size-selectivity models}
\description{
Compare the fit of catch data to several size-selectivity models fitted by \code{\link{fit.catch}}.
}
\usage{
fit.catch.compare(catch, perimeter.factor = 2, tol = 1e-8, omega0 = 0.1)

modelaverageTotalSelcurve(x)

\method{plot}{fit.catch.compare}(x, y, ...)

\method{deviance}{fit.catch.compare}(object, ...)

\method{AIC}{fit.catch.compare}(object, ..., k = 2)

\method{print}{fit.catch.compare}(x, digits = max(3, getOption("digits") - 3), ...)

\method{summary}{fit.catch.compare}(object, ...)

\method{print}{summary.fit.catch.compare}(x, digits = max(3, getOption("digits") - 3), ...)
}
\arguments{
  \item{catch}{an object of class \code{catch} created using \code{\link{make.catch}}.}
  \item{perimeter.factor}{Factor by which to multiply the inputted mesh sizes to obtain mesh perimeters, which are required by the analyses (if using bar mesh perimeter.factor = 4; if using stretch mesh perimeter.factor = 2, the default).}
  \item{x}{an object of class \code{fit.catch.compare} or \code{summary.fit.catch.compare}.}
  \item{object}{an object of class \code{fit.catch.compare}.}
  \item{y}{not used. only for consistency with S3 plot method.}
  \item{k}{not used.  only for consistency with default S3 method.}
  \item{tol}{tolerance used to decide on whether fitted values are numerically zero. used to correct AIC values. this correction depends on sample size, and the standard way to calculate sample size is as the number of mesh sizes times the number of fish length categories and then to subtract from this number the number of zero fitted counts}
  \item{omega0}{initial value for the tangle parameter}
  \item{digits}{number of digits to round to}
  \item{\dots}{additional arguments to be passed}
}
\value{
An object of class \code{fit.catch} with components:
\item{catch}{the original \code{catch} object}
\item{sel.curve}{the original selection curve}
\item{theta}{the fitted parameters of the selection curve}
\item{mu}{the fitted values}
\item{res}{the deviance residuals (see Millar and Fryer 1999)}
\item{dev}{model deviance(s)}
\item{tangle}{TRUE if tangle parameter fitted, FALSE otherwise}
\item{call}{the matched call}
\item{l}{the 'length matrix'}
\item{m}{the 'mesh matrix'}
}
\references{
J.F. Bromaghin (2005) A versatile net selectivity model, with application to Pacific salmon and freshwater species of the Yukon River, Alaska. Fisheries Research 74: 157-168.

R.B. Millar & R.J. Fryer (1999) Estimating the size-selection curves of towed gears, traps, nets and hooks. Reviews in Fish Biology and Fisheries 9: 89-116.
}
\author{
Steve Walker
}
\examples{
np <- make.catch(north.pike$MESH, north.pike$FLEN)
fms <- fit.catch.compare(np)
fms
plot(fms)
}
