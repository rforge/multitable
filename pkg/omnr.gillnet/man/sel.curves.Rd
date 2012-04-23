\name{sel.curve}
\alias{sel.curves}
\alias{sel.curve}
\alias{norm.loc}
\alias{norm}
\alias{lognorm}
\alias{gamm}
\alias{inv.gau}
\title{Selection curve functions}
\description{These functions generate various other functions for making computations under various assumptions about the underlying selection curve.}
\usage{
norm.loc()
norm()
lognorm()
gamm()
inv.gau()

data(sel.curves)
}
\value{
For \code{norm.loc}, \code{norm}, \code{lognorm}, \code{gamm}, and \code{inv.gau}, a list of functions:
\item{loglinpredictors}{calculates the predictors used in log-linear model fitting}
\item{off}{calculates the offset term for log-linear model fitting}
\item{selcurvparams}{calculates the parameters of the selection curve from the coefficients of the terms in the log linear model}
\item{curv}{calculates a matrix of values from the selection curve from two matrices of the same size giving the fish lengths and mesh sizes}
\item{peak}{calculates the peak of the selection curve for different mesh sizes}

\code{sel.curves} is a vector of the five other functions.
}
\author{
Steve Walker
}
\seealso{
\code{\link{getSelcurve}}
}
\examples{
data(north.pike)
np <- make.catch(north.pike$MESH, north.pike$FLEN)
lapply(sel.curves, function(x) fit.catch(np, x))
}