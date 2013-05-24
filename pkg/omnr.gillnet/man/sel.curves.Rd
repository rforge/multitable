\name{sel.curves}
\alias{gamm}
\alias{inv.gau}
\alias{lognorm}
\alias{norm}
\alias{norm.loc}
\alias{sel.curve}
\alias{sel.curves}
\title{Selection curve functions}
\usage{
  norm()

  norm.loc()

  lognorm()

  gamm()

  inv.gau()
}
\value{
  For \code{norm.loc}, \code{norm}, \code{lognorm},
  \code{gamm}, and \code{inv.gau}, a list of functions:

  \item{loglinpredictors}{calculates the predictors used in
  log-linear model fitting}

  \item{off}{calculates the offset term for log-linear
  model fitting}

  \item{selcurvparams}{calculates the parameters of the
  selection curve from the coefficients of the terms in the
  log linear model}

  \item{curv}{calculates a matrix of values from the
  selection curve from two matrices of the same size giving
  the fish lengths and mesh sizes}

  \item{peak}{calculates the peak of the selection curve
  for different mesh sizes}
}
\description{
  These functions generate various other functions for
  making computations under various assumptions about the
  underlying selection curve.
}
\details{
  \code{sel.curves} is a list with the names of the five
  selection curve functions.
}
\examples{
data(north.pike)
np <- make.catch(north.pike$MESH, north.pike$FLEN)
lapply(sel.curves, function(x) fit.catch(np, eval(x)))
}
\author{
  Steve Walker
}
\seealso{
  \code{\link{getSelcurve}}
}

