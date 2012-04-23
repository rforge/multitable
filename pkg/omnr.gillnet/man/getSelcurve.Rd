\name{getSelcurve}
\alias{getSelcurve}
\alias{getTotalSelcurve}
\title{Get fitted selection curve}
\description{Calculate a selection curve estimated under a fitted selectivity model (i.e. \code{fit.catch} object).}
\usage{
getSelcurve(x, l, m, tangle = TRUE)

getTotalSelcurve(x, tangle = TRUE)
}
\arguments{
  \item{x}{a \code{\link{fit.catch}} object}
  \item{l}{a vector of fish lengths the same size and dimensions as \code{m}; defaults to x$l}
  \item{m}{a vector of mesh sizes the same size and dimensions as \code{l}; defaults to x$m}
  \item{tangle}{selection curve for model with a tangle parameter?}
}
\value{
For \code{getSelcurve} a vector of relative selection probabilities corresponding to the fish length and mesh data in \code{l} and \code{m}, with identical dimensions to \code{l} and \code{m}.  For \code{getSelcurve} a vector of total relative selection probabilities (i.e. over all mesh sizes) corresponding to each fish length class in \code{x}.
}
\author{
Steve Walker
}
\seealso{
\code{\link{sel.curve}}
}