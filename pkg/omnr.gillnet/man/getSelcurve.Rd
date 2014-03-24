\name{getSelcurve}
\alias{getSelcurve}
\alias{getTotalSelcurve}
\alias{modelaverageTotalSelcurve}
\title{Get fitted selection curve}
\usage{
getSelcurve(x, l = get.lens(x), m = get.mesh(x), tangle = TRUE)

getTotalSelcurve(x, ...)

modelaverageTotalSelcurve(x, l = get.lens(x), m = get.mesh(x), ...)
}
\arguments{
  \item{x}{a \code{\link{fit.catch}} object (for
  \code{modelaverageTotalSelcurve} a
  \code{\link{fit.catch.compare}} object).}

  \item{l}{a vector of fish lengths the same size and
  dimensions as \code{m}; defaults to x$l}

  \item{m}{a vector of mesh sizes the same size and
  dimensions as \code{l}; defaults to x$m}

  \item{tangle}{selection curve for model with a tangle
  parameter?}

  \item{...}{arguments to pass to \code{getSelcurve}}
}
\value{
For \code{getSelcurve} a vector of relative selection
probabilities corresponding to the fish length and mesh
data in \code{l} and \code{m}, with identical dimensions to
\code{l} and \code{m}.  For \code{getSelcurve} a vector of
total relative selection probabilities (i.e. over all mesh
sizes) corresponding to each fish length class in \code{x}.
}
\description{
Calculate a selection curve estimated under a fitted
selectivity model (i.e. \code{fit.catch} object).
}
\author{
Steve Walker
}
\seealso{
\code{\link{sel.curve}}
}

