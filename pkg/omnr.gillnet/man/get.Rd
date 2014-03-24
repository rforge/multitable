\name{get.lens.mesh}
\alias{get.lens}
\alias{get.lens.catch}
\alias{get.lens.fit.catch}
\alias{get.lens.fit.catch.compare}
\alias{get.lens.mesh}
\alias{get.mesh}
\alias{get.mesh.catch}
\alias{get.mesh.fit.catch}
\alias{get.mesh.fit.catch.compare}
\title{Extract lengths and mesh sizes}
\usage{
get.lens(object)

get.mesh(object, ...)

\method{get.lens}{catch}(object)

\method{get.mesh}{catch}(object, perimeter.factor = 1, ...)

\method{get.lens}{fit.catch}(object)

\method{get.mesh}{fit.catch}(object, ...)

\method{get.lens}{fit.catch.compare}(object)

\method{get.mesh}{fit.catch.compare}(object, ...)
}
\arguments{
  \item{object}{A \code{catch}, \code{fit.catch},
  \code{fit.catch.compare} or object.}

  \item{perimeter.factor}{Factor by which to multiply the
  inputted mesh sizes to obtain mesh perimeters, which are
  rquired by the analyses.  This argument is only used for
  \code{catch} objects because mesh sizes in
  \code{fit.catch} and \code{fit.catch.compare} objects
  have already been converted to mesh perimeters.}

  \item{...}{Not used now.}
}
\description{
Extract lengths and mesh sizes
}

