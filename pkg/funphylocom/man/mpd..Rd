\name{mpd.}
\alias{mpd.}
\title{Mean pairwise distance}
\usage{
  mpd.(samp, dis, abundance.weighted = FALSE, ord = TRUE)
}
\arguments{
  \item{samp}{Community data matrix.}

  \item{dis}{Interspecific distance matrix.}

  \item{abundance.weighted}{Should mean pairwise diatnces
  be weighted by species abundance? (default = FALSE).}

  \item{ord}{Order rows and columns of the matrices?
  (defaults to \code{TRUE}).  Note that sites are never
  ordered.}
}
\value{
  Vector of MPD values for each community.
}
\description{
  Calculates mean pairwise distance separating taxa in a
  community (slightly modified from the \code{picante}
  function: \code{\link{mpd}})
}
\details{
  Two changes from original \code{\link{mpd}} function: (1)
  avoid NA values (TODO: better description here). (2) give
  useful error messages for poorly named \code{samp} and
  \code{dis}. Also please be aware that if
  \code{abundance.weighted = FALSE}, then \code{samp}
  should be explicitly a presence-absence matrix (i.e. with
  zeros and ones only).
}
\author{
  Code modified from original version by Steven Kembel.
}

