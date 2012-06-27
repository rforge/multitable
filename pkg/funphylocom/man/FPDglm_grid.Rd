\name{FPDglm_grid}
\alias{FPDglm_grid}
\title{Calculate  Grid search functional phylogenetic diversity generalised linear model}
\usage{
  FPDglm_grid(tab, y, PDist, FDist, a = seq(0, 1, 0.01),
    p = 2, index = "mpd", abundance.weighted = FALSE, ...)
}
\arguments{
  \item{tab}{The community by species presence table.}

  \item{y}{An ecosystem function (or other site
  characteristic being used as a response variable).}

  \item{PDist}{A phylogenetic distance matrix.}

  \item{FDist}{A functional distance matrix.}

  \item{a}{A vector of numbers between 0 and 1 giving the
  amount of weight to put on \code{PDist} relative to
  \code{FDist}.  It is best if a is an evenly-spaced grid.}

  \item{p}{A number giving the \code{p}-norm.}

  \item{index}{Character string indicating the diversity
  index used to combine distances and community data.
  Currently, either \code{'mpd'} or \code{'rao'}.}

  \item{abundance.weighted}{Should mean pairwise distances
  be weighted by species abundance? (default = FALSE).
  Only relevant if \code{index = 'mpd'} and \code{tab} is
  an abundance matrix.}

  \item{...}{Additional arguments to pass to
  \code{\link{FPDglm}}.}
}
\value{
  TODO.
}
\description{
  Calculate deviances over a grid of the phylogenetic
  weighting parameter, a, for generalised linear models
  using functional-phylogenetic diversity indices as
  predictors.
}

