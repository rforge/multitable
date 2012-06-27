\name{FPDist}
\alias{FPDist}
\title{Functional phylogenetic distance}
\usage{
  FPDist(PDist, FDist, a, p, ord = TRUE)
}
\arguments{
  \item{PDist}{A phylogenetic distance matrix.}

  \item{FDist}{A functional distance matrix.}

  \item{a}{A number between 0 and 1 giving the amount of
  weight.  to put on \code{PDist} relative to
  \code{FDist}.}

  \item{p}{A number giving the \code{p}-norm.}

  \item{ord}{Order rows and columns of the matrices?
  (defaults to \code{TRUE}).}
}
\value{
  A distance matrix.
}
\description{
  Computates a functional phylogenetic distance matrix from
  phylogenetic and functional distance matrices and two
  weighting parameters.
}
\note{
  This function is not very user friendly yet.  There are
  no doubt many use cases that I've ignored.
}

