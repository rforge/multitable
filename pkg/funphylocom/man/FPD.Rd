\name{FPD}
\alias{FPD}
\title{Functional phylogenetic distance}
\usage{
  FPD(PD, FD, a, p)
}
\arguments{
  \item{PD}{A phylogenetic distance matrix.}

  \item{FD}{A functional distance matrix.}

  \item{a}{A number between 0 and 1 giving the amount of
  weight to put on \code{PD} relative to \code{FD}.}

  \item{p}{A number giving the \code{p}-norm.}
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

