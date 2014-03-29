\name{rao}
\alias{rao}
\title{Rao's quadratic entropy}
\usage{
rao(X, D, ord = TRUE)
}
\arguments{
  \item{D}{A species by species distance matrix.}

  \item{X}{A sites by species community matrix.}

  \item{ord}{Order rows and columns of the matrices?
  (defaults to \code{TRUE}).  Note that sites are never
  ordered.}
}
\value{
A vector of diversity indices (one for each site).
}
\description{
Computes Rao's quadratic entropy from a distance matrix and
community matrix.
}

