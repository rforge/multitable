\name{msr}
\alias{msr}
\title{Square matrix square-root}
\usage{
  msr(sqrmat, method = c("chol", "eigen"))
}
\arguments{
  \item{sqrmat}{A square matrix.}

  \item{method}{A method for calculating the matrix
  square-root.}
}
\value{
  The matrix square-root.
}
\description{
  Calculate the square-root of a square matrix.
}
\details{
  A square-root, \code{S}, of a square matrix, \code{M},
  satisfies, \code{M = t(S) \%*\% S}.
}

