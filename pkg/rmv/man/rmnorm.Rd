\name{rmnorm}
\alias{rmnorm}
\alias{rmnorm1}
\title{Matrix normal distribution}
\usage{
  rmnorm(n, ...)

  rmnorm1(M, Omega = diag(1, ncol(M), ncol(M)),
    Sigma = diag(1, nrow(M), nrow(M)))
}
\arguments{
  \item{M}{Mean matrix.}

  \item{Omega}{Covariance matrix for the columns.}

  \item{Sigma}{Covariance matrix for the rows.}

  \item{n}{Number of random matrices to generate.}

  \item{...}{Arguments to be passed to \code{rmnorm1}.}
}
\value{
  Random matrix or a list of i.i.d. random matrices.
}
\description{
  Simulate random matrix normal variates.
}
\details{
  \code{rmnorm} generates a list of \code{n} matrices,
  whereas \code{rmnorm1} generates one matrix.
}

