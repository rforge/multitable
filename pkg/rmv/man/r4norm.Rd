\name{r4norm}
\alias{r4norm}
\alias{r4norm1}
\alias{r4normstd}
\title{Fourth-corner normal distribution}
\usage{
r4norm(n, ...)

r4normstd(n, rowsResp, colsResp, rowsPred, colsPred)

r4norm1(C, U, V, Omega, Sigma, Phi, Psi)
}
\arguments{
  \item{C}{The fourth-corner matrix from a bilinear model.}

  \item{U}{The mean of the X matrix from a bilinear model.}

  \item{V}{The mean of the Z matrix from a bilinear model.}

  \item{Omega}{The covariance matrix for the columns of the
  error matrix.}

  \item{Sigma}{The covariance matrix for the rows of the
  error matrix.}

  \item{Phi}{The covariance matrix for the columns of the X
  matrix.}

  \item{Psi}{The covariance matrix for the columns of the Z
  matrix.}

  \item{n}{Number of random matrices to generate.}

  \item{...}{Arguments to be passed to \code{r4norm1}.}
}
\value{
Random matrix or a list of i.i.d. random matrices.
}
\description{
Simulate random data sets from the fourth-corner normal
distribution.
}
\details{
\code{r4norm} generates a list of \code{n} matrices,
whereas \code{r4norm1} generates one matrix.
}

