\name{rmv}
\alias{rmv}
\title{Multivariate random number generation}
\usage{
rmv(n, covmat, rfunc = rnorm, method = c("chol", "eigen"), ...)
}
\arguments{
  \item{n}{Number of random multivariate draws.}

  \item{covmat}{The covariance matrix with which to obtain
  correlations.}

  \item{rfunc}{Function that returns a vector of random
  draws from a particular distribution.  The first argument
  of \code{rfunc} must give the number of random draws to
  make.}

  \item{method}{The method for taking the matrix
  square-root.  Currently only \code{'chol'} and
  \code{'eigen'} are allowed for the Cholesky and eigen
  decomposition methods.  See \code{\link{msr}}.}

  \item{...}{Further arguments to pass to \code{rfunc}.}
}
\value{
A matrix with rows giving observations and columns giving
correlated variables.
}
\description{
Generates correlated random variables by the matrix
square-root method.
}
\details{
The number, \code{m}, of variables generated per each of
the \code{n} observations is equal to the number of rows
(or columns) of \code{covmat}.  \code{rfunc} is used to
fill an \code{n} by \code{m} matrix of uncorrelated
observations.  This matrix is multiplied by the matrix
square-root (\code{\link{msr}}) to obtain a matrix of
\code{m} correlated random variables.
}
\examples{
set.seed(1)
cm <- cor(matrix(rnorm(12), 6, 2))
x <- rmv(100, cm, rbinom, size = 100, prob = 0.7)
pairs(x)
cm
}

