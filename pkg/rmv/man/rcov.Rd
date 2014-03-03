\name{rcov}
\alias{rcov}
\title{Random covariance matrices}
\usage{
rcov(n, m, rfunc = rnorm, ...)
}
\arguments{
  \item{n}{Number of multivariate samples.}

  \item{m}{Number of variables.}

  \item{rfunc}{Function that returns a vector of random
  draws from a particular distribution.  The first argument
  of \code{rfunc} must give the number of random draws to
  make.}

  \item{...}{Additional arguments to \code{rfunc}.}
}
\value{
A covariance matrix.
}
\description{
Simulate random covariance matrices by samping an \code{n}
by \code{m} matrix of iid variates from \code{rfunc} and
then passing that matrix to \code{\link{cov}}.
}

