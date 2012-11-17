\name{rfa}
\alias{rfa}
\title{Correlated random number generation from factor analysis models}
\usage{
  rfa(n, Lambda, psi)
}
\arguments{
  \item{n}{Number of random multivariate draws.}

  \item{Lambda}{Variables-by-factors matrix of loadings for
  the factor analysis model.}

  \item{psi}{Vector of uniquenesses (i.e. residual
  variances).  One variance for each variable.}
}
\value{
  A matrix with rows giving observations and columns giving
  correlated variables.
}
\description{
  Simulate correlated random variables by supplying the
  parameters of a linear factor analysis model.
}
\details{
  The population covariance matrix for the multivariate
  observations is given by: \code{Lambda %*% t(Lambda) +
  diag(psi)}.  The idea here is that Lambda are
  coefficients relating a set of An alternative
  interpretation of this function is that Lambda is a
  matrix square-root of a singular covariance matrix TODO

  be understood similarly to \code{\link{rmv}}
}

