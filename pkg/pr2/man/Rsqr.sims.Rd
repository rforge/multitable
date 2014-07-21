\name{Rsqr.sims}
\alias{Rsqr.sims}
\title{Simulate community data for testing R squares}
\usage{
  Rsqr.sims(n = 100, m = 10, p = 10, sigmaE = 1,
    sigmaB = 1, mu = 0,
    cmt = cor(matrix(rnorm(p * (p + 1)), p + 1, p)),
    cmv = cmt,
    B = matrix(rnorm(m * p, mean = mu, sd = sigmaB), p, m),
    hell = TRUE, untr = TRUE)
}
\arguments{
  \item{n}{Number of sites}

  \item{m}{Number of species}

  \item{p}{Number of environmental variables}

  \item{sigmaE}{Standard deviation of the error term}

  \item{sigmaB}{Standard deviation of the coefficients
  relating species to environment}

  \item{mu}{Mean of the coefficients relating species to
  environment}

  \item{cmt}{Correlation matrix of the environmental
  variables in the training set}

  \item{cmv}{Correlation matrix of the environmental
  variables in the validation set}

  \item{B}{\code{p} by \code{m} matrix of coefficients
  relating species to environment}

  \item{hell}{Should the Hellinger transformed results be
  returned?}

  \item{untr}{Should untransformed results be returned?}
}
\value{
  A vector with the sample, adjusted, predictive, and
  validation set R-squares. If both \code{hell} and
  \code{untr} are \code{TRUE}, then the vector contains 8
  values.
}
\description{
  Simulate community data for testing R squares
}
\seealso{
  Other sims: \code{\link{rep.Rsqr.sims}}
}

