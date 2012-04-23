\name{peresneto.simulation}
\alias{peresneto.simulation}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Multivariate normal simulation with Peres-Neto covariance matrices
}
\description{
Multivariate normal simulation with Peres-Neto covariance matrices.
}
\usage{
peresneto.simulation(rmarg = rnorm, n = 50, vars = "nine", mat = 1, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rmarg}{A function that returns univariate samples, such that the first argument accepts the univariate sample size.}
  \item{n}{Multivariate sample size (i.e. the number of rows in the returned matrix).}
  \item{vars}{Either "nine" or "eighteen", giving the 9- or 18-variable Peres-Neto matrices respectively.}
  \item{mat}{An integer from 1-18 giving the type of Peres-Neto matrix.}
  \item{\dots}{Further arguments to \code{rmarg}.}
}
\value{
A random matrix with \code{n} rows and either 9 or 18 columns.
}
\author{
Steve Walker
}