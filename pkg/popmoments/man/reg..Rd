\name{reg.}
\alias{reg.}
\title{Population-level regression}
\usage{
  reg.(y, x, p)
}
\arguments{
  \item{x}{A numeric vector giving the independent variable
  of the population.}

  \item{y}{A numeric vector giving the dependent variable
  of the population.}

  \item{p}{An optional vector of weights.  If missing,
  equal weights are used.}
}
\value{
  The correlation of \code{x} \code{y}.
}
\description{
  Regression of one vector on another in a bivariate
  population.
}

