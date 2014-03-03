\name{var.}
\alias{var.}
\title{Population-level variance}
\usage{
  var.(x, p)
}
\arguments{
  \item{x}{A numeric vector giving the population.}

  \item{p}{An optional vector of weights.  If missing,
  equal weights are used.}
}
\value{
  The variance of \code{x}.
}
\description{
  Variance of a vector giving the elements of a population.
}
\examples{
# Note the difference between population and sample variance
 var.(1:10)
 var(1:10)
}

