\name{a.hpd}
\alias{a.hpd}
\title{Highest posterior density region for a}
\usage{
a.hpd(a_grid, posterior, level = 0.95)
}
\arguments{
  \item{a_grid}{A vector of (preferably evenly spaced) 'a'
  values (between 0 and 1).}

  \item{posterior}{The values of the posterior density at
  each point in \code{a_grid}.}

  \item{level}{Size of the highest posterior density
  region.}
}
\value{
A data frame with two columns: the values of the grid
within the hpd region and the value of the posterior at
each point in this grid.
}
\description{
Find points on a grid within a 100\code{p}\% highest
posterior density region for the tuning parameter, a
}
\details{
The 100\code{p}\% highest posterior density region for 'a'
is the subset of the interval between 0 and 1, which
contains 100\code{p}\% of the probability.
}

