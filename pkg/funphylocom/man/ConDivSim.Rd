\name{ConDivSim}
\alias{ConDivSim}
\title{Null models for functional-phylogenetic diversity}
\usage{
ConDivSim(tab, Dist, Sim, Plot = TRUE, ord = TRUE, disp99 = FALSE)
}
\arguments{
  \item{tab}{The community by species presence table.}

  \item{Dist}{The distance (functional, phylogenetic,
  FPDist) on which the mean pairwise distance is based.}

  \item{Sim}{The number of permutations of the presence
  vector used to make the estimations.}

  \item{Plot}{TRUE or FALSE to make the plot of the
  expected average mean pairwise distance, and the 5-95\%
  confidence interval.}

  \item{ord}{Order rows and columns of the matrices?
  (defaults to \code{TRUE}).  Note that sites are never
  ordered.}

  \item{disp99}{Display the 99\% interval?}
}
\value{
TODO
}
\description{
Simulate expectations (under a null model) of mean pairwise
distance for a set of communities with different species
richness.
}
\details{
If \code{plot == TRUE}, then a surface is drawn giving the
null distribution.  Lighter shades of gray give larger
intervals with categories: 0.005-0.995 = 99\%, 0.025-0.975
= 95\%, 0.05-0.95 = 90\%, 0.25-0.75 = 50\%.
}

