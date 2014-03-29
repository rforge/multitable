\name{ConDivSimComm}
\alias{ConDivSimComm}
\title{Null models for (TODO:  better title)}
\usage{
ConDivSimComm(comm, PDist, FDist, Sim, p = 2, Plot = TRUE, ord = TRUE,
  disp99 = FALSE, num.a = 20)
}
\arguments{
  \item{comm}{A vector over the species in the regional
  pool, giving the presence or absence of the species
  occurring in the community (1 for present species, 0 for
  absent species).}

  \item{PDist}{Phylogenetic distance matrix over the
  species in the regional pool.}

  \item{FDist}{Functional distance matrix over the species
  in the regional pool.}

  \item{Sim}{The number of permutations of the presence
  vector used to make the estimations.}

  \item{p}{Parameter passed to \code{\link{FPDist}}
  function.}

  \item{Plot}{TRUE or FALSE to make the plot of the
  expected average mean pairwise distance over a range of
  a-values (\code{a} is a parameter in the
  \code{\link{FPDist}} function), and the 5-95\% confidence
  interval.}

  \item{ord}{Order rows and columns of the matrices?
  (defaults to \code{TRUE}).  Note that sites are never
  ordered.}

  \item{disp99}{Display the 99\% interval?}

  \item{num.a}{Number of a-values to compute.}
}
\value{
TODO
}
\description{
Check for a given community how the mean pairwise distance
changes with the parameter a in FPDist
}
\details{
If \code{plot == TRUE}, then a surface is drawn giving the
null distribution.  Lighter shades of gray give larger
intervals with categories: 0.005-0.995 = 99\%, 0.025-0.975
= 95\%, 0.05-0.95 = 90\%, 0.25-0.75 = 50\%.
}

