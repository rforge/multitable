\name{traitgram2}
\alias{traitgram2}
\title{Draw Ackerly traitgram and return phylogenetic and functional distances}
\usage{
  traitgram2(x, phy, ..., plot = TRUE, a, p)
}
\arguments{
  \item{x}{See \code{\link{traitgram}}}

  \item{phy}{See \code{\link{traitgram}}}

  \item{...}{Additional arguments to
  \code{\link{traitgram}}}

  \item{plot}{Should the traitgram be plotted?}

  \item{a}{Optional, see \code{\link{FPD}}}

  \item{p}{Optional, see \code{\link{FPD}}}
}
\value{
  A plot (if \code{plot = TRUE}) and an object of class
  \code{traitgram} with elements: \item{traits}{The trait
  values} \item{tree}{The phylogenetic tree} \item{PD}{A
  phylogenetic distance matrix (computed with
  \code{\link{cophenetic}})} \item{FD}{A functional
  distance matrix} \item{FPD}{A distance matrix combining
  both phylogenetic and functional distances (see
  \code{\link{FPD}})}
}
\description{
  Draws Ackerly's \code{\link{traitgram}}, but also returns
  information about the distance relationships implied by
  the traitgram.
}

