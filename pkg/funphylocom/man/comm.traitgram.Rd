\name{comm.traitgram}
\alias{comm.traitgram}
\title{Community traitgram}
\usage{
  comm.traitgram(x, phy, xaxt = "s", underscore = FALSE,
    show.names = TRUE, show.xaxis.values = TRUE,
    method = c("ace", "pic"),
    edge.color = c("black", "light grey"), edge.lwd.in = 2,
    edge.lwd.out = 1, tip.color = c("black", "light grey"),
    tip.cex = par("cex"), Group = NULL, ...)
}
\arguments{
  \item{x}{See \code{\link{traitgram}}.}

  \item{phy}{See \code{\link{traitgram}}.}

  \item{xaxt}{See \code{\link{traitgram}}.}

  \item{underscore}{See \code{\link{traitgram}}.}

  \item{show.names}{See \code{\link{traitgram}}.}

  \item{show.xaxis.values}{See \code{\link{traitgram}}.}

  \item{method}{See \code{\link{traitgram}}.}

  \item{edge.color}{A single color or a vector of two
  colors.}

  \item{edge.lwd.in}{A single width value for species in
  the subset.}

  \item{edge.lwd.out}{A single width value for species out
  of the subset (ignored if \code{is.null(Group)}).}

  \item{tip.color}{A single color or a vector of two
  colors.}

  \item{tip.cex}{A single size value.}

  \item{Group}{Either a subset of the tip labels in
  \code{phy} or the indices associated with the species in
  the community.}

  \item{...}{Additional arguments to
  \code{\link{traitgram}}.}
}
\value{
  No return value, but a traitgram is plotted.
}
\description{
  Displays a subset of species from a species pool within
  an Ackerly traitgram (\code{\link{traitgram}}).
}
\details{
  This function is a modification of the
  \code{\link{traitgram}} function in the \code{picante}
  package.
}
\author{
  David Ackerly, modified by Cecile Albert.
}

