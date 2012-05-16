\name{replicationgraph}
\alias{replicationgraph}
\title{Replication graph}
\usage{
  replicationgraph(replicationmatrix, dimensionnames,
    variablenames, edgespace = 0.3,
    boxheight = 1/(max(dim(replicationmatrix)) + 1),
    cex = 1, lwd = 1)
}
\arguments{
  \item{replicationmatrix}{A binary matrix with rows and
  columns representing dimensions of replication and
  variables respectively.}

  \item{dimensionnames}{Optional character vector giving
  names for the dimensions of replication.  If
  \code{\link{missing}}, the \code{\link{rownames}} of
  \code{replicationmatrix} are used.}

  \item{variablenames}{Optional character vector giving
  names for the variables.  If \code{\link{missing}}, the
  \code{\link{colnames}} of \code{replicationmatrix} are
  used.}

  \item{edgespace}{Number between 0 and 1 giving the width
  of the space for the edges of the graph.}

  \item{boxheight}{Number greater than zero giving the
  height of the boxes representing the nodes of the graph.
  Numbers that are too large to fit in the plot region will
  be automatically reduced.}

  \item{cex}{Text size.}

  \item{lwd}{Edge width.}
}
\description{
  Visualise multiple table replication structure as a
  bipartite graph.
}
\examples{
s <- matrix(c(1, 1), 2, 1, byrow = TRUE, dimnames = list(c('sites',
 'species'), c('abundance')))
replicationgraph(s)
s <- matrix(c(1, 1, 0, 1), 2, 2, byrow = TRUE, dimnames = list(c('sites',
 'species'), c('envrnmnt', 'abundance')))
replicationgraph(s)
s <- matrix(c(1, 1, 0, 0, 1, 1), 2, 3, byrow = TRUE, dimnames = list(c('sites', '
 species'), c('envrnmnt', 'abundance', 'traits')))
replicationgraph(s)
s <- matrix(c(1, 1, 1, 0, 1, 1), 2, 3, byrow = TRUE, dimnames = list(c('sites',
 'species'), c('envrnmnt', 'abundance', 'traits')))
replicationgraph(s)
replicationgraph(s, lwd = 10)
replicationgraph(s, cex = 3)
replicationgraph(s, edgespace = 0.1)
replicationgraph(s, boxheight = 0)
replicationgraph(s,
 variablenames = c('envrnmntl\\nvrbls','species\\nabdncs','species\\ntraits'))
}

