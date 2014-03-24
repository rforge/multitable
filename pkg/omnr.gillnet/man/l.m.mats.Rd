\name{l.m.mats}
\alias{l.m.mats}
\title{Create consistent length and mesh size matrices}
\usage{
l.m.mats(l, m, perimeter.factor = 1)
}
\arguments{
  \item{l}{Lengths.}

  \item{m}{Mesh sizes.}

  \item{perimeter.factor}{Factor by which \code{m} is
  multiplied.}
}
\value{
A list of matrices with two components:

l A lengths matrix.

m A mesh sizes matrix.
}
\description{
Create consistent length and mesh size matrices
}
\seealso{
Other l.m.combine: \code{\link{l.m.vecs}}
}

