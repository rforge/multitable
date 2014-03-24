\name{l.m.vecs}
\alias{l.m.vecs}
\title{Create consistent length and mesh size vectors}
\usage{
l.m.vecs(l, m, perimeter.factor = 1)
}
\arguments{
  \item{l}{Lengths.}

  \item{m}{Mesh sizes.}

  \item{perimeter.factor}{Factor by which \code{m} is
  multiplied.}
}
\value{
A list of matrices with two components:

l A lengths vector.

m A mesh sizes vector.
}
\description{
Create consistent length and mesh size vectors
}
\seealso{
Other l.m.combine: \code{\link{l.m.mats}}
}

