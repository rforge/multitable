\name{make.catch}
\alias{make.catch}
\alias{plot.catch}
\title{
Create dataset of class \code{catch}
}
\description{
Creates \code{catch} objects out of information on gillnet catches.  These objects are required for use with the software in this package.
}
\usage{
make.catch(mesh, lens, counts, binsize = 20)

\method{plot}{catch}(x, y, mesh.pos = "topleft", y.incr = 200,...)
}
\arguments{
  \item{mesh}{a vector of gillnet mesh sizes (one for each fish).}
  \item{lens}{a vector of fish lengths (one for each fish).}
  \item{counts}{optional.  not important for now.  I'll explain later.}
  \item{binsize}{positive number giving the size of the histogram bins that fish lengths are categorized into.}
  \item{x}{an object of class \code{catch}.}
  \item{y}{not used...only present for consistency with default S3 method.}
  \item{mesh.pos}{a character string indicating where to print the mesh sizes. see \code{\link{legend}} for more details.}
  \item{y.incr}{increment of the y-axis scales}
  \item{\dots}{additional arguments to pass to \code{\link{plot}}.}
}
\value{
An object of class \code{catch} with components:
\item{counts}{a number-of-length-bins by number-of-mesh-sizes matrix of fish counts.}
\item{lens}{a vector of the mid-point lengths corresponding to the rows of counts (note difference between input lens).}
\item{mesh}{a vector of the mesh sizes corresponding to the columns of counts (note difference between input mesh).}
}
\author{
Steve Walker
}
\seealso{
\code{\link{fit.catch}}, \code{\link{plot.catch}}
}
