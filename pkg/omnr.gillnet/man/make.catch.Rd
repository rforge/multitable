\name{make.catch}
\alias{make.catch}
\alias{plot.catch}
\title{Create dataset of class \code{catch}}
\usage{
  make.catch(mesh, lens, counts, binsize = 20,
    data.name = deparse(substitute(lens)))

  \method{plot}{catch} (x, y, mesh.pos = "topleft",
    y.incr = 200, cex.mesh = 1, cex.fork = 1,
    cex.main = 1.5, cex.xaxis = 1, cex.yaxis = 1,
    mar = c(1, 4, 0, 1), oma = c(4, 0, 4, 0),
    line.fork = 2, line.main = 2, ...)
}
\arguments{
  \item{mesh}{a vector of gillnet mesh sizes (one for each
  fish).}

  \item{lens}{a vector of fish lengths (one for each
  fish).}

  \item{counts}{optional.  not important for now.  I'll
  explain later.}

  \item{binsize}{positive number giving the size of the
  histogram bins that fish lengths are categorized into.}

  \item{data.name}{name of the data set.}

  \item{x}{an object of class \code{catch}.}

  \item{y}{not used...only present for consistency with
  default S3 method.}

  \item{mesh.pos}{a character string indicating where to
  print the mesh sizes. see \code{\link{legend}} for more
  details.}

  \item{y.incr}{increment of the y-axis scales}

  \item{cex.mesh}{relative point size of text indicating
  mesh sizes.}

  \item{cex.fork}{relative point size of text indicating
  fork lengths.}

  \item{cex.main}{relative point size of main title.}

  \item{cex.xaxis}{relative point size of x-axis.}

  \item{cex.yaxis}{relative point size of y-axis.}

  \item{mar}{margins of each individual plot.}

  \item{oma}{margins of overall plot.}

  \item{line.fork}{line for the text indicating fork
  lengths.}

  \item{line.main}{line for the main title.}

  \item{\dots}{additional arguments to pass to
  \code{\link{par}}.}
}
\value{
  An object of class \code{catch} with components:

  \item{counts}{a number-of-length-bins by
  number-of-mesh-sizes matrix of fish counts.}

  \item{lens}{a vector of the mid-point lengths
  corresponding to the rows of counts (note difference
  between input lens).}

  \item{mesh}{a vector of the raw (i.e. uncorrected by
  \code{perimeter.factor} in \code{\link{fit.catch}}) mesh
  sizes corresponding to the columns of counts (note
  difference between input mesh).}
}
\description{
  Creates \code{catch} objects out of information on
  gillnet catches.  These objects are required for use with
  the software in this package.
}
\author{
  Steve Walker
}
\seealso{
  \code{\link{fit.catch}}, \code{\link{plot.catch}}
}

