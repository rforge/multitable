\name{fish.bins}
\alias{fish.bins}
\title{Bin for \code{\link{make.catch}}}
\usage{
fish.bins(lens, binsize)
}
\arguments{
  \item{lens}{vector of fish lengths (e.g. fork length)}

  \item{binsize}{size of histogram bins with which to
  categorize fish by their length}
}
\value{
a vector of the breakpoints between bins, suitable for
inputing as the breaks argument in \code{\link{hist}}.
}
\description{
Creates histogram bins suitable for use with
\code{\link{make.catch}}
}

