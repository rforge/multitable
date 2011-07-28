\name{nvar}
\alias{nvar}
\title{Number of variables in a data list object}
\description{
Extract the number of variables in a data list.
}
\usage{
nvar(dl)
}
\arguments{
  \item{dl}{A \code{\link{data.list}} object.}
}
\details{Currently, this is simply implemented as \code{length(dl)}.}
\value{The number of variables in \code{dl}.}