\name{varnames}
\alias{varnames}
\alias{varnames<-}
\title{Data list variable names}
\description{
Retrieve and set variable names in data list objects.
}
\usage{
varnames(dl)
varnames(dl) <- value
}
\arguments{
  \item{dl}{A \code{\link{data.list}} object.}
  \item{value}{A character vector with the names of the data list.}
}
\details{
Currently, \code{varnames(dl)} is implemented as \code{names(dl)}, and therefore the two are identical.
}
\seealso{
\code{\link{names}} for a similar function and \code{\link{nvar}} for the number of variables in a \code{\link{data.list}} object.
}