\name{dropdl}
\alias{dropdl}
\alias{dropl}
\title{Drop Data List Dimensions}
\description{Delete the dimensions of a data list.}
\usage{
dropdl(x)

dropl(x)
}
\arguments{
  \item{x}{A data list (for \code{dropdl}) or a list (for \code{dropl}).}
}
\value{
For \code{dropdl}, a data list without the dimensions with only one level, and without any of the variables that are only replicated along dimensions with only one level.

For \code{dropl}, a list without null elements.
}
\seealso{
\code{\link{drop}}, \code{\link{data.list}}, \code{\link{list}}
}