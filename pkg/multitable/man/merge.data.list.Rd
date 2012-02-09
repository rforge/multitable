\name{merge.data.list}
\alias{merge.data.list}
\title{
Merge Two Data Lists
}
\description{
Merge two data lists, such that one of the data lists is replicated along all of the dimensions of the other.
}
\usage{
\method{merge}{data.list}(x, y, ...)
}
\arguments{
  \item{x}{A data list}
  \item{y}{Another data list}
  \item{\dots}{Not currently used}
}
\details{This \code{merge.data.list} function is a simple wrapper to data list addition, \code{\link{Ops.data.list}}}
\value{
Yet another data list
}
