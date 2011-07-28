\name{print.data.list}
\alias{print.data.list}
\title{Printing data lists}
\description{
A print method for data lists.
}
\usage{
\method{print}{data.list}(x, ...)
}
\arguments{
  \item{x}{A \code{\link{data.list}} object.}
  \item{\dots}{Additional arguments to pass to \code{\link{print}}.}
}
\details{
Prints each variable in \code{x} followed by the names of the dimensions that that variable is replicated along.  A special notation is used whereby dimension names are separated by two vertical bars, \code{||}.  After all the variables are printed, the \code{repdim} attribute of \code{x} is also printed.
}
\seealso{
\code{\link{print}}
}
