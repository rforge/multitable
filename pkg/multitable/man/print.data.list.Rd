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
  \item{\dots}{Not used.}
}
\details{
Prints each variable in \code{x} proceeded by the underlined name of that variable and followed by the names of the dimensions that that variable is replicated along.  A special notation is used whereby dimension names are separated by two vertical bars, \code{||}.  After all the variables are printed, the \code{repdim} attribute of \code{x} is also printed.
}
\seealso{
\code{\link{print}}
}
