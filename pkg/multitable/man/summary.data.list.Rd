\name{summary.data.list}
\alias{str.data.list}
\alias{summary.data.list}
\title{Information about data list objects}
\description{
Summarize and compactly display the structure of a data list object.
}
\usage{
\method{summary}{data.list}(object, ...)

\method{str}{data.list}(object, give.attr = FALSE, hide.internals = TRUE, ...)
}
\arguments{
  \item{object}{A data list object}
  \item{give.attr}{See documentation for \code{\link{str}}.  Ignored if \code{hide.internals} is FALSE.}
  \item{hide.internals}{If FALSE, \code{object} is \code{\link{unclass}}ed first to reveal further internal details.}
  \item{...}{Potential further arguments (see \code{\link{str}})}
}
\details{
The \code{summary.data.list} function returns a logical matrix indicating which dimensions (rows) are associated with which variables (columns) of the data list object.

Currently \code{str.data.list} is identical to \code{\link{str}} except that \code{give.attr} defaults to \code{FALSE}, because data lists are 'cluttered' with attributes that should not be part of the \code{str} summary.  Also, setting the \code{hide.internals} argument to FALSE reveals more information.  Perhaps in the future, a special \code{str} method will be written for data lists.
}
\note{
The \code{hide.internals} argument was suggested by a reviewer of a JSS manuscript.
}
\seealso{
\code{\link{summary}}, \code{\link{str}}
}
\examples{
data(fake.community)
summary(fake.community)
str(fake.community)
}
