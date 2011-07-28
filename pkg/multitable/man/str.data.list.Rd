\name{str.data.list}
\alias{str.data.list}
\title{Structure of data list objects}
\description{
Compactly display the structure of a data list object.
}
\usage{
\method{str}{data.list}(object, give.attr = FALSE, ...)
}
\arguments{
  \item{object}{A data list object}
  \item{give.attr}{See documentation for \code{\link{str}}}
  \item{...}{Potential further arguments (see \code{\link{str}})}
}
\details{
Currently identical to \code{\link{str}} except that \code{give.attr} defaults to \code{FALSE}, because data lists are 'cluttered' with attributes that should not be part of the \code{str} summary.  Perhaps in the future, a special \code{str} method will be written for data lists.
}
\seealso{
\code{\link{str}}
}