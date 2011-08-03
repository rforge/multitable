\name{summary.data.list}
\alias{str.data.list}
\alias{summary.data.list}
\title{Information about data list objects}
\description{
Summarize and compactly display the structure of a data list object.
}
\usage{
\method{summary}{data.list}(object, ...)

\method{str}{data.list}(object, give.attr = FALSE, ...)
}
\arguments{
  \item{object}{A data list object}
  \item{give.attr}{See documentation for \code{\link{str}}}
  \item{...}{Potential further arguments (see \code{\link{str}})}
}
\details{
A list with two elements is returned by \code{summary.data.list}:  \itemize{
\item{dims}{A logical matrix indicating which dimensions (rows) are associated with which variables (columns) of the data list}
\item{modes}{A character vector indicating the \code{\link{mode}}s of the variables of the data list}
}

Currently \code{str.data.list} is identical to \code{\link{str}} except that \code{give.attr} defaults to \code{FALSE}, because data lists are 'cluttered' with attributes that should not be part of the \code{str} summary.  Perhaps in the future, a special \code{str} method will be written for data lists.
}
\seealso{
\code{\link{summary}}, \code{\link{str}}
}