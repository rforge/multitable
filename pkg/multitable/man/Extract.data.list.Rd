\name{Extract.data.list}
\alias{Extract.data.list}
\alias{[.data.list}
\alias{$<-.data.list}
\alias{[[<-.data.list}
\title{Extract or replace parts of a data list object}
\description{Operators acting on data lists to extract or replace parts.}
\usage{
\method{[}{data.list}(x,...)
\method{$}{data.list}(x,i) <- value
\method{[[}{data.list}(x,i) <- value
}
\arguments{
  \item{x}{A \code{\link{data.list}} object.}
  \item{i}{Subscript or variable name.}
  \item{value}{Replacement value.}
  \item{...}{Vectors to subscript the data list.}
}
\details{
See vignette on extraction and replacement of data lists.

The \code{[.data.list} method of extraction differs from the default method \code{\link{[}} for lists.  The latter extracts list elements where as the former extracts from each of the vectors, matrices, and arrays of the data list simultaneously.  Therefore, even though a data list is based on a \code{\link{list}} object, the \code{[.data.list} extraction function works more like extraction from an array.  For objects in the data list that are missing some dimensions, those dimensions are ignored in the extraction.  But those dimensions are not ignored for other objects that are not missing them.  This is one of the major advantages of the data list concept; because the vectors, matrices, and arrays in a data list are related via dimension sharing, they can all be extracted from simultaneously.

The \code{$<-.data.list} and \code{[[<-.data.list} methods of replacement work very much like the default methods \code{\link{$<-}} and \code{\link{[[<-}} for lists, except that they ensure that \code{subsetdim} attributes are retained (see \code{\link{data.list}} for a description of these attributes).

To make use of the default methods for lists, simply \code{\link{unclass}} the data list object as in \code{unclass(x)[...]}, \code{unclass(x)$i <- value}, and \code{unclass(x)[[i]] <- value}.
}
\seealso{
\code{\link{data.list}} for creating new data lists and \code{\link{as.data.frame.data.list}} for coercing data lists to data frames.
}
