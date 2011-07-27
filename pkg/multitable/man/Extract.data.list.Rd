\name{Extract.data.list}
\alias{Extract.data.list}
\alias{[.data.list}
\alias{$<-.data.list}
\alias{[[<-.data.list}
\title{Subscript extraction from data lists}
\description{
You get it don't you?
}
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
Currently \code{$<-.data.list} and \code{[[<-.data.list} overwrite any attributes that are passed via \code{value}, but maintains the attributes that were originally in \code{x}.
}
