\name{with.data.list}
\alias{with.data.list}
\alias{within.data.list}
\title{Evaluate an Expression in a Data (List) Environment}
\description{Evaluate an \R expression in an environment constructed from a data list, possibly modifying the original data.}
\usage{
\method{with}{data.list}(data, expr, func = "I", ...)
\method{within}{data.list}(data, expr, ...)
}
\arguments{
  \item{data}{A data list.}
  \item{expr}{Expression to evaluate.}
  \item{func}{A character string giving the name of a function to process \code{data} before evaluating \code{expr}.  Defaults to "\code{\link{I}}" which just uses \code{data} unaltered.  Another useful choice is "\code{\link{as.data.frame}}".} 
  \item{...}{Arguments to pass to \code{func}.  Not used with \code{within.data.list}.}
}
\value{
For with, the value of the evaluated \code{expr}.  For \code{within}, the modified \code{data.list}.
}
\seealso{
\code{\link{with}}
}