\name{as.data.list}
\alias{as.data.list}
\alias{as.data.list.list}
\alias{as.data.list.data.frame}
\alias{as.data.frame.data.list}
\alias{as.list.data.list}
\alias{data.list.mold}
\title{Coercion to and from data lists}
\description{
These functions coerce lists and data frame objects to data lists and vice versa.}
\usage{
as.data.list(x, ...)

data.list.mold(x)

\method{as.data.list}{list}(x, dnames, match.dnames, ...)

\method{as.data.list}{data.frame}(x, ...)

\method{as.data.frame}{data.list}(x, row.names = NULL, 
optional = FALSE, scheme = "repeat", mold, ...)

\method{as.list}{data.list}(x, drop.attr = TRUE, ...)
}
\arguments{
  \item{x}{An object. Either a data.list, data.frame, or list.}
  \item{mold}{A list typically obtained as output from \code{\link{data.list.mold}}.  See details.}
  \item{dnames}{Passed to \code{\link{data.list}}.}
  \item{match.dnames}{Passed to \code{\link{data.list}}.}
  \item{row.names}{Passed to the default method after some processing.}
  \item{optional}{Passed to the default method after some processing.}
  \item{scheme}{Type of coercion.  Currently, only "repeat" is allowed but others may follow.}
  \item{drop.attr}{Should the attributes of the data list be dropped?}
  \item{...}{Additional arguments (passed to the default method in \code{as.data.frame.data.list}).}
}
\details{
See vignette on coercing data lists into data frames.

By default, \code{as.data.frame.data.list} works in two steps.  First, a \code{mold} of the coercion process is created with \code{data.list.mold}.  The \code{mold} is a list of vectors, each with \code{\link{length}} equal to the number of rows in the coerced data frame.  Each vector is associated with a particular variable in \code{x} and contains indices that point to the elements of that variable.  Second, the \code{mold} is 'filled with' (i.e. used to subscript) \code{x} thereby producing the coerced data frame.

If data lists are to be coerced iteratively (e.g. repeated random subscripting and coercion), then speed can be enhanced by creating a \code{mold} explicitly with \code{data.list.mold} and then passing it to \code{as.data.frame.data.list} through the \code{mold} argument.  Because the \code{mold} will be the same for any data list of the same 'shape', the \code{mold} only needs to be created once before the iterations thereby reducing the need to iteratively create the \code{mold}.

When coercing from a data list to a list, consider the \code{drop.attr} argument carefully.  The default (TRUE) removes any data list-related \code{\link{attributes}}, which means that coercion back to a data list will be difficult if not impossible without explicitly specifying the lost structure via a call to \code{\link{data.list}}.  When \code{drop.attr = FALSE}, \code{as.list.data.list} is equivalent to \code{\link{unclass}}.

The \code{as.data.list.list} function is almost the same as \code{\link{data.list}}, except that it accepts a single \code{\link{list}} instead of a \code{...} argument.  The functions \code{as.data.list.data.frame} is currently not very useful, but is present for completeness and the possibility that it may one day do something interesting.
}
\value{The coerced object.}
