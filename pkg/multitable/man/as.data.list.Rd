\name{as.data.list}
\alias{as.data.list}
\alias{as.data.list.default}
\alias{as.data.frame.data.list}
\alias{as.matrix.data.list}
\alias{as.list.data.list}
\alias{data.list.mold}
\title{Coercion to and from data lists}
\description{
These functions coerce lists and data frame objects to data lists and vice versa.}
\usage{
as.data.list(x, ...)

data.list.mold(x)

\method{as.data.list}{default}(x, dnames, match.dnames, check = TRUE,...)

\method{as.data.frame}{data.list}(x, row.names = NULL, 
optional = FALSE, scheme = "repeat", mold, ...)

\method{as.matrix}{data.list}(x, ...)

\method{as.list}{data.list}(x, drop.attr = TRUE, ...)
}
\arguments{
  \item{x}{An object. Either a data.list, data.frame, or list.}
  \item{mold}{A list typically obtained as output from \code{\link{data.list.mold}}.  See details.}
  \item{dnames}{See \code{\link{data.list}} for an explanation.}
  \item{match.dnames}{See \code{\link{data.list}} for an explanation.}
  \item{check}{See \code{\link{data.list}} for an explanation.}
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

The \code{as.data.list.default} function is designed to work on arrays, matrices, vectors, data.frames, and lists of such objects.

The \code{as.matrix} method for data lists returns either a single matrix (if all variables are numeric or all variables are either factors or character); otherwise, two matrices are returned, one with the numeric and one with the factor variables respectively.  The matrices are returned by first coercing to data frames and then to matrices.  This method was created as a result of a suggestion by Philip Dixon.
}
\value{The coerced object.}
