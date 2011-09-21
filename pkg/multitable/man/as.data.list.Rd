\name{as.data.list}
\alias{as.data.list}
\alias{is.data.list}
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

is.data.list(x)

data.list.mold(x)

\method{as.data.list}{default}(x, dimids, match.dimids,
check = TRUE, drop = TRUE,...)

\method{as.data.frame}{data.list}(x, row.names = NULL, 
optional = FALSE, scheme = "repeat", mold, ...)

\method{as.matrix}{data.list}(x, ...)

\method{as.list}{data.list}(x, drop.attr = TRUE, 
factorsTOstrings = FALSE, ...)
}
\arguments{
  \item{x}{An object. Either a data.list, data.frame, or list.}
  \item{mold}{A list typically obtained as output from \code{\link{data.list.mold}}.  See details.}
  \item{dimids}{See \code{\link{data.list}} for an explanation.}
  \item{match.dimids}{See \code{\link{data.list}} for an explanation.}
  \item{check}{See \code{\link{data.list}} for an explanation.}
  \item{drop}{If TRUE, single dimension data lists are coerced to data frames (i.e. their replication dimensions are 'dropped').}
  \item{row.names}{Passed to the default method after some processing.}
  \item{optional}{Passed to the default method after some processing.}
  \item{scheme}{Type of coercion.  Currently, only "repeat" is allowed but others may follow.}
  \item{drop.attr}{Should the attributes of the data list be dropped?}
  \item{factorsTOstrings}{Should factors be converted to character strings?}
  \item{...}{Additional arguments (passed to the default method in \code{as.data.frame.data.list}).}
}
\details{
See vignette on coercing data lists into data frames.

By default, \code{as.data.frame.data.list} works in two steps.  First, a \code{mold} of the coercion process is created with \code{data.list.mold}.  The \code{mold} is a list of vectors, each with \code{\link{length}} equal to the number of rows in the coerced data frame.  Each vector is associated with a particular variable in \code{x} and contains indices that point to the elements of that variable.  Second, the \code{mold} is 'filled with' (i.e. used to subscript) \code{x} thereby producing the coerced data frame.

If data lists are to be iteratively coerced to data frames (e.g. repeated random subscripting and coercion), then speed can be enhanced by creating a \code{mold} explicitly with \code{data.list.mold} and then passing it to \code{as.data.frame.data.list} through the \code{mold} argument.  Because the \code{mold} will be the same for any data list of the same 'shape', the \code{mold} only needs to be created once before the iterations thereby reducing the need to iteratively create the \code{mold}.

The \code{as.data.list.default} function is designed to work on arrays, matrices, vectors, data.frames, and lists of such objects.  The \code{\link{data.list}} function is based on \code{as.data.list.default}, and is recommended for data list creation.  The documentation for \code{\link{data.list}} contains more information about \code{as.data.list.default}.

The \code{as.list.data.list} function is equivalent to \code{unclass} if \code{drop.attr = FALSE}, otherwise (i.e. \code{drop.attr = TRUE})  the \code{"match.dimids"}, \code{"bm"}, and \code{"repdim"} attributes are dropped from \code{x} and the \code{"subsetdim"} attributes are dropped from each variable in \code{x}.

The \code{as.matrix} method for data lists returns either a single matrix (if all variables are numeric or all variables are either factors or character); otherwise, two matrices are returned, one with the numeric and one with the factor variables respectively.  The matrices are returned by first coercing to data frames and then to matrices.  This method was created as a result of a suggestion by Philip Dixon.

The \code{is.data.list} function tests if an object inherits from class \code{data.list}.
}
\value{The coerced object.}
\seealso{
\code{\link{as.data.frame}}; \code{\link{as.list}}; \code{\link{as.matrix}}; \code{\link{variablize}}; \code{\link{data.list}}
}
\examples{
data(fake.community)
fake.community

fake.mold <- data.list.mold(fake.community)
fake.mold
as.data.frame(fake.community)
as.data.frame(fake.community,mold=fake.mold)

x <- runif(10)
as.data.list(x)
as.data.list(x,drop=FALSE)

as.list(fake.community)
as.list(fake.community,drop.attr=FALSE)
unclass(fake.community)
}
