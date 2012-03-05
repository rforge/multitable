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
  \item{mold}{A list typically obtained as output from \code{\link{data.list.mold}}.  If \code{\link{missing}}, then \code{mold} is automatically created.  See details.}
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
Ultimately, all functions that create \code{data.list} objects (i.e. \code{\link{data.list}}; \code{\link{dlcast}}; \code{\link{read.multitable}}; \code{\link{variable}}; \code{\link{variableGroup}}) are wrappers for \code{as.data.list}.  From a practical perspective the \code{as.data.list} function is very similar to \code{data.list}, with the following principal differences: \itemize{
  \item{The \code{as.data.list} function requires that multiple objects be organised in a \code{list} (\code{x}) whereas \code{data.list} uses a \code{...} argument.}
  \item{Like \code{data.frame}, \code{data.list} automatically names variables as \code{\link{character}} versions of the names passed to \code{...}, whereas variables must be named explicitly with \code{as.data.list}.}
}

The \code{is.data.list} function tests if an object inherits from class \code{data.list}.

By default, \code{as.data.frame.data.list} works in two steps.  First, a \code{mold} of the coercion process is created with \code{data.list.mold}.  The \code{mold} is a list of vectors, each with \code{\link{length}} equal to the number of rows in the coerced data frame.  Each vector is associated with a particular variable in \code{x} and contains indices that point to the elements of that variable.  Second, the \code{mold} is 'filled with' (i.e. used to subscript) \code{x} thereby producing the coerced data frame.

If data lists are to be iteratively coerced to data frames (e.g. repeated random subscripting and coercion), then speed can be enhanced by creating a \code{mold} explicitly with \code{data.list.mold} and then passing it to \code{as.data.frame.data.list} through the \code{mold} argument.  Because the \code{mold} will be the same for any data list of the same 'shape', the \code{mold} only needs to be created once before the iterations thereby eliminating the need to iteratively create the \code{mold}.

The \code{as.matrix} method for data lists returns either a single matrix (if all variables are numeric or all variables are either factors or character); otherwise, two matrices are returned, one with the numeric and one with the factor variables respectively.  The matrices are returned by first coercing to data frames and then to matrices.  This method was created as a result of a suggestion by Philip Dixon.

The \code{as.list.data.list} function is equivalent to \code{unclass} if \code{drop.attr = FALSE}, otherwise (i.e. \code{drop.attr = TRUE})  the \code{"match.dimids"}, \code{"bm"}, and \code{"repdim"} attributes are dropped from \code{x} and the \code{"subsetdim"} attributes are dropped from each variable in \code{x}.
}
\value{The coerced object.}
\seealso{
\code{\link{as.data.frame}}; \code{\link{as.list}}; \code{\link{as.matrix}}; \code{\link{variablize}}; \code{\link{data.list}}; \code{\link{dlcast}}; \code{\link{read.multitable}}.
}
\examples{
data(fake.community)
fake.community

## testing for data list
is.data.list(fake.community)			# TRUE
is.data.list(fake.community$abundance)	# FALSE
is.array(fake.community$abundance)	# TRUE

## this example illustrates the main difference between
## \code{data.list} and \code{as.data.list}. compare with 
## the similar example in \code{\link{data.list}}. notice
## the naming problems with the latter method.
a1 <- matrix(runif(50), 10, 5)
a2 <- matrix(runif(50), 10, 5)
a3 <- matrix(runif(50), 10, 5)
a <- list(a1, a2, a3)
b <- runif(10)
c <- letters[1:5]
data.list(a, b, c)
as.data.list(list(a, b, c))

## coercing to a data frame with explicit mold creation
fake.mold <- data.list.mold(fake.community)
fake.mold
as.data.frame(fake.community)
as.data.frame(fake.community, mold = fake.mold)

## we can coerce a similar data list faster using this
## \code{fake.mold} object
fc.rnd <- fake.community
fc.rnd$abundance <- fc.rnd$abundance[sample(6), , ]
as.data.frame(fc.rnd,mold = fake.mold)

## unless drop=FALSE, simple data lists are immediately
## coerced into data frames -- simple data don't require
## data lists
x <- runif(10)
as.data.list(x)
as.data.list(x, drop = FALSE)

## different ways to coerce a data list to a list.
## each way can produce slightly different results.
as.list(fake.community)
as.list(fake.community, drop.attr = FALSE)
unclass(fake.community)
lapply(fake.community, I)
}
