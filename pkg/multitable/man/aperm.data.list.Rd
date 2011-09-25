\name{aperm.data.list}
\alias{aperm.data.list}
\alias{t.data.list}
\title{Data list transposition}
\description{Transpose a data list by permuting its dimensions.}
\usage{
\method{aperm}{data.list}(a, perm, ...)

\method{t}{data.list}(x)
}
\arguments{
  \item{a}{a data list to be transposed.}
  \item{x}{a data list to be transposed.}
  \item{perm}{the subscript permutation vector (see \code{\link{aperm}}).}
  \item{...}{not currently used.}
}
\note{
Does not allow \code{resize = FALSE}, as can be done in the default method (\code{\link{aperm}}).

Currently \code{aperm.data.list} is implemented by converting the data list to a list, transposing the benchmark variable (see \code{\link{data.list}} for a description of the benchmark concept), and creating a new data list object. This works because the \code{\link{as.data.list}} function automatically transposes the other variables such that their dimensions are in the same order as the benchmark.  However, this implementation is perhaps not as efficient as it could be and may be changed in the future.

\code{t.data.list} is equivalent to \code{aperm.data.list} with the \code{perm} argument \code{\link{missing}}, which results in a data list for which the order of subscripts is reversed (as in the default method, \code{\link{aperm}}).
}
\value{
A transposed version of the data list with the replication dimensions permuted as indicated by \code{perm}.
}
\seealso{
\code{\link{aperm}}, \code{\link{t}}
}