\name{aperm.data.list}
\alias{aperm.data.list}
\title{Data list transposition}
\description{Transpose a data list by permuting its dimensions.}
\usage{
\method{aperm}{data.list}(a, perm, ...)
}
\arguments{
  \item{a}{the data list to be transposed.}
  \item{perm}{the subscript permutation vector (see \code{\link{aperm}}).}
  \item{...}{not currently used.}
}
\note{
Does not allow \code{resize = FALSE}, as can be done in the default method (\code{\link{aperm}}).

Currently \code{aperm.data.list} is implemented by converting the data list to a list, transposing the benchmark variable (see \code{\link{data.list}} for a description of the benchmark concept), and creating a new data list object. This works because the \code{\link{data.list}} function automatically transposes the other variables such that their dimensions are in the same order as the benchmark.  However, this implementation is perhaps not as efficient as it could be and may be changed in the future.
}
\value{
A transposed version of the data list with the replication dimensions permuted as indicated by \code{perm}.
}
\seealso{
\code{\link{aperm}}
}