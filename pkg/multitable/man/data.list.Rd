\name{data.list}
\alias{data.list}
\title{Create a data list}
\description{
Functions for creating a data list from vectors, matrices, arrays, data frames, lists, and information on how these objects are related.
}
\usage{
data.list(..., dimids, match.dimids, check = TRUE, 
drop = TRUE, unique = TRUE)
}
\arguments{
  \item{...}{A comma-separated collection of vectors, matrices, arrays, data frames, and lists.}
  \item{dimids}{An optional vector of character strings giving the names of the replication dimensions of the data list.  If \code{\link{missing}}, these names are either set to \code{c("D1","D2",...)} or determined from information passed to the \code{match.dimids} argument if it is not \code{\link{missing}}.}
  \item{match.dimids}{A (possibly named) list of vectors, each associated with the elements of \code{...}, giving character strings of the names of the replication dimensions of these data structures.  The only replication dimension of a data frame is along the rows, as columns of data frames are taken to be variables. Data frames are split into their component vectors in the returned data list.  If missing, it is determined automatically if possible.}
  \item{check}{If TRUE, the structure of the created data list is checked for consistency.}
  \item{drop}{If TRUE, single dimension data lists are coerced to data frames (i.e. their replication dimensions are 'dropped').}
  \item{unique}{If TRUE, variable names are forced to be unique via \code{\link{make.names}}.}
}
\details{
This function creates data lists, which are multiple-table extensions of \code{\link{data.frame}}s.  With the \code{data.frame} function, a collection of vectors (of identical length) containing data are combined into a single object that can be passed to model-fitting and plotting functions.  In contrast, the \code{data.list} function allows not just vectors of the same length in the collection but matrices and arrays with possibly different dimensions.  The rest of this details section assumes that the package vignette (DOESN'T EXIST YET!) has been read.  This vignette describes the conceptual framework of the data list.

The \code{...} argument accepts a collection of vectors, matrices, arrays, data frames, and lists to be converted to a data list, by adding structure to a \code{\link{list}} containing this collection.  This additional structure specifies how the collection of vectors, matrices, arrays, data frames, and lists are related to each other, and is either determined automatically (if \code{match.dimids} is missing) or supplied by passing a list via the \code{match.dimids} argument.  Each element of the list passed to \code{match.dimids} is associated with one of the objects in the collection (e.g. the first element corresponds to the first object in the collection).  In particular, the elements in \code{match.dimids} specify which dimensions the associated objects are replicated along.  Each element in \code{match.dimids} should consist of a vector of character strings giving the names of the dimensions in the corresponding object in \code{...}.  If this corresponding element is a: \describe{
  \item{vector}{it is considered to have one dimension (and therefore one name in \code{match.dimids}),}
  \item{matrix or array}{ the number of dimensions (and therefore the number of names) is obvious.}
  \item{data frame}{there is only a single dimension (i.e. the rows) to be named because the columns represent variables and not a dimension of replication,}
  \item{list}{each element must be atomic with the same dimension attribute with the dimensions optionally named via \code{match.dimids}.}
}

To form a valid data list, at least one of the objects in \code{...} must be replicated along all dimensions.
 
During the production of a data list, one variable is singled out as the 'benchmark' variable.  See \code{\link{bm}} for further details on the benchmark concept.  Note that the dimensions of each variable are permuted such that their order matches that of the benchmark.
}

\note{
The \code{data.list} function is largely a wrapper for \code{\link{as.data.list.default}} that lets objects be combined into a data list via a \code{...} argument, as is done in \code{\link{data.frame}}.
}

\value{
A data list object which is a list with one element for each variable passed via \code{...} (note that each column in a data frame is treated as a separate variable, as is each element in a list).  Each variable is given a "subsetdim" attribute, which is a logical vector with each element corresponding to one of the dimensions in the benchmark variable.  TRUE elements specify that this variable is replicated along the corresponding dimension, and FALSE indicates otherwise.  The data list object itself also contains the following attributes
\item{names}{Names of the variables}
\item{match.dimids}{A list of vectors giving the names of the dimensions of replication for each variable (one vector per variable).}
\item{bm}{The index of the benchmark variable (see \code{\link{bm}})}
\item{repdim}{The replication dimensions (equal to the dimension attribute of the benchmark variable)}
}
\seealso{
\code{\link{as.data.frame.data.list}} for coercing to a data frame, \code{\link{Extract.data.list}} for subscripting the multiple tables in a data list simultaneously, and \code{\link{dim.data.list}}, \code{\link{dimnames.data.list}}, \code{\link{nvar}}, \code{\link{varnames}}, and \code{\link{print.data.list}} for other methods for \code{data.list} objects.
}
\examples{
a1 <- matrix(runif(50),10,5)
a2 <- matrix(runif(50),10,5)
a3 <- matrix(runif(50),10,5)
a <- list(a1,a2,a3)
b <- runif(10)
c <- letters[1:5]
(dl <- data.list(a,b,c))
}