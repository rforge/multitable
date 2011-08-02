\name{data.list}
\alias{data.list}
\alias{fc.data.list}
\title{Create a data list}
\description{
Functions for creating a data list from vectors, matrices, arrays, data frames, and information on how these objects are related.
}
\usage{
data.list(..., dnames, match.dnames, check = TRUE)
fc.data.list(Y, X, Z, dnames = c("sites", "species"), respname = "community")
}
\arguments{
  \item{...}{A comma-separated collection of vectors, matrices, arrays, and data frames.}
  \item{dnames}{An optional vector of character strings giving the names of the replication dimensions of the data list.  If \code{\link{missing}}, these names are either set to \code{c("D1","D2",...)} or determined from information passed to the \code{match.dnames} argument if it is not \code{\link{missing}}.}
  \item{match.dnames}{A (possibly named) list of vectors, each associated with the elements of x, giving character strings of the names of the replication dimensions of these data structures.  the only replication dimension of a data frame is along the rows, as columns of data frames are taken to be variables. data frames are split into their component vectors in the returned data list. In \code{fc.data.list} this argument is simply a two-element vector with the names of the two replication dimensions of Y.}
  \item{check}{If TRUE, the structure of the created data list is checked for consistency.}
  \item{Y}{A matrix of response data in a fourth-corner problem.}
  \item{X}{A data frame of predictors for the rows of Y.}
  \item{Z}{A data frame of predictors for the columns of Y.}
  \item{respname}{The name of the response matrix, Y.}
}
\details{
This function creates data lists, which are multiple-table extensions of \code{\link{data.frame}}s.  With the \code{data.frame} function, a collection of vectors (of identical length) containing data are combined into a single object that can be passed to model-fitting and plotting functions.  In contrast, \code{data.list} function allows not just vectors of the same length in the collection but matrices and arrays with possibly (or even usually) different dimensions.  The rest of this details section assumes that the package vignette (DOESN'T EXIST YET!) has been read.  This vignette describes the conceptual framework of the data list.

The \code{data.list} function converts a collection of vectors, matrices, arrays, and data frames into a data list, by adding structure to a \code{\link{list}} containing this collection.  This additional structure specifies how the collection of vectors, matrices, arrays and data frames are related to each other, and is either determined automatically or supplied to the function via the \code{match.dnames} argument.  Each element of the list passed to \code{match.dnames} is associated with one of the objects in the collection (e.g. the first element corresponds to the first object in the collection).  In particular, the elements in \code{match.dnames} specify which dimensions the associated objects are replicated along.

There are several criteria that must be met for the collection of data objects to be validly converted into a data list.
\enumerate{
\item At least one object must be replicated along all dimensions
\item Don't know what comes next?
}

This dimensional structure is achieved by naming each dimension of replication with a character string.  Then each element in \code{rep.dim.names} consists of a vector of character strings giving the names of the dimensions in the corresponding element in \code{x}.  If this corresponding element is a vector, it is considered to have one dimensions (and hence one name in \code{rep.dim.names}.  For matrices and arrays, the number of dimensions (and hence the number of names) is obvious.  For data frames, there is only a single dimension (i.e. the rows) to be named because the other dimension (i.e. the columns) represents variables and not a dimension of replication.

Data frames supplied via \code{x} are handled somewhat differently from vectors, matrices, and arrays.  While each vector, matrix, and array are treated as a single variable in the produced data list, data frames are split up such that each variable (i.e. column) in the data frame is treated as its own variable in the produced data list.

During the production of a data list, one variable is singled out as the 'benchmark' variable.  See \code{\link{bm}} for further details on the benchmark concept.  Note that the dimensions of each variable are permuted such that their order matches that of the benchmark.

If \code{rep.dim.names} is missing, then \code{data.list} tries to automatically generate dimension names (with a warning).  This is risky because there is no guarantee that \code{data.list} will decide upon the desired pattern of dimension sharing.

The \code{fc.data.list} function is a wrapper for \code{data.list} for classic 'fourth-corner' data sets.  Such data consist of one matrix (\code{Y}) and two data frames (\code{X}, \code{Z}).  The matrix is typically a sites-by-species matrix of community data, while the data frames contain predictor variables for the sites and species respectively.  In particular, \code{X} (\code{Z}) is a data frame with as many rows as the size of the first (second) dimension of \code{Y}.  The names for the two replication dimensions of a fourth-corner data set are typically "sites" and "species", hence the default for \code{rep.dim.names}.  But this default can be changed.
}
\value{
A data list object which is a list with one element for each variable passed via \code{x}.  Each variable is given a "subsetdim" attribute, which is a logical vector with each element corresponding to one of the dimensions in the benchmark variable.  TRUE elements specify that this variable is replicated along the corresponding dimension, and FALSE indicates otherwise.  The data list object itself also contains the following attributes
\item{names}{Names of the variables}
\item{bm}{The index of the benchmark variable (see \code{\link{bm}})}
\item{repdim}{The replication dimensions (equal to the dimension attribute of the benchmark variable)}
}
\seealso{
\code{\link{as.data.frame.data.list}} for coercing to a data frame, \code{\link{Extract.data.list}} for subscripting the multiple tables in a data list simultaneously, and \code{\link{dim.data.list}}, \code{\link{dimnames.data.list}}, \code{\link{nvar}}, \code{\link{varnames}}, and \code{\link{print.data.list}} for other methods for \code{data.list} objects.
}