\name{data.list}
\alias{data.list}
\title{Create a data list}
\description{
Function for creating a \code{data.list} object from vectors, matrices, arrays, data frames, lists, and information on how these objects are related.
}
\usage{
data.list(..., dimids, match.dimids, check = TRUE, 
drop = TRUE, unique = TRUE)
}
\arguments{
  \item{...}{A comma-separated collection of vectors, matrices, arrays, data frames, and lists, containing the variables that will comprise the resulting data list.}
  \item{dimids}{An optional vector of character strings giving identifiers for the replication dimensions of the data list.  If \code{\link{missing}}, these identifiers are either set to \code{c("D1","D2",...)} or determined from information passed to the \code{match.dimids} argument if it is not \code{\link{missing}}.}
  \item{match.dimids}{A (possibly named) list of character vectors, each associated with the elements of \code{...}, giving identifiers for the replication dimensions of these elements.  The only replication dimension of a data frame is along the rows, as columns of data frames are taken to be variables. Data frames are split into their component vectors in the returned data list.  If missing, it is determined automatically if possible.  See details on how automatic dimension matching is done.}
  \item{check}{If TRUE, the structure of the created data list is checked for consistency.}
  \item{drop}{If TRUE, single dimension data lists are coerced to data frames (i.e. their replication dimensions are 'dropped').}
  \item{unique}{If TRUE, variable names are forced to be unique via \code{\link{make.names}}.}
}
\details{
This function creates data lists, which are multiple-table extensions of \code{\link{data.frame}}s.  With the \code{data.frame} function, a collection of vectors (of identical length) containing data are combined into a single object that can be passed to model-fitting and plotting functions.  In contrast, the \code{data.list} function allows not just vectors of the same length in the collection but matrices and arrays with possibly different dimensions.

The \code{data.list} function creates objects of class \code{data.list}, which are collections of variables (i.e. vectors, matrices, and arrays).  These variables are related because they share dimensions of replication.  For example, a \code{matrix}-valued variable might share its first dimension with the only dimension of a \code{vector}-valued variable.  See \code{vignette("multitable")} for more information on the structure of data lists. 

The \code{...} argument accepts a collection of vectors, matrices, arrays, data frames, and lists to be converted to a data list.  These different types of objects are used by \code{data.list} in different ways: \describe{
  \item{\code{vector}}{Becomes a variable in the resulting data list with a single dimension of replication. In particular, a \code{vector} without a dimension attribute is converted to a one-dimensional \code{array}.}
  \item{\code{matrix}}{Becomes a variable in the resulting data list with two dimensions of replication.}
  \item{\code{array}}{Becomes a variable in the resulting data list with the same number of dimensions as the array itself.}
  \item{\code{data.frame}}{Each column becomes a variable with a single dimension.}
  \item{\code{list}}{Each element becomes a variable. It is required that each element be either a \code{vector}, \code{matrix}, or \code{array}, and that they all have the same value for their dimension attributes.}
}

The pattern of dimension sharing between the variables is either determined automatically (if \code{match.dimids} is missing) or supplied by passing a list via the \code{match.dimids} argument.  Automatic dimension matching proceeds in two steps.  First, \code{data.list} tries to deduce the pattern of dimension matching through the names of the dimensions of the objects passed to \code{...}.  Different names are used for the different types of objects: \describe{
  \item{\code{vector}}{The \code{names} attribute.}
  \item{\code{matrix} or \code{array}}{The \code{dimnames} attribute.}
  \item{\code{data.frame}}{The \code{row.names} attribute.}
  \item{\code{list}}{Either the \code{names} or \code{dimnames} attribute, depending on which one its elements posses.}
}
For example, if the \code{names} attribute of a \code{vector} is identical to the first element in the \code{dimnames} attribute of an \code{array}, then the single dimension of this \code{vector} is matched with the first dimension of this \code{array}.  Dimension matching by naming is the recommended method, because it requires thought about the relationships between the variables and therefore ensures that the structure of the data are well-understood.

If dimension matching via the names of the dimensions fails, then \code{data.list} tries to infer the pattern of matching by the sizes of the dimensions of its variables.  This method will fail if (1) any object has two or more dimensions of the same size AND (2) at least one other object also has at least one other dimension of the same size.  In the case of failure, an error message is reported suggesting that either the dimensions of the variables be named or that explicit dimension matching be supplied as a \code{list} via the \code{match.dimids} argument.

Each element of the list passed to \code{match.dimids} (i.e. match dimension identifiers) is associated with one of the objects in the collection (e.g. the first element corresponds to the first object in the collection).  In particular, the elements in \code{match.dimids} specify which dimensions the associated objects are replicated along.  Each element in \code{match.dimids} should consist of a vector of character strings identifying the dimensions in the corresponding object in \code{...}.  Dimensions in different objects will be considered shared if they share the same identifier passed to \code{match.dimids}.  The specification of dimension identifiers depends on the associated type of object passed to \code{...}: \describe{
  \item{\code{vector}}{A single string identifying the only dimension.}
  \item{\code{matrix}}{A length-2 character vector identifying the first and second dimensions.}
  \item{\code{array}}{A length-n character vector identifying the n dimensions.}
  \item{\code{data.frame}}{A single string identifying the dimension associated with the rows.  Each column is given the same dimension identifier.}
  \item{\code{list}}{A length-n character vector identifying the n dimensions of the elements of the list.  Each element is given the same set of dimension identifiers.}
}

To form a valid data list, at least one of the objects in \code{...} must be replicated along all dimensions.
 
During the production of a data list, one variable is singled out as the 'benchmark' variable.  See \code{\link{bm}} for further details on the benchmark concept.  Note that the dimensions of each variable are permuted such that their order matches that of the benchmark.
}

\note{
The \code{data.list} function is largely a wrapper for \code{\link{as.data.list.default}} that lets objects be combined into a data list via a \code{...} argument, as is done in \code{\link{data.frame}}.
}

\value{
A \code{data.list} object which is a list with one element for each variable passed via \code{...} (note that each column in a data frame is treated as a separate variable, as is each element in a list).  Each variable is given a "subsetdim" attribute, which is a logical vector with each element corresponding to one of the dimensions in the benchmark variable.  TRUE elements specify that this variable is replicated along the corresponding dimension, and FALSE indicates otherwise.  The \code{data.list} object itself also contains the following attributes:
\item{names}{Names of the variables}
\item{match.dimids}{A list of vectors giving the names of the dimensions of replication for each variable (one vector per variable).}
\item{bm}{The index of the benchmark variable (see \code{\link{bm}})}
\item{repdim}{The replication dimensions (equal to the dimension attribute of the benchmark variable)}
}
\seealso{
\code{\link{as.data.frame.data.list}} for coercing to a data frame, \code{\link{Extract.data.list}} for subscripting the multiple tables in a data list simultaneously, and \code{\link{dim.data.list}}, \code{\link{dimnames.data.list}}, \code{\link{nvar}}, \code{\link{varnames}}, and \code{\link{print.data.list}} for other methods for \code{data.list} objects.  If your data are originally in (database-like) `long' format data frames, then use \code{\link{dlcast}} for creating data lists.  If your data are originally in text files, use \code{\link{read.multitable}}.
}
\examples{
## Automatic dimension matching by the sizes of dimensions.
## Note that this example would not work if all of the 10's were
## changed to 5's.  This example also illustrates how to pass
## several variables through one list, as long as each variable
## shares the same dimensions (in this case 10-by-5 matrices).
## We also see here how \code{data.list} automatically converts 
## character vectors to factors.
a1 <- matrix(runif(50),10,5)
a2 <- matrix(runif(50),10,5)
a3 <- matrix(runif(50),10,5)
a <- list(a1,a2,a3)
b <- runif(10)
c <- letters[1:5]
data.list(a,b,c)

## Here we illustrate the use of dimension matching by
## dimension naming.
a <- lapply(a,`dimnames<-`,list(letters[1:10],LETTERS[1:5]))
names(b) <- letters[1:10]
names(c) <- LETTERS[1:5]
data.list(a,b,c)

## If we want to name the dimension identifiers themselves
## we can use \code{dimids}.
data.list(a,b,c,dimids=c("small letters","large letters"))

## Or we could explicitly specify the pattern of dimension
## sharing using \code{match.dimids}.
md <- list(
	c("small letters","large letters"),
	"small letters",
	"large letters")
data.list(a,b,c,match.dimids=md)
}