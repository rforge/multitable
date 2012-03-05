\name{dimnames.data.list}
\alias{dimnames.data.list}
\alias{dimnames<-.data.list}
\title{Dimnames of a data list object}
\description{Retrieve or set the dimnames of a data list object.}
\usage{
\method{dimnames}{data.list}(x)
\method{dimnames}{data.list}(x) <- value
}
\arguments{
  \item{x}{A \code{\link{data.list}} object.}
  \item{value}{A list with each element containing a character vector with the names of the dimensions of the data list, \code{x}.}
}
\seealso{
\code{\link{dimnames}} for the default method and \code{\link{dim.data.list}} to retrieve and set the dimensions of a data list.
}
\examples{
data(fake.community)
fake.community

dimnames(fake.community)
dimnames(fake.community)[[1]] <- letters[1:6]
dimnames(fake.community)
dimnames(fake.community) <- list(
	letters[7:12], letters[13:15], letters[16:18])
dimnames(fake.community)	
}
