\name{dim.data.list}
\alias{dim.data.list}
\alias{repdim}
\alias{dim<-.data.list}
\title{Replication dimensions of a data list}
\description{
Retrieve or set the replication dimensions of a data list object.
}
\usage{
repdim(x)

\method{dim}{data.list}(x)

\method{dim}{data.list}(x) <- value
}
\arguments{
  \item{x}{A data list.}
  \item{value}{Any value for \code{value} will result in an error (see details).}
}
\details{
For extraction, \code{repdim} and \code{dim.data.list} are identical.

Currently, \code{dim<-.data.list} simply returns an error because in the vast majority of cases it will not make sense to set the replication dimensions in this way.
}
\value{
The \code{repdim} attribute of \code{x} (see \code{\link{data.list}} for information on this attribute).
}
\seealso{
\code{\link{dim}}; \code{\link{dimnames.data.list}}.
}
\examples{
data(fake.community)
dim(fake.community)
repdim(fake.community)
}
