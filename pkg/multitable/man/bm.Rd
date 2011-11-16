\name{bm}
\alias{bm}
\title{Data list benchmark}
\description{
Extract the 'benchmark' variable from a data list.
}
\usage{
bm(x)
}
\arguments{
  \item{x}{A data list.}
}
\details{
When \code{\link{data.list}}, \code{\link{as.data.list}}, \code{\link{dlcast}}, or \code{\link{read.multitable}} is used to create a new data list object, one of the variables in this object is designated to be the 'benchmark' variable.  The first variable passed to \code{\link{data.list}} that is replicated along every dimension is designated the benchmark.  The "bm" attribute in a \code{data.list} object gives the subscript of the benchmark variable.  Thus, the \code{bm} function is a single line:  \code{x[[attr(x, "bm")]]}.

Although any variable that is replicated along every dimension can serve as the benchmark, it is recommended that the benchmark be a response variable if such a variable exists.  Therefore, such a response variable should be the first variable passed to \code{\link{data.list}}.

The purpose of the benchmark is to provide a frame of reference for other less-replicated variables in a data list and is a prototype of the replication in a data list as a whole.  In this way, one may access the names of the dimensions of the data list simply by accessing the names of the dimensions of the benchmark.  Indeed, this is how \code{\link{dimnames.data.list}} works.
}
\value{
The benchmark variable.
}
\seealso{
\code{\link{data.list}}; \code{\link{as.data.list}}
}
\examples{
data(fake.community)
bm(fake.community)
}
