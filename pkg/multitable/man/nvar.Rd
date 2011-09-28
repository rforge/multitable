\name{nvar}
\alias{nvar}
\title{Number of variables in a data list}
\description{
Extract the number of variables in a data list object.
}
\usage{
nvar(dl)
}
\arguments{
  \item{dl}{A \code{\link{data.list}} object.}
}
\details{Implemented as simply \code{length(dl)}.}
\value{The number of variables in \code{dl}.}
\examples{
data(fake.community)
nvar(fake.community) # 6
}
