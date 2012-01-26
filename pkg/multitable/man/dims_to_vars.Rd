\name{dims_to_vars}
\alias{dims_to_vars}
\title{
Dimensions of replication to variables
}
\description{
Creates a new data list from an old one that includes categorical variables indicating replicates along specified dimensions of replication.
}
\usage{
dims_to_vars(dl, dimids)
}
\arguments{
  \item{dl}{A data list}
  \item{dimids}{A character vector containing the identifier(s) for the dimension(s) that are to be used to create the new variable(s).  If \code{\link{missing}}, all of the dimensions are used.}
}
\value{
A data list.
}
\examples{
data(fake.community)

fc0 <- fake.community
fc1 <- dims_to_vars(fake.community)
fc2 <- dims_to_vars(fake.community, "years")

summary(fc0)
summary(fc1)
summary(fc2)
}