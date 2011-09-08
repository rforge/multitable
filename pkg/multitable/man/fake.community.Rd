\name{fake.community}
\alias{fake.community}
\docType{data}
\title{
Fake community data
}
\description{
A simple and silly data set for illustrating multiple-table data organization and manipulation.
}
\usage{data(fake.community)}
\format{
  A \code{\link{data.list}} object with six fictitious variables (abundance, precipitation, body size, metabolic rate, and homeotherm) replicated along three dimensions (sites, years, species):
\tabular{lllllll}{
\tab abundance \tab temperature \tab precipitation \tab body.size \tab metabolic.rate \tab homeotherm \cr
sites \tab TRUE \tab TRUE \tab TRUE \tab FALSE \tab FALSE \tab FALSE \cr
years \tab TRUE \tab TRUE \tab TRUE \tab FALSE \tab FALSE \tab FALSE \cr
species \tab TRUE \tab FALSE \tab FALSE \tab TRUE \tab TRUE \tab TRUE
}
}
\source{
My brain.
}
\examples{
data(fake.community)
str(fake.community)
summary(fake.community)
}
\keyword{datasets}
