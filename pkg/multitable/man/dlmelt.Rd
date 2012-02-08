\name{dlmelt}
\alias{dlmelt}
\title{Melt data lists}
\description{
Melt a data list into a list of molten (i.e. long-format) (i.e. database-like) data frames.
}
\usage{
dlmelt(x)
}
\arguments{
  \item{x}{A data list}
}
\note{
This function is inspired by the \code{melt} function in the \code{reshape2} package by Hadley Wickham.}
\value{
A data list.
}
\seealso{
For an approximate inverse of \code{dlmelt}, see \code{\link{dlcast}}.
}
\examples{
data(fake.community)
dlmelt(fake.community)
}
