\name{variablize}
\alias{variablize}
\alias{variablize.default}
\alias{variablize.factor}
\alias{variablize.data.list}
\title{Variablization}
\description{Convert all but the first dimension of replication of an array or data list into variables}
\usage{
variablize(x, ...)

\method{variablize}{default}(x, ...)

\method{variablize}{factor}(x, ...)

\method{variablize}{data.list}(x, ...)
}
\arguments{
  \item{x}{A data list}
  \item{...}{Not currently used}
}
\value{
A data frame with the first dimension of \code{x} representing the observations (rows) and all other dimensions of \code{x} treated as variables (columns). If \code{x} is a \code{\link{data.list}} then any variable not replicated along the first dimension is removed.
}
\note{
To use a dimension other than the first as the rows of the data frame, transpose the array (\code{\link{aperm}}) or data list (\code{\link{aperm.data.list}}) first.
}
\seealso{
\code{\link{aperm.data.list}},\code{\link{data.list}}
}
