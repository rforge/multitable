\name{data.list}
\alias{data.list}
\alias{fc.data.list}
\title{Create a data list}
\description{
Functions for creating a data list from vectors, matrices, arrays, data frames, and information on how these objects are related.
}
\usage{
data.list(x, rep.dim.names, check = TRUE)
fc.data.list(Y, X, Z, rep.dim.names = c("sites", "species"), respname = "community")
}
\arguments{
  \item{x}{A list of vectors, matrices, arrays, and data frames.}
  \item{rep.dim.names}{A (possibly named) list of vectors, each associated with the elements of x, giving character strings of the names of the replication dimensions of these data structures.  the only replication dimension of a data frame is along the rows, as columns of data frames are taken to be variables. data frames are split into their component vectors in the returned data list. In \code{fc.data.list} this argument is simply a two-element vector with the names of the two replication dimensions of Y.}
  \item{check}{If TRUE, the structure of the created data list is checked for consistency.}
  \item{Y}{A matrix of response data in a fourth-corner problem.}
  \item{X}{A data frame of predictors for the rows of Y.}
  \item{Z}{A data frame of predictors for the columns of Y.}
  \item{respname}{The name of the response matrix, Y.}
}
\value{
A data list.
}