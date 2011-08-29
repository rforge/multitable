\name{dlcast}
\alias{dlcast}
\title{Cast data lists}
\description{
Cast a list of molten data frames into a data list.
}
\usage{
dlcast(x,dnames,fill)
}
\arguments{
  \item{x}{A list of molten data frames}
  \item{dnames}{See \code{\link{data.list}}}
  \item{fill}{A vector the same \code{\link{length}} as \code{x}, giving the value to use for structural missing values for each of the data frames in \code{x}.}
}
\note{
This function is inspired by the \code{acast} function in the \code{reshape2} package by Hadley Wickem.}
\value{
A data list.
}