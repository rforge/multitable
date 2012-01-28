\name{variable}
\alias{variable}
\alias{variableGroup}
\title{
Create simple data lists
}
\description{
These functions create data lists with a single variable (\code{variable}) or several variables with identical dimensions of replication (\code{variableGroup}).
}
\usage{
variable(name, x, dimids)
variableGroup(x, dimids)
}
\arguments{
  \item{name}{Character string giving the name of the variable}
  \item{x}{For \code{variable}, the variable (vector, matrix, or array); for \code{variableGroup}, a \code{\link{data.frame}} or \code{\link{list}} with the variables in the group.}
  \item{dimids}{Character vector of identifiers for the dimensions of replication of the variable(s).}
}
\value{A \code{\link{data.list}} object.}
\seealso{
Designed to be used in variable arithmetic (see \code{\link{Ops.data.list}}).

To create more complex data lists see:  \code{\link{data.list}}; \code{\link{as.data.list}}; \code{\link{dlcast}}; \code{\link{read.multitable}}
}
\examples{
##
variable("a variable", runif(5), "a dimension")

## stringing variables together using the + operator
variable("A", matrix(runif(15), 5, 3), c("n","m")) + 
variable("B", letters[1:3], "m") + 
variable("C", runif(5), "n") + 
variable("D", array(runif(15*4), c(3,5,4)), c("m","n","p")) +
variableGroup(data.frame(a = runif(5), b = runif(5)), "n") + 
variableGroup(list(
	c = matrix(runif(20), 4, 5),
	d = matrix(runif(20), 4, 5)
), c("p","n"))
}