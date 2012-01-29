\name{Data_list_arithmetic}
\alias{Data_list_arithmetic}
\alias{Ops.data.list}

\title{
Data list arithmetic
}
\description{
Operators for adding (subtracting) the variables in one data list to (from) the variables in another data list.
}
\usage{
\method{Ops}{data.list}(e1, e2)
}
\arguments{
  \item{e1}{A data list}
  \item{e2}{Another data list}
}
\value{Yet another data list}

\seealso{
Useful with \code{\link{variable}} and \code{\link{variableGroup}}.
}
\examples{
variable(matrix(runif(15), 5, 3), c("n","m"), "A") + 
	variable(letters[1:3], "m", "B")

data(fake.community)
fake.community + variable(c(10, 0, 3), "species", "cuteness")
}