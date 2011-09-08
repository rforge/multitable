\name{dlcast}
\alias{dlcast}
\title{Cast data lists}
\description{
Cast a list of molten data frames into a data list.
}
\usage{
dlcast(x,dimids,fill)
}
\arguments{
  \item{x}{A list of molten data frames}
  \item{dimids}{See \code{\link{data.list}}}
  \item{fill}{A vector the same \code{\link{length}} as \code{x}, giving the value to use for structural missing values for each of the data frames in \code{x}.}
}
\details{
For the purposes of \code{dlcast}, a molten data frame is a data frame with one named column for each variable, one named column for each dimension of replication (called \code{idvar}s in the \code{reshape2} package), and one row for each (possibly multivariate) sample.  Several data frames will allow variables to differ in their dimensions of replication.  However,  in order to combine them into a valid data list: \itemize{
\item{each data frames must share at least one dimension of replication with at least one other data frame and}
\item{at least one data frame must contain all of the dimensions of replication in all of the data frames.}
}
}
\note{
This function is inspired by the \code{acast} function in the \code{reshape2} package by Hadley Wickham.  Levi Waldron was a great help during the writing of \code{dlcast}.}
\value{
A data list.
}
\examples{
## a fictitious data set with a fourth-corner problem

abundance <- data.frame(
	sites=c(
		"midlatitude","subtropical","tropical","equatorial",
		"arctic","midlatitude","tropical","equatorial",
		"subtropical"
	),
	species=c(rep("capybara",4),rep("moss",4),"vampire"),
	abundance=c(4,10,8,7,5,6,9,3,1)
)

environment <- data.frame(
	sites=c(
		"arctic","subarctic","midlatitude","subtropical",
		"tropical","equatorial"
	),
	temperature=c(-30,0,10,20,50,30),
	precipitation=c(20,40,20,100,150,200)
)

trait <- data.frame(
	species=c("capybara","moss","vampire"),
	body.size=c(140,5,190),
	metabolic.rate=c(20,5,0)
)

dlcast(list(abundance,environment,trait),
	dimids=c("sites","species"),
	fill=c(0,NA,NA)
)
}