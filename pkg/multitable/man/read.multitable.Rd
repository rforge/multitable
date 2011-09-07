\name{read.multitable}
\alias{read.multitable}
\alias{read.multicsv}
\alias{read.multidelim}
\alias{read.fourthcorner}
\alias{multifile.choose}
\title{Multiple table data input}
\description{
These functions are used to read several text files and create a data list from them.
}
\usage{
read.multitable(files, dnames, tnames, fill = rep(NA,length(files)), ...)

read.multicsv(files, dnames, tnames, fill = rep(NA,length(files)), ...)

read.multidelim(files, dnames, tnames, fill = rep(NA,length(files)), ...)

read.fourthcorner(community, environment, traits, dnames=c("sites","species"), 
community.name="abundance", ...)

multifile.choose(n)
}
\arguments{
  \item{files}{A character vector with the names of the files containing the tables (possibly created with \code{multifile.choose}).}
  \item{community}{A character string of the name of the file containing the community data of a fourth-corner problem.}
  \item{environment}{A character string of the name of the file containing the environment data of a fourth-corner problem.}
  \item{traits}{A character string of the name of the file containing the trait data of a fourth-corner problem.}
  \item{dnames}{A vector with the names of the columns associated with replication dimensions.  For \code{read.fourthcorner} this is simply the names of the replication dimensions.}
  \item{tnames}{An optional vector with the names of the tables. If missing the filenames are used after being passed through \code{\link{make.names}}.}
  \item{fill}{See \code{\link{dlcast}}.}
  \item{community.name}{Character string of the name of \code{community} matrix.}
  \item{n}{Number of files to choose.}
  \item{...}{Additional arguments to pass to \code{\link{read.table}}. Note that headers MUST be present in order to distinguish columns for replication dimensions from columns for variables, and therefore \code{header} is set to \code{TRUE} and cannot be changed.  Exception:  \code{header} may be missing in \code{read.fourthcorner}.}
}
\details{
The \code{read.multitable} function is a multiple-table version of 
}
\value{
A \code{\link{data.list}} object.
}
\examples{
abundance.file <- tempfile()
environment.file <- tempfile()
trait.file <- tempfile()

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

write.table(abundance,abundance.file,sep=",")
write.table(environment,environment.file,sep=",")
write.table(trait,trait.file,sep=",")

files <- c(abundance.file,environment.file,trait.file)
dnames <- c("sites","species")
read.multicsv(files,dnames,fill=c(0,NA,NA))
}