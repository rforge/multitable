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
read.multitable(files, dimids, fill = rep(NA,length(files)), ...)

read.multicsv(files, dimids, fill = rep(NA,length(files)), ...)

read.multidelim(files, dimids, fill = rep(NA,length(files)), ...)

read.fourthcorner(community, environment, traits, dimids=c("sites","species"), 
community.name="abundance", ...)

multifile.choose(n)
}
\arguments{
  \item{files}{A character vector with the names of the files containing the tables (possibly created with \code{multifile.choose}).}
  \item{community}{A character string of the name of the file containing the community data of a fourth-corner problem.}
  \item{environment}{A character string of the name of the file containing the environment data of a fourth-corner problem.}
  \item{traits}{A character string of the name of the file containing the trait data of a fourth-corner problem.}
  \item{dimids}{A vector with the names of the columns associated with replication dimensions.  For \code{read.fourthcorner} this is simply the names of the replication dimensions.}
  \item{fill}{See \code{\link{dlcast}}.}
  \item{community.name}{Character string of the name of \code{community} matrix.}
  \item{n}{Number of files to choose.}
  \item{...}{Additional arguments to pass to \code{\link{read.table}}.  The most important such argument is \code{sep}, which specifies how entries are separated in the data files.  Note that headers MUST be present in order to distinguish columns for replication dimensions from columns for variables, and therefore \code{header} is set to \code{TRUE} and cannot be changed.  Exception:  \code{header} may be missing in \code{read.fourthcorner}.}
}
\details{
The \code{read.multitable} function is a multiple-table version of \code{\link{read.table}}.  It is largely a wrapper for \code{\link{dlcast}}, so the tables that are read need to produce `long' format data frames.  The implementation of \code{read.multitable} is very simple:  (1) repeatedly call \code{\link{read.table}} to load in the files with names in \code{files} and then (2) call \code{\link{dlcast}} to combine these tables into a data list.  Therefore, the \code{dimids} and \code{fill} arguments are simply passed to \code{\link{dlcast}}.

The \code{read.multicsv} and \code{read.multidelim} simply wrap \code{read.multitable} with the appropriate value for the \code{sep} argument (compare with \code{\link{read.csv}} for example).

The \code{read.fourthcorner} function reads in three files (with names passed to \code{community}, \code{environment}, and \code{traits}), and creates a data list out of the resulting files.  The \code{community} table is coerced to a \code{matrix} so that the rows and columns are treated as two different dimensions of replication, whereas the other two tables are left as data frames.  This function is useful because many community ecologists will store their data in this way.

The \code{multifile.choose} function allows the interactive selection of the names of \code{n} files.  Compare with \code{\link{file.choose}}.
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
dimids <- c("sites","species")
read.multicsv(files,dimids,fill=c(0,NA,NA))

## Modifications necessary to use the read.fourthcorner function
abundance <- data.frame(
	capybara = c(4,10,8,7,0,0),
	moss = c(6,0,9,3,5,0),
	vampire = c(0,1,0,0,0,0),
	row.names = c(
		"arctic","subarctic","midlatitude","subtropical",
		"tropical","equatorial"
	)
)
write.table(abundance,abundance.file,sep=",")
read.fourthcorner(abundance.file,environment.file,trait.file,sep=",")
}