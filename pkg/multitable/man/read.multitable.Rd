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
I'll get to this a little later.
}
\value{
A \code{\link{data.list}} object.
}