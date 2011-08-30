\name{read.multitable}
\alias{read.multitable}
\alias{read.multicsv}
\alias{read.multidelim}
\alias{read.fourthcorner}
\title{Multiple table data input}
\description{
Reads several files in long table format and creates a data list from them.
}
\usage{
read.multitable(files, dnames, tnames, fill = rep(NA,length(files)), ...)

read.multicsv(files, dnames, tnames, fill = rep(NA,length(files)), ...)

read.multidelim(files, dnames, tnames, fill = rep(NA,length(files)), ...)

read.fourthcorner(Y, X, Z, dnames=c("sites","species"), Yname="abundance", ...)
}
\arguments{
  \item{files}{A vector with the names of the files containing the tables.}
  \item{Y}{A character string of the name of the file containing the community matrix of a fourth-corner problem.}
  \item{X}{A character string of the name of the file containing the environment matrix of a fourth-corner problem.}
  \item{Z}{A character string of the name of the file containing the trait matrix of a fourth-corner problem.}
  \item{dnames}{A vector with the names of the columns associated with replication dimensions.  For \code{read.fourthcorner} this is simply the names of the replication dimensions.}
  \item{tnames}{An optional vector with the names of the tables. If missing the filenames are used after being passed through \code{\link{make.names}}.}
  \item{fill}{See \code{\link{dlcast}}.}
  \item{Yname}{Character string of the name of \code{Y} matrix.}
  \item{...}{Additional arguments to pass to \code{\link{read.table}}. Note that headers MUST be present in order to distinguish columns for replication dimensions from columns for variables, and therefore \code{header} is set to \code{TRUE} and cannot be changed.  Exception:  \code{header} may be missing in \code{read.fourthcorner}.}
}
\details{
I'll get to this a little later.
}
\value{
A \code{\link{data.list}} object.
}