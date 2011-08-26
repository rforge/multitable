\name{read.multitable}
\alias{read.multitable}
\title{Multiple table data input}
\description{
Reads several files in long table format and creates a data list from them.
}
\usage{
read.multitable(files, dim.cols, tnames, ..., fill = NULL, drop = TRUE)
}
\arguments{
  \item{files}{A vector with the names of the files containing the tables.}
  \item{dim.cols}{A vector with the names of the columns associated with replication dimensions.}
  \item{tnames}{An optional vector with the names of the tables. If missing the filenames are used after being passed through \code{\link{make.names}}.}
  \item{...}{Additional arguments to pass to \code{\link{read.table}}. Note that headers MUST be present in order to distinguish columns for replication dimensions from columns for variables, and therefore \code{header} is set to \code{TRUE} and cannot be changed.}
  \item{fill}{See \code{\link{acast}}.  Unlike \code{\link{acast}}, \code{read.multitable} can take vector-valued \code{fill} arguments -- one element per file.}
  \item{drop}{See \code{\link{acast}}.}
}
\details{
I'll get to this a little later.
}
\value{
A \code{\link{data.list}} object.
}