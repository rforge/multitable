\name{make.required.dimnames}
\alias{make.required.dimnames}
\title{Make required dimnames}
\usage{
  make.required.dimnames(x, clear.names = FALSE, prefix = "name")
}
\arguments{
  \item{x}{An object that could contain dimnames, probably an array.}
  \item{clear.names}{Should current names be cleared?}
  \item{prefix}{Prefix to be used for the names.}
}
\value{
  An object with full dimnames.
}
\description{
  If dimnames are null then create some generic ones.
}