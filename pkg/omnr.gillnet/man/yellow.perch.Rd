\docType{data}
\name{yellow.perch}
\alias{north.pike}
\alias{walleye}
\alias{yellow.perch}
\title{Gillnet data}
\format{Three data frames, each with the following 8 variables.  \describe{
\item{list("MESH")}{mesh size (mm)} \item{list("TLEN")}{total length (mm)}
\item{list("FLEN")}{fork length (mm)} \item{list("RWT")}{a numeric vector}
\item{list("AGE")}{age at capture} \item{list("SEX")}{sex}
\item{list("MAT")}{a numeric vector} \item{list("PRJ_NUM")}{project number} }
The \code{yellow.perch}, \code{walleye}, and \code{north.pike} datasets each
contain 6033, 13534, and 2948 observations (i.e. fish) respectively.}
\source{
  Ontario Ministry of Natural Resources
}
\description{
  Yellow Perch, Walleye, and Northern Pike gillnet data.
}
\examples{
data(yellow.perch)
}
\keyword{datasets}

