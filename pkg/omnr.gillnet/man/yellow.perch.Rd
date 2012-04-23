\name{yellow.perch}
\alias{yellow.perch}
\alias{walleye}
\alias{north.pike}
\docType{data}
\title{
Gillnet data
}
\description{
Yellow Perch, Walleye, and Northern Pike gillnet data.
}
\usage{
data(yellow.perch)
data(walleye)
data(north.pike)
}
\format{
  Three data frames, each with the following 8 variables.
  \describe{
    \item{\code{MESH}}{mesh size (mm)}
    \item{\code{TLEN}}{total length (mm)}
    \item{\code{FLEN}}{fork length (mm)}
    \item{\code{RWT}}{a numeric vector}
    \item{\code{AGE}}{age at capture}
    \item{\code{SEX}}{sex}
    \item{\code{MAT}}{a numeric vector}
    \item{\code{PRJ_NUM}}{project number}
  }
  The \code{yellow.perch}, \code{walleye}, and \code{north.pike} datasets each contain 6033, 13534, and 2948 observations (i.e. fish) respectively.
}

\source{
Ontario Ministry of Natural Resources}
\examples{
data(yellow.perch)
}
\keyword{datasets}
