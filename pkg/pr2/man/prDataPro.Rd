\name{prDataPro}
\alias{prDataPro}
\title{Process data}
\usage{
  prDataPro(Y, X, form, trans = "hellinger", ...)
}
\arguments{
  \item{Y}{Response matrix}

  \item{X}{Design matrix.}

  \item{form}{Optional one-sided formula for specifying
  Design matrix}

  \item{trans}{Transformation of the response matrix}

  \item{...}{More parameters}
}
\value{
  List with components: \item{Y}{Transformed response
  matrix} \item{X}{Model matrix}
}
\description{
  Function for processing data in \code{pr2}.
}

