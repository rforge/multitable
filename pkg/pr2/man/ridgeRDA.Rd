\name{ridgeRDA}
\alias{predict.ridgeRDA}
\alias{print.ridgeRDA}
\alias{ridgeRDA}
\title{Ridge redundancy analysis}
\usage{
  ridgeRDA(lambda, Y, X, form, trans = "hellinger", ...)

  \method{predict}{ridgeRDA} (object, ...)

  \method{print}{ridgeRDA} (x, ...)
}
\arguments{
  \item{lambda}{regularization parameter}

  \item{Y}{Response matrix}

  \item{X}{Design matrix.}

  \item{form}{Optional one-sided formula for specifying
  Design matrix}

  \item{trans}{Transformation of the response matrix}

  \item{...}{More parameters}

  \item{object}{\code{ridgeRDA} object}

  \item{x}{\code{ridgeRDA} object}
}
\value{
  List with components: \item{Y}{Transformed response
  matrix} \item{X}{Model matrix} \item{beta}{Estimated
  coefficients}
}
\description{
  Redundancy analysis with a ridge penalty.
}

