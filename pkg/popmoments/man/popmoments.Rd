\docType{package}
\name{popmoments}
\alias{package-popmoments}
\alias{popmoments}
\alias{popmoments-package}
\title{Population-level moments}
\description{
  Simple functions for calculating the population-level
  moments.
}
\details{
  The expectation (\code{\link{E.}}), variance
  (\code{\link{var.}}), covariance (\code{\link{cov.}}),
  correlation (\code{\link{cor.}}), and regression
  (\code{\link{reg.}}) operators are provided. Note that
  each function ends in a \code{.} to distinguish them from
  other standard functions with similar names. Convenience
  functions for other quantities are also provided (e.g.
  \code{\link{skew.}}, \code{\link{coskew.}},
  \code{\link{cm.}}, \code{\link{int.}}, \code{\link{z.}}).
  All of these functions act on vectors without regard to
  dimension information or any other attributes (including
  S3 class information). Two functions are special in that
  they do not act on vectors: \code{\link{Edist}} and
  \code{\link{cm.}}. \code{\link{Edist}} acts on
  \code{d-functions} (e.g. \code{\link{dnorm}}), to give
  (or approximate) the expected value of a (univariate)
  distribution.  \code{\link{cm.}} acts on data frames to
  give the mixed central moment of the columns of a data
  frame.
}

