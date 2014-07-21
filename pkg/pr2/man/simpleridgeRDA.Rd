\name{simpleridgeRDA}
\alias{simpleridgeRDA}
\title{Simple ridge RDA}
\usage{
  simpleridgeRDA(lambda, Y, X, ...)
}
\arguments{
  \item{lambda}{Regularization parameter}

  \item{Y}{Response matrix}

  \item{X}{Design matrix}

  \item{...}{Dots}
}
\value{
  A vector with the R^2 and effective degrees of freedom
}
\description{
  Simple redundancy analysis (RDA) using a ridge penalty.
}

