\name{Rsqr.regpath}
\alias{Rsqr.regpath}
\title{R^2 regularization path for ridge RDA}
\usage{
  Rsqr.regpath(Y, X, form, trans = "hellinger",
    lambdas = exp(-seq(0, ncol(X) + 1, length.out = 30)),
    ...)
}
\arguments{
  \item{Y}{Response matrix}

  \item{X}{Design matrix}

  \item{form}{Optional one-sided formula for specifying
  Design matrix}

  \item{trans}{Transformation of the response matrix}

  \item{lambdas}{Vector of regularization parameters
  defining the path}

  \item{...}{Additional parameters}
}
\value{
  A data frame with one row per lambda and columns:
  \item{lambdas}{lambdas} \item{df}{Effective degrees of
  freedom (i.e. trace of the hat matrix)}
  \item{rel.eff.n.params}{Relative effective number of
  parameters} \item{Rsquare}{Unadjusted R^2}
  \item{Rsquare.adj}{Adjusted R^2}
  \item{Rsquare.pred}{Doubly adjusted (i.e. prediction)
  R^2}
}
\description{
  R^2 regularization path for ridge RDA
}

