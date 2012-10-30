\name{simple.scale}
\alias{simple.scale}
\title{(Simple) Scaling and Centering}
\description{Simpler version of the \code{\link{scale}} function}
\usage{
simple.scale(x, center = TRUE, scale = TRUE, simplify = TRUE)
}
\arguments{
  \item{x}{an R object with numeric data}
  \item{center}{should mean of \code{x} be subtracted from \code{x}?}
  \item{scale}{should \code{x} (centered or otherwise) be divided by its (population) standard deviation?}
  \item{simplify}{should \code{x} be first passed through \code{\link{simplify2array}}?}
}
\details{
\code{simple.scale} simplifies \code{\link{scale}} in three ways:
\itemize{
	\item{Treats matrix-like objects as a single variable, rather than several variables (i.e. one per column as in \code{\link{scale}}).}
	\item{Uses the population version of the standard deviation, rather than the sample version.}
	\item{Preserves dimensions of arrays of any number of dimensions, rather than collapse to a single column matrix for arrays with three or more dimensions.}
	\item{Doesn't bother to check if \code{x} is numeric.}
}
For further information, just type \code{simple.scale} to see the source (it really is a very simple function).
}
\value{
A scaled version of \code{x} (possibly simplified to an array, if \code{simplify = TRUE}).
}
\seealso{\code{\link{scale}}}
