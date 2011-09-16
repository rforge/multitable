\name{multitable-package}
\alias{multitable-package}
\alias{multitable}
\docType{package}
\title{Simultaneous manipulation of multiple arrays of data, with data.list objects}
\description{
Data frames are integral to \R.  They provide a standard format for passing data to model-fitting and plotting functions.  This standard makes it easier for experienced use\R{s} to learn new functions, because most developments in \R continue to accept a single data frame as input.  Still, many data sets do not easily fit into a single data frame, and my field of community ecology provides many examples of such inherently multiple-table data (e.g. fourth-corner problem and other trait-based data sets).  Storing such data in a single data frame results in either large numbers of meaningless missing values or storage of redundant information.  These storage problems have led ecologists to model summaries of their data (e.g. community-weighted trait matrices), rather then their data itself.  Perhaps more importantly, my experience with manipulating such data using data frames has resulted in difficult-to-read workflows with many lines.  The \code{multitable} package introduces new data storage objects called \code{data.list}s, which are extensions of \code{data.frame}s.  As \code{data.list}s can be coerced to \code{data.frame}s, they can be used with all \R functions that accept an object that is coercible to a \code{data.frame} (e.g. \code{lm}; \code{plot}; \code{lme}; and many more).  The \code{multitable} package also provides several mechanisms for simplifying the manipulation of \code{data.list} objects.}
\details{
\tabular{ll}{
Package: \tab multitable\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2011-07-17\cr
License: \tab GPL-2\cr
LazyLoad: \tab yes\cr
}
}
\author{
Maintainer: Steve Walker <steve.walker@utoronto.ca>
}
\keyword{ package }
