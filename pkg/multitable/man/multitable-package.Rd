\name{multitable-package}
\alias{multitable-package}
\alias{multitable}
\docType{package}
\title{Simultaneous manipulation of multiple arrays of data, with data.list objects}
\description{
Data frames are integral to \code{R}.  They provide a standard format for passing data to model-fitting and plotting functions, and this standard makes it easier for experienced users to learn new functions that accept data as a single data frame.  Still, many data sets do not easily fit into a single data frame; data sets in ecology with a so-called fourth-corner problem provide important examples.  Manipulating such inherently multiple-table data using several data frames can result in long and difficult-to-read workflows.  We introduce the \code{R} \pkg{multitable} package to provide new data storage objects called \code{data.list} objects, which extend the \code{data.frame} concept to explicitly multiple-table settings.  Like data frames, data lists are lists of variables stored as vectors; what is new is that these vectors have dimension attributes that make accessing and manipulating them easier.  As \code{data.list} objects can be coerced to \code{data.frame} objects, they can be used with all \code{R} functions that accept an object that is coercible to a \code{data.frame}.	
}
\details{
\tabular{ll}{
Package: \tab multitable\cr
Type: \tab Package\cr
Version: \tab 1.5\cr
Date: \tab 2012-11-09\cr
Suggests: \tab MASS, lattice, testthat, arm, ggplot2, rbenchmark, scales, vegan\cr
License: \tab GPL-2\cr
URL: \tab \url{http://multitable.r-forge.r-project.org/}\cr
LazyLoad: \tab yes\cr
}
}
\note{
Using \pkg{multitable} DOES NOT REQUIRE ANY OTHER PACKAGES, other than those that typically come with \code{R}.  The suggested packages are only for examples in the help files and vignette, and for package development (i.e. \pkg{testthat}).
}
\author{
Maintainer: Steve Walker <steve.walker@utoronto.ca>
}
\references{
Steven C Walker, Guillaume Guenard, Peter Solymos, Pierre Legendre (2012). Multiple-Table Data in R with the multitable Package. Journal of Statistical Software, 51(8), 1-38. URL \url{http://www.jstatsoft.org/v51/i08/}
}
\keyword{ package }
\examples{
######################################################
# The package vignette (Walker et al. 2012 JSS) is a
# useful place to start
######################################################
vignette("multitable")


######################################################
# The structure of data lists
######################################################

# load the example data set in data list form.
data(fake.community)
fake.community

# print a summary of the relational structure
# of the data set.
summary(fake.community)

######################################################
# Subscripting data lists
######################################################

# extract two years of data.
fake.community[, c("2008", "2009"), ]

# extraction using both numerical and character
# vectors.
fake.community[1:3, "1537", 1]

# subscripting data lists is designed to be as
# intuitive as possible to R users.  above are
# the examples covered in the manuscript, but
# see the help file for more examples and 
# explanation.
?`[.data.list`

######################################################
# Transforming variables in data lists
######################################################

# transformation occurs much like it would with
# data frames.
fake.community$abundance <- log1p(fake.community$abundance)
fake.community$temperature[, "1537"] <- c(5, 10, 30, 20, -80, -10)	
fake.community$precipitation[, "1537"] <- c(5, 50, 75, 50, 2, 7)
fake.community$body.size["moss"] <- 1
fake.community

######################################################
# Simple analysis functions
######################################################

# we can pass data lists to lm just as we would pass
# data frames.
lm(abundance ~ body.size*temperature, data = fake.community)
lm(abundance ~ homeotherm*temperature, data = fake.community)

# this works for any function that tries to coerce 
# data to a data frame, such as the robust linear
# model function from MASS.
library("MASS")
rlm(abundance ~ body.size*temperature, data = fake.community)

######################################################
# Coercing data lists to data frames
######################################################

# data frames are easily coerced to data frames via
# the as.data.frame method for data.list objects.
fake.community.df <- as.data.frame(fake.community)
fake.community.df[, -6]

# therefore, data list objects can easily be passed to
# any R function accepting a data frame, after they
# have been converted to a data frame.
library(lattice)
xyplot(abundance ~ temperature | body.size, data = fake.community.df)

# for further information about coercing in multitable:
?as.data.list

######################################################
# How data lists are made
######################################################

# here are three example objects to be combined into
# a data list.
abundance <- data.frame(
	sites=c(
		"midlatitude", "subtropical", "tropical", "equatorial",
		"arctic", "midlatitude", "tropical", "equatorial",
		"subtropical"
	),
	species=c(rep("capybara", 4),rep("moss", 4), "vampire"),
	abundance=c(4, 10, 8, 7, 5, 6, 9, 3, 1)
)
environment <- data.frame(
	sites=c(
		"subarctic", "midlatitude", "subtropical",
		"tropical", "equatorial"
	),
	temperature=c(0, 10, 20, 50, 30),
	precipitation=c(40, 20, 100, 150, 200)
)
trait <- data.frame(
	species=c("capybara", "moss", "vampire"),
	body.size=c(140, 5, 190),
	metabolic.rate=c(20, 5, 0)
)
abundance
environment
trait

# we use the dlcast function to combine them.
# the dimids argument tells dlcast what dimensions
# (or columns as they are in 'long' format) are 
# shared among tables.  the fill argument tells 
# dlcast how to fill in any structural missing 
# values.
dl <- dlcast(list(abundance, environment, trait),
	dimids=c("sites", "species"),
	fill=c(0, NA, NA)
)
dl

# for other ways to create data list objects, see:
?data.list
?as.data.list
?read.multitable
?variable
}