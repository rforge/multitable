\name{multitable-package}
\alias{multitable-package}
\alias{multitable}
\docType{package}
\title{Simultaneous manipulation of multiple arrays of data, with data.list objects}
\description{
Many data sets do not easily fit into a single data frame.  Storing such data in a single data frame often results in either large numbers of meaningless missing values or storage of redundant information; yet storing them in multiple data frames often results in long difficult-to-read workflows.  The multitable package introduces a new data object called a \code{\link{data.list}}, which organises several data tables as a single \R object.  The primary goal of \pkg{multitable} is to provide a more intuitive framework for manipulating multiple-table data in \R.  As data.lists can be coerced to data.frames, they can be used with all \R functions that accept an object that is coercible to a data.frame (e.g. \code{\link{lm}}; \code{\link{plot}}; \code{lme}; and many more).}
\details{
\tabular{ll}{
Package: \tab multitable\cr
Type: \tab Package\cr
Version: \tab 1.3\cr
Date: \tab 2011-11-15\cr
Suggests: \tab MASS, lattice, testthat\cr
License: \tab GPL-2\cr
URL: \tab http://multitable.r-forge.r-project.org/\cr
LazyLoad: \tab yes\cr
}
}
\author{
Maintainer: Steve Walker <steve.walker@utoronto.ca>
}
\keyword{ package }
\examples{
######################################################
# The following examples appear in the package
# vignette, and the sections below correspond to
# that document
######################################################
vignette("multitable")

######################################################
# SECTION 2: The structure of data lists
######################################################

# load the example data set in data list form.
data("fake.community")
fake.community

# print a summary of the relational structure
# of the data set.
summary(fake.community)

######################################################
# SECTION 3: Subscripting data lists
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
# SECTION 4: Transforming variables in data lists
######################################################

# transformation occurs much like it would with
# data frames.
fake.community$abundance <- log1p(fake.community$abundance)
fake.community$temperature[, "1537"] <- c(5, 10, 30, 20, -80, -10)	
fake.community$precipitation[, "1537"] <- c(5, 50, 75, 50, 2, 7)
fake.community$body.size["moss"] <- 1
fake.community

######################################################
# SECTION 5: Simple analysis functions
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
# SECTION 6: Coercing data lists to data frames
######################################################

# data frames are easily coerced to data frames via
# the as.data.frame method for data.list objects.
fake.community.df <- as.data.frame(fake.community)
fake.community.df[, -6]

# therefore, data list objects can easily be passed to
# any R function accepting a data frame, after they
# have been converted to a data frame.
library("lattice")
xyplot(abundance ~ temperature | body.size, data = fake.community.df)

# for further information about coercing in multitable:
?as.data.list

######################################################
# SECTION 7: How data lists are made
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
dl <- dlcast(list(abundance,environment,trait),
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