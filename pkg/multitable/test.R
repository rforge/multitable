library(testthat)

context("data list subscripting")

test_that("repdims are calculated correctly",{
	library(multitable)
	data(fake.community)
	fake.community <- fake.community[-1,,]
	
	expect_that(attr(fake.community,"repdim"),equals(c(5,3,3)))
})

test_that("logical subscripting too long",{
	library(multitable)
	data(fake.community)
	em <- try(fake.community[,rep(TRUE,4),],silent=TRUE)[1]
	
	expect_that(em,equals(
		"Error in eval(expr, envir, enclos) : \n  (subscript) logical subscript too long\n"
	))
})

test_that("long logical subscripting with 1D data lists",{
	library(multitable)
	data(fake.community)
	fake.community <- fake.community[4:6,drop=FALSE]
	fake.community <- fake.community[rep(TRUE,4),vextract=FALSE]
	
	expect_that(attr(fake.community,"repdim"),equals(4))
	expect_that(is.na(fake.community[[1]][[4]]),is_true())
	expect_that(is.na(fake.community[[2]][[4]]),is_true())
	expect_that(is.na(fake.community[[3]][[4]]),is_true())
})

test_that("subscripting with empty character strings and completely empty subscripts",{
	library(multitable)
	data(fake.community)
	em <- try(fake.community[,],silent=TRUE)[1]
	
	expect_that(fake.community["","",""],is_identical_to(fake.community))
	expect_that(fake.community[,"",""],is_identical_to(fake.community))
	expect_that(fake.community["",,""],is_identical_to(fake.community))
	expect_that(fake.community["","",],is_identical_to(fake.community))
	expect_that(fake.community["",,],is_identical_to(fake.community))
	expect_that(fake.community[,"",],is_identical_to(fake.community))
	expect_that(fake.community[,,""],is_identical_to(fake.community))
	expect_that(fake.community[,,],is_identical_to(fake.community))
	expect_that(fake.community[],is_identical_to(fake.community))
	expect_that(em,equals("Error in `[.data.list`(fake.community, , ) : \n  incorrect number of dimensions\n"))
})

test_that("NULL subscripting",{
	library(multitable)
	data(fake.community)
	em <- try(fake.community[NULL,,NULL],silent=TRUE)[1]
	
	expect_that(em,equals("Error in `[.data.list`(fake.community, NULL, , NULL) : \n  NULL subscripting is not allowed in data lists\n"))
})

test_that("matrix subscripting",{
	library(multitable)
	data(fake.community)
	em <- try(fake.community[matrix(c(1,2,3,4,1,2,3,1,1,2,3,1),4,3)],silent=TRUE)[1]
	
	expect_that(em,equals("Error in `[.data.list`(fake.community, matrix(c(1, 2, 3, 4, 1, 2, 3, 1,  : \n  subscripting data lists with matrices is currently not allowed, but this may change in the future\n"))
})

test_that("too many variables extracted",{
	library(multitable)
	data(fake.community)
	em <- try(fake.community[-(1:6)],silent=TRUE)[1]
	
	expect_that(em,equals("Error in match.dnames[[which.max(sapply(match.dnames, length))]] : \n  attempt to select less than one element\n"))
})

test_that("too much extracted",{
	library(multitable)
	data(fake.community)
	em <- try(fake.community[-(1:6),,],silent=TRUE)[1]
	
	expect_that(em,equals("Error in `[.data.list`(fake.community, -(1:6), , ) : \n  some replication dimensions have been reduced to zero length and this is not allowed\n"))
})

test_that("data list of a single vector becomes a data frame",{
	library(multitable)
	a <- runif(10)
	a.dl <- as.data.list(a)
	a.df <- as.data.frame(a)
	
	expect_that(class(a.dl),is_equivalent_to(class(a.df)))
	expect_that(class(a.dl),equals("data.frame"))
	expect_that(unlist(a.dl),is_equivalent_to(unlist(a.df)))
})

test_that("as.data.frame molds",{
	library(multitable)
	data(fake.community)
	fake.mold <- data.list.mold(fake.community)
	
	expect_that(as.data.frame(fake.community),
		equals(as.data.frame(fake.community,mold=fake.mold)))
	
})

library(multitable)
data(fake.community)
fake.community$body.size <- letters[1:3]
fake.community$homeotherm <- letters[1:3]
fake.community$homeotherm <- dimnames(fake.community)[[3]]
fake.community[["body.size"]] <- letters[1:3]
fake.community[["homeotherm"]] <- letters[1:3]
fake.community[["homeotherm"]] <- dimnames(fake.community)[[3]]





fake.community[["temperature"]] <- fake.community[["temperature"]] + 1

dm <- dimnames(fake.community)[[3]]
fake.community[["species",match.dnames="species"]] <- dimnames(fake.community)[[3]]
fake.community[["species",match.dnames]] <- dm