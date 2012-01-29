library(testthat)

context("multitable package")

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
	
	expect_that(em,equals("Error in match.dimids[[which.max(sapply(match.dimids, length))]] : \n  attempt to select less than one element\n"))
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

test_that("character valued benchmark variable",{
	library(multitable)
	A <- runif(3)
	B <- matrix(letters[1:15],5,3)
	C <- runif(5)
	dl <- data.list(A,B,C)
	dl2 <- dl[1:3,1:2]
	dl2$B <- dl2$B[,drop = TRUE]
	A <- A[1:2]
	B <- B[1:3,1:2]
	C <- C[1:3]
	dl3 <- data.list(A,B,C)
	expect_that(dl2,equals(dl3))
})

test_that("automatic dimension matching with data.frame inputs",{
	library(multitable)
	A <- matrix(runif(10),5,2)
	B <- data.frame(runif(5),runif(5))
	rownames(A) <- rownames(B) <- letters[1:5]
	colnames(A) <- LETTERS[1:2]
	dl1 <- data.list(A,B,dimids=c("small","large"))
	dl2 <- data.list(A,B,match.dimids=list(c("small","large"),"small"))
	expect_that(dl1,equals(dl2))
})

test_that("two different ways to NULL-out a variable are equivalent",{
	library(multitable)
	data(fake.community)
	dl1 <- dl2 <- fake.community
	dl1[["body.size"]] <- NULL
	dl2$body.size <- NULL
	expect_that(dl1,equals(dl2))
})

test_that("names of the variables themselves and corresponding names of the match.dimids attribute of data lists should match",{
	library(multitable)
	data(fake.community)
	names(fake.community) <- letters[1:6]
	fc.names <- names(fake.community)
	fc.match.dimids.names <- names(attr(fake.community,"match.dimids"))
	expect_that(fc.names,equals(fc.match.dimids.names))
})

test_that("aperm.factor doesn't screw up factor valued variables",{
	library(multitable)
	
	A <- structure(factor(letters[7:16], levels = letters[1:16]), dim = c(5,2))
	B <- aperm(A, c(1,2))
	expect_that(A,equals(B))
	
	A <- structure(factor(letters[7:16]), dim = c(5,2))
	B <- aperm(A, c(1,2))
	expect_that(A,equals(B))
	
	A <- structure(factor(7:16, levels = 1:16), dim = c(5,2))
	B <- aperm(A, c(1,2))
	expect_that(A,equals(B))
	
	A <- structure(factor(7:16, levels = 1:16), dim = c(5,2))
	B <- aperm(aperm(A, c(2,1)),c(2,1))
	expect_that(A,equals(B))
	
	A <- structure(factor(7:16, levels = rank(runif(50))), dim = c(5,2))
	B <- aperm(aperm(A, c(2,1)),c(2,1))
	expect_that(A,equals(B))
	
	A <- structure(factor(2:25, levels = 1:50), dim = c(4,3,2))
	B <- aperm(A, c(1,2,3))
	expect_that(A,equals(B))
	B <- aperm(aperm(A, c(2,3,1)), c(3,1,2))
})

test_that("with.data.list works as expected",{
	library(multitable)
	
	dl <- data.list(A = matrix(runif(10), 5, 2), B = runif(2))
	expect_that(with(dl, A %*% B), equals(dl$A %*% dl$B))
	
	sum1 <- with(dl, A + B, "as.data.frame")
	sum2 <- as.data.frame(dl)$A + as.data.frame(dl)$B
	expect_that(sum1, equals(sum2))
})

test_that("placeholder cases can be removed properly (test due to a reviewer of the JSS manuscript)",{
	library(multitable)
	
	x <- data.frame(
   		samples = paste("Sample", c(1,1,2,2,3,4), sep="."),
		species = c(paste("Species", c(1,1,1,2,3), sep="."), "NONE"),
   		count = c(1,2,10,3,4,0)
   	)
	samp <- data.frame(samples=levels(x$samples), var1=c(1,2,1,2))
	taxa <- data.frame(species=levels(x$species), var2=c("b","a","b","a"))
	rownames(samp) <- samp$samples
	rownames(taxa) <- taxa$species
	dl1 <- dlcast(list(x,samp,taxa), c("samples","species"), fill=c(0,NA,NA), 
		placeholders = c("NONE", "Sample.4"))
	
	x <- data.frame(
	   	samples = paste("Sample", c(1,1,2,2,3), sep="."),
		species = paste("Species", c(1,1,1,2,3), sep="."),
	   	count = c(1,2,10,3,4)
	)
	samp <- data.frame(samples=levels(x$samples), var1=c(1,2,1))
	taxa <- data.frame(species=levels(x$species), var2=c("a","b","a"))
	rownames(samp) <- samp$samples
	rownames(taxa) <- taxa$species
	dl2 <- dlcast(list(x,samp,taxa), c("samples","species"), fill=c(0,NA,NA))
	
	expect_that(dl1, equals(dl2))
})

test_that("dims_to_vars works like [[<-.data.list",{
	library(multitable)
	data(fake.community)
	
	dl1 <- dims_to_vars(fake.community, "years")
	dl2 <- fake.community
	dl2[["years", match.dimids = "years"]] <- dimnames(dl2)[[2]]
	expect_that(dl1, equals(dl2))
	
	dl1 <- dims_to_vars(fake.community)
	dl2[["sites", match.dimids = "sites"]] <- dimnames(dl2)[[1]]
	dl2[["species", match.dimids = "species"]] <- dimnames(dl2)[[3]]
	dl2 <- dl2[c(1:6,8,7,9)] # must re-arrange the variable order
							 # to get exact equivalence
	expect_that(dl1, equals(dl2))	
})

test_that("variable addition works",{
	library(multitable)
	variable(matrix(runif(15), 5, 3), c("n","m"), "A") + 
	variable(letters[1:3], "m", "B") + 
	variable(runif(5), "n", "C") + 
	variable(array(runif(15*4), c(3,5,4)), c("m","n","p"), "D") +
	variableGroup(data.frame(a = runif(5), b = runif(5)), "n") + 
	variableGroup(list(
		c = matrix(runif(20), 4, 5),
		d = matrix(runif(20), 4, 5)
	), c("p","n"))
})

test_that("taxon names can be sorted without mismatch between taxon names and trait values",{
	library(multitable)
	
	n <- 10
	m <- 5
	
	set.seed(1)
	species.names <- paste(
		letters[ceiling(runif(m,0,26))],
		letters[ceiling(runif(m,0,26))],
		"_",
		letters[ceiling(runif(m,0,26))],
		letters[ceiling(runif(m,0,26))],
		sep = ""
	)
	site.names <- paste(
		letters[ceiling(runif(n,0,26))],
		letters[ceiling(runif(n,0,26))],
		letters[ceiling(runif(n,0,26))],
		letters[ceiling(runif(n,0,26))],
		sep = ""
	)
	
	abundance <- matrix(runif(n*m), n, m, dimnames = list(site.names, species.names))
	env <- structure(runif(n), names = site.names)
	trait <- factor(structure(letters[ceiling(runif(m,0,26))], names = species.names), 
		levels = letters[1:26])
	
	dl1 <- 
		variable(abundance, c("sites","species"), "abundance") + 
		variable(env, "sites", "env") +
		variable(trait, "species", "trait")
	dl1 <- dl1[, order(trait)]
	
	abundance <- abundance[, order(trait)]
	trait <- trait[order(trait)]
	dl2 <- 
		variable(abundance, c("sites","species"), "abundance") + 
		variable(env, "sites", "env") +
		variable(trait, "species", "trait")
		
	expect_that(dl1, equals(dl2))
})

test_that("data lists with duplicated dimids should fail to be created",{
	library(multitable)
	em <- try(variable(matrix(1:4,2,2), rep("n",2), "square.matrix"), silent = TRUE)[1]
	
	expect_that(em,equals("Error in as.data.list.default(x, dimids, match.dimids, check = check,  : \n  the dimensions of replication for\neach variable must be different\nfrom each other\n"))

})

test_that("zombie factors in the dimid columns are handled appropriately with dlcast (test due to a reviewer of the JSS manuscript)",{
	library(multitable)
	
	x <- data.frame(
		samples = paste("Sample", c(1,1,2,2,3,4), sep="."),
		species = c(paste("Species", c(1,1,1,2,3), sep="."), "NONE"),
		count = c(1,2,10,3,4,0))
	samp <- data.frame(samples=levels(x$sample), var1=1:2)
	taxa <- data.frame(species=levels(x$species), var2=c("b","a"))
	rownames(samp) <- samp$samples
	rownames(taxa) <- taxa$species
	
	levels(x$species) <- c(levels(x$species), "sp.90","sp.91")
	dl <- dlcast(list(x,samp,taxa), c("samples","species"), fill=c(0,NA,NA), placeholders = "NONE")
	expect_that(levels(x$species)[-1], equals(dimnames(dl)[[2]]))

	# make sure that it also works when dimid columns are not factors
	x <- data.frame(
		samples = c(1,1,2,2,3,4),
		species = c(paste("Species", c(1,1,1,2,3), sep="."), "NONE"),
		count = c(1,2,10,3,4,0))
	samp <- data.frame(samples=1:4, var1=1:2)
	taxa <- data.frame(species=levels(x$species), var2=c("b","a"))
	rownames(samp) <- 1:4
	rownames(taxa) <- taxa$species

	levels(x$species) <- c(levels(x$species), "sp.90","sp.91")
	dlcast(list(x,samp,taxa), c("samples","species"), fill=c(0,NA,NA), placeholders = "NONE")
	expect_that(levels(x$species)[-1], equals(dimnames(dl)[[2]]))

})

test_that("variables created with variable are named correctly",{
	library(multitable)

	A <- 1:2
	dl <- variable(A, "sites")
	
	expect_that(names(dl), equals("A"))
})
