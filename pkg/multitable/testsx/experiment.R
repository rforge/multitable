library(reshape2)
library(testthat)
library(multitable)

### make.match.dimids testing function for tests on
### a simple simple simple 4th corner problem
mmdtf <- function(
	n = 3, m = 2,
	YrownamesQ = TRUE,
	YcolnamesQ = TRUE,
	xnamesQ = TRUE,
	znamesQ = TRUE,
	D1D2distinguishable = TRUE,
	D1consistent = TRUE,
	D2consistent = TRUE,
	ord = 'Yxz'
){
	set.seed(1)
	Y <- matrix(rnorm(n*m), n, m)
	x <- rnorm(n)
	z <- rnorm(m)


	if(D1D2distinguishable){	
		rownames(Y) <- names(x) <- paste('x', 1:n, sep = '')
		colnames(Y) <- names(z) <- paste('z', 1:m, sep = '')
	}
	else{
		rownames(Y) <- names(x) <- letters[1:n]
		colnames(Y) <- names(z) <- letters[1:m]
	}
	
	if(!D1consistent){
		rownames(Y) <- paste('Yx', 1:n, sep = '')
	}

	if(!D2consistent){
		colnames(Y) <- paste('Yz', 1:m, sep = '')
	}
	
	if(!YrownamesQ) rownames(Y) <- NULL
	if(!YcolnamesQ) colnames(Y) <- NULL
	if(!xnamesQ) names(x) <- NULL
	if(!znamesQ) names(z) <- NULL
	
	if(ord == 'Yxz') outcome <- try(data.list(Y, x, z), silent = TRUE)
	if(ord == 'Yzx') outcome <- try(data.list(Y, z, x), silent = TRUE)
	if(ord == 'xYz') outcome <- try(data.list(x, Y, z), silent = TRUE)
	if(ord == 'zYx') outcome <- try(data.list(z, Y, x), silent = TRUE)
	if(ord == 'xzY') outcome <- try(data.list(x, Y, z), silent = TRUE)
	if(ord == 'zxY') outcome <- try(data.list(z, Y, x), silent = TRUE)
	if(!inherits(outcome, 'try-error')){
		if(is.4th.corner(outcome)) return('everything is fine!')
		else return('non-4th corner')
	}
	return(outcome)
}

experimental.design <- expand.grid(
	n = c(2, 3),
	YrownamesQ = c(TRUE, FALSE),
	YcolnamesQ = c(TRUE, FALSE), 
	xnamesQ = c(TRUE, FALSE), 
	znamesQ = c(TRUE, FALSE), 
	D1D2distinguishable = c(TRUE, FALSE), 
	D1consistent = c(TRUE, FALSE), 
	D2consistent = c(TRUE, FALSE),
	ord = c('Yxz','Yzx','xYz','zYx','xzY','zxY')
)

n.treat <- nrow(experimental.design)
outcomes <- numeric(n.treat)
for(i in 1:n.treat){
	outcomes[i] <- do.call(mmdtf, as.list(experimental.design[i,]))
}
success <- outcomes == 'everything is fine!'

success.design <- cbind(experimental.design, success, outcomes)

############################
## order shouldn't matter

# other factors by ord matrix:
of.by.o <- acast(success.design, n + YrownamesQ + YcolnamesQ + xnamesQ + znamesQ + D1D2distinguishable + D1consistent + D2consistent ~ ord, value.var = 'success')

dup.cols.o <- !duplicated(of.by.o, MARGIN = 2)
tst1 <- sum(dup.cols.o) == 1L

## if test passes so omit redundancy
if(tst1){
	success.design <- with(success.design, success.design[
		ord == colnames(of.by.o)[dup.cols.o],
		!(names(success.design) == 'ord')])
}

test_that('order does not matter', expect_that(tst1, is_true()))

#############################

#############################
## w.r.t. naming, all we should need is fully-named vrs not:

# other factors by Y names matrix:
of.by.n <- acast(success.design, n + D1D2distinguishable + D1consistent + D2consistent ~ xnamesQ + znamesQ + YrownamesQ + YcolnamesQ, value.var = 'success')

dup.cols.n <- !duplicated(of.by.n, MARGIN = 2)
tst2 <- sum(dup.cols.n) == 2L

allnamed <- apply(success.design[,c('xnamesQ', 'znamesQ', 'YrownamesQ', 'YcolnamesQ')], 1, all)
partnamed <- apply(!success.design[,c('xnamesQ', 'znamesQ', 'YrownamesQ', 'YcolnamesQ')], 1, all)

## if test passes so omit redundancy
if(tst2){
	success.design <- success.design[allnamed | partnamed, -(3:5)]
	colnames(success.design)[2] <- 'allnamed'
}

test_that('only two naming categories required', expect_that(tst2, is_true()))

##################

##########################
## create tables of results
errmgs <- acast(success.design, n + allnamed ~ D1D2distinguishable + D1consistent + D2consistent, value.var = 'outcomes')

suctab <- acast(success.design, n + allnamed ~ D1D2distinguishable + D1consistent + D2consistent, value.var = 'success')

## expected suctab
suctab_comp <- matrix(c(
	rep(FALSE, 8),
	c(rep(FALSE, 7), TRUE),
	rep(TRUE, 8),
	rep(c(FALSE, rep(TRUE, 3)), 2)
), 4, 8, byrow = TRUE)
tst3 <- all(suctab == suctab_comp)

test_that('we get errors where we expect', expect_that(tst3, is_true()))

#write.csv(apply(errmgs, c(1, 2), as.character),
#	file = 'throw.csv')
##########################
