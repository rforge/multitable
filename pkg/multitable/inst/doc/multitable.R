### R code from vignette source 'jss813.Rnw'

###################################################
### code chunk number 1: change the prompt symbol
###################################################
options(prompt = "R> ")


###################################################
### code chunk number 2: packages to install
###################################################
### install.packages(c('multitable', 'rbenchmark', 'ggplot2', 'arm', 'vegan','scales'), repos="http://probability.ca/cran")


###################################################
### code chunk number 3: required packages
###################################################
require(multitable)
require(rbenchmark)
require(ggplot2)
require(arm)
require(vegan)
require(scales)


###################################################
### code chunk number 4: the structure of data lists
###################################################
library("multitable")
data("fake.community")
fake.community


###################################################
### code chunk number 5: unimportant
###################################################
options(width=60)


###################################################
### code chunk number 6: the summary function reveals the structure of a data list
###################################################
summary(fake.community)


###################################################
### code chunk number 7: an example matrix-valued variable to include in a new data list
###################################################
abundance <- data.frame(
	sites=c(
		"midlatitude","subtropical","tropical","equatorial",
		"arctic","midlatitude","tropical","equatorial",
		"subtropical"
	),
	species=c(rep("capybara",4),rep("moss",4),"vampire"),
	abundance=c(4,10,8,7,5,6,9,3,1)
)


###################################################
### code chunk number 8: what the abundance matrix looks like
###################################################
abundance


###################################################
### code chunk number 9: data frames of vector-valued variables to include in a new data list
###################################################
environment <- data.frame(
	sites=c(
		"subarctic","midlatitude","subtropical",
		"tropical","equatorial"
	),
	temperature=c(0,10,20,50,30),
	precipitation=c(40,20,100,150,200)
)
trait <- data.frame(
	species=c("capybara","moss","vampire"),
	body.size=c(140,5,190),
	metabolic.rate=c(20,5,0)
)


###################################################
### code chunk number 10: what the environment and traits data frames look like
###################################################
environment
trait


###################################################
### code chunk number 11: unimportant
###################################################
options(width=80)


###################################################
### code chunk number 12: create a new data list
###################################################
l <- list(abundance, environment, trait)
dl <- dlcast(l, fill = c(0, NA, NA))
summary(dl)
dl


###################################################
### code chunk number 13: a wide-format variable to add to a data list
###################################################
allele <- matrix(c(
	NA, 0.4, 0, 0.1, 0, NA,
	0, 0, 0, NA, 0.2, NA,
	NA,NA,NA,0,NA,NA
), 6, 3)
dimnames(allele) <- dimnames(dl)


###################################################
### code chunk number 14: what the wide-format example looks like
###################################################
allele


###################################################
### code chunk number 15: adding a wide-format variable to an existing data list
###################################################
dl.with.allele <- dl + variable(allele, c("sites", "species"))


###################################################
### code chunk number 16: structure of the modified data list
###################################################
summary(dl.with.allele)


###################################################
### code chunk number 17: array-like data list subscripting example
###################################################
fake.community[,c("2008","2009"),]


###################################################
### code chunk number 18: another array-like data list subscripting example (eval = FALSE)
###################################################
## fake.community[1:3, "1537", c(TRUE, FALSE, FALSE)]


###################################################
### code chunk number 19: list-like data list subscripting example
###################################################
fake.community[c("temperature", "precipitation")]


###################################################
### code chunk number 20: if resulting data list has only a single dimension it is dropped
###################################################
fake.community[5:6]


###################################################
### code chunk number 21: this dropping behaviour can be suppressed
###################################################
fake.community[5:6, drop = FALSE]


###################################################
### code chunk number 22: assigning new values to variables in data lists
###################################################
fake.community$precipitation[is.na(fake.community$precipitation)] <- 
  c(30, 5, 50, 75, 50, 2, 7)


###################################################
### code chunk number 23: transforming variables in data lists (eval = FALSE)
###################################################
## fake.community$log.precipitation <- log(fake.community$precipitation)


###################################################
### code chunk number 24: specifying the shape of a transformed variable
###################################################
fake.community[['log.precipitation', shape = 'precipitation']] <- 
  log(fake.community$precipitation)
fake.community['log.precipitation']


###################################################
### code chunk number 25: creating variables to identify replicates
###################################################
fake.community <- dims_to_vars(fake.community)
summary(fake.community)


###################################################
### code chunk number 26: here are some variables that were created from dimensions of replication
###################################################
fake.community['species', drop = FALSE]


###################################################
### code chunk number 27: simplifying the example to illustrate melt-recast techniques
###################################################
data("fake.community")
fake.community <- fake.community[1:3, 1:2, 1:2][1:4]
summary(fake.community)


###################################################
### code chunk number 28: melting a data list
###################################################
dlm <- dlmelt(fake.community)


###################################################
### code chunk number 29: summary of a melted data list
###################################################
summary(dlm)


###################################################
### code chunk number 30: the first two lines of the data frames in the melted data list
###################################################
lapply(dlm, head, n = 2)


###################################################
### code chunk number 31: dlcast is an approximate inverse to dlmelt (eval = FALSE)
###################################################
## dlcast(dlm)


###################################################
### code chunk number 32: manipulating dimensions of replication in melted form
###################################################
dlm$sites.years.species <- within(dlm$sites.years.species, {
  sites.years <- interaction(sites, years, drop = TRUE)
  sites <- years <- NULL
})
dlm$sites.years <- within(dlm$sites.years, {
  sites.years <- interaction(sites, years, drop = TRUE)
  sites <- years <- NULL
})


###################################################
### code chunk number 33: recasting can now be applied to produce a reshaped data list
###################################################
dl <- dlcast(dlm)
summary(dl)


###################################################
### code chunk number 34: unimportant
###################################################
options(width=100)


###################################################
### code chunk number 35: coercing a data list to data frame
###################################################
data("fake.community")
fake.community.df <- as.data.frame(fake.community)


###################################################
### code chunk number 36: looking at the data list in data frame form
###################################################
fake.community.df[,-6]


###################################################
### code chunk number 37: unimportant
###################################################
options(width=70)


###################################################
### code chunk number 38: the molding technique to speed up data list to data frame coercion
###################################################
data("fake.community")
fc.mold <- data.list.mold(fake.community)


###################################################
### code chunk number 39: coercion with a mold (eval = FALSE)
###################################################
## as.data.frame(fake.community, mold = fc.mold)


###################################################
### code chunk number 40: coercion without a mold (eval = FALSE)
###################################################
## as.data.frame(fake.community)


###################################################
### code chunk number 41: functions for benchmarking the molding technique
###################################################
with_molding <- function(){
  fake.community.mold <- data.list.mold(fake.community)
  for(i in 1:100)
    as.data.frame(fake.community, mold = fake.community.mold)
}
without_molding <- function(){
  for(i in 1:100)
    as.data.frame(fake.community)
}


###################################################
### code chunk number 42: benchmarking molding
###################################################
library("rbenchmark")
benchmark(with_molding(), without_molding(),
  replications = 10, 
  columns = c("test", "replications", "relative"))


###################################################
### code chunk number 43: unimportant
###################################################
options(width=70)


###################################################
### code chunk number 44: a real example data list
###################################################
data("higgins")


###################################################
### code chunk number 45: structure of the higgins fish data
###################################################
summary(higgins)
dim(higgins)


###################################################
### code chunk number 46: introducing dlapply (eval = FALSE)
###################################################
## dlapply(higgins, 1, median)


###################################################
### code chunk number 47: introducing dlapply
###################################################
dlapply(higgins, 1, median)


###################################################
### code chunk number 48: unimportant
###################################################
options(width=75)


###################################################
### code chunk number 49: simplifying the results of a dlapply using as.data.frame (eval = FALSE)
###################################################
## as.data.frame(dlapply(higgins, c(2, 3), median))


###################################################
### code chunk number 50: simplifying the results of a dlapply using as.data.frame
###################################################
as.data.frame(dlapply(higgins, c(2, 3), median))


###################################################
### code chunk number 51: dlapply error
###################################################
dlapply(higgins[1:2], 3, quantile)


###################################################
### code chunk number 52: dlapply error (eval = FALSE)
###################################################
## dlapply(higgins[1:2], 3, summary)


###################################################
### code chunk number 53: data lists are also lists
###################################################
is.list(higgins) && is.data.list(higgins)


###################################################
### code chunk number 54: using lapply with data lists
###################################################
lapply(higgins[c('abundance', 'width', 'trophic')], summary)


###################################################
### code chunk number 55: a boxplot from a data list (eval = FALSE)
###################################################
## data("higgins")
## boxplot(
##   formula = sqrt(abundance) ~ species,
##   data = as.data.frame(dims_to_vars(higgins)),
##   horizontal = TRUE, las = 1, xaxt = 'n',
##   xlab = "abundance (square-root scale)", ylab = "species")
## tickmarks <- with(higgins, pretty(sqrt(abundance)))
## axis(1, at = tickmarks, labels = tickmarks^2)


###################################################
### code chunk number 56: boxplot
###################################################
data("higgins")
boxplot(
  sqrt(abundance) ~ species, 
  as.data.frame(dims_to_vars(higgins)), 
  horizontal = TRUE, las = 1, xaxt = 'n',
  xlab = "abundance (square-root scale)", ylab = "species")
tickmarks <- with(higgins, pretty(sqrt(abundance)))
axis(1, at = tickmarks, labels = tickmarks^2)


###################################################
### code chunk number 57: a faceted ggplot scatterplot from a data list
###################################################
data("higgins")
higgins <- dims_to_vars(higgins)
higgins$species <- with(higgins, 
  reorder(species, rank(life.history)))
higgins.df <- as.data.frame(higgins)
library("ggplot2")
library("arm")
p <- ggplot(higgins.df)
p <- p + facet_wrap( ~ species, ncol = 4)
p <- p + geom_point(aes(x = width, y = abundance, shape = life.history))
p <- p + stat_smooth(aes(x = width, y = abundance), se = FALSE, 
    method = 'bayesglm', family = poisson, form = y ~ x + I(x^2),
    colour = 'black', alpha = 0.4, geom = 'line')
p <- p + scale_y_continuous(
  trans = 'sqrt', 
  breaks = trans_breaks('sqrt', function(x) x^2))


###################################################
### code chunk number 58: ggplot
###################################################
print(p)


###################################################
### code chunk number 59: create variables out of higgins dimensions of replication
###################################################
data("higgins")
higgins <- dims_to_vars(higgins)


###################################################
### code chunk number 60: reorder the species dimension according to life history
###################################################
higgins$species <- with(higgins, reorder(species, life.history))


###################################################
### code chunk number 61: ggplot2
###################################################
library("ggplot2")


###################################################
### code chunk number 62: create ggplot object (eval = FALSE)
###################################################
## p <- ggplot(as.data.frame(higgins))


###################################################
### code chunk number 63: add faceting (eval = FALSE)
###################################################
## p <- p + facet_wrap( ~ species, ncol = 4)


###################################################
### code chunk number 64: add the points (eval = FALSE)
###################################################
## p <- p + geom_point(aes(x = width, y = abundance, shape = life.history))


###################################################
### code chunk number 65: fit the smoother (eval = FALSE)
###################################################
## library("arm")
## p <- p + stat_smooth(aes(x = width, y = abundance), se = FALSE, 
##     method = 'bayesglm', family = poisson, form = y ~ x + I(x^2),
##     colour = 'black', alpha = 0.4, geom = 'line')


###################################################
### code chunk number 66: use sqrt y-axis (eval = FALSE)
###################################################
## p <- p + scale_y_continuous(
##   trans = 'sqrt', 
##   breaks = trans_breaks('sqrt', function(x) x^2))


###################################################
### code chunk number 67: plot the graph (eval = FALSE)
###################################################
## print(p)


###################################################
### code chunk number 68: unimportant
###################################################
options(width=80)


###################################################
### code chunk number 69: unimportant
###################################################
options(width=80)


###################################################
### code chunk number 70: formula with hypothesised relationships among variables in higgins data list
###################################################
form <- abundance ~ -1 + life.history + (scale(width):life.history)


###################################################
### code chunk number 71: fit a model to a data list
###################################################
higgins.glm <- glm(
  form, family = poisson, 
  data = as.data.frame(higgins))
printCoefmat(summary(higgins.glm)$coefficients, signif.stars = FALSE)


###################################################
### code chunk number 72: plot observed versus expected graph (eval = FALSE)
###################################################
## higgins[['fitted', shape = 'abundance']] <- 
##   array(fitted.values(higgins.glm), dim(higgins))
## ggplot(as.data.frame(higgins)) + 
##   facet_wrap( ~ species, ncol = 4) + 
##   geom_point(aes(x = fitted, y = abundance)) + 
##   geom_abline(intercept = 0, slope = 1) + 
##   scale_y_continuous(trans = 'sqrt', 
##     breaks = trans_breaks('sqrt', function(x) x^2)) + 
##   scale_x_continuous(trans = 'sqrt', 
##     breaks = trans_breaks('sqrt', function(x) x^2))


###################################################
### code chunk number 73: obsvrsexp
###################################################
higgins[['fitted', shape = 'abundance']] <- 
  array(fitted.values(higgins.glm), dim(higgins))
ggplot(as.data.frame(higgins)) + 
  facet_wrap( ~ species, ncol = 4) + 
  geom_point(aes(x = fitted, y = abundance)) + 
  geom_abline(intercept = 0, slope = 1) + 
  scale_y_continuous(trans = 'sqrt', 
    breaks = trans_breaks('sqrt', function(x) x^2)) + 
  scale_x_continuous(trans = 'sqrt', 
    breaks = trans_breaks('sqrt', function(x) x^2))


###################################################
### code chunk number 74: store coefficients from the unrandomised data
###################################################
coef.obs <- coefficients(higgins.glm)[6:10]


###################################################
### code chunk number 75: number of randomisations
###################################################
B <- 500


###################################################
### code chunk number 76: allocate an array to store randomised coefficients
###################################################
coef.B <- array(0, c(B, length(coef.obs), 2))
dimnames(coef.B) <- 
list(1:B, names(coef.obs), c('species','seasons.rivers'))


###################################################
### code chunk number 77: create a mold for the higgins data list
###################################################
mold.higgins <- data.list.mold(higgins)


###################################################
### code chunk number 78: set the pseudo-random number generator seed for replicability
###################################################
set.seed(1)


###################################################
### code chunk number 79: loop over permutations
###################################################
higgins.tmp <- higgins
for(i in 1:B){
  higgins.tmp$abundance <- higgins$abundance[sample(24),,]
  df.tmp <- as.data.frame(higgins.tmp, mold = mold.higgins)
  coef.B[i, , 1] <- glm(form, family = poisson, df.tmp)$coefficients[6:10]
}


###################################################
### code chunk number 80: loop over permutations
###################################################
higgins.tmp <- higgins
for(i in 1:B){
  higgins.tmp$abundance <- higgins$abundance[,sample(4),sample(3)]
  df.tmp <- as.data.frame(higgins.tmp, mold = mold.higgins)
  coef.B[i, , 2] <- glm(form, family = poisson, df.tmp)$coefficients[6:10]
}


###################################################
### code chunk number 81: take the absolute values of the coefficients
###################################################
coef.B.abs <- abs(coef.B)
coef.obs.abs <- abs(coef.obs)


###################################################
### code chunk number 82: calculate pvalues
###################################################
pvalues <- apply(sweep(coef.B.abs, 2, coef.obs.abs, '>='), c(2,3), mean)
round(cbind(coef.obs, pvalues), 3)


###################################################
### code chunk number 83: melt and recast higgins to collapse dimensions of replication
###################################################
data("higgins")
higgins.melt <- dlmelt(higgins)

higgins.melt$species.seasons.rivers <- 
  within(higgins.melt$species.seasons.rivers, {
    seasons.rivers <- interaction(seasons, rivers, drop = TRUE)
    seasons <- rivers <- NULL
})
higgins.melt$seasons.rivers <- 
  within(higgins.melt$seasons.rivers, 
    seasons.rivers <- interaction(seasons, rivers, drop = TRUE)
)

higgins <- dlcast(higgins.melt)


###################################################
### code chunk number 84: summarise higgins with collapsed dimensions
###################################################
summary(higgins)


###################################################
### code chunk number 85: analysis of dissimilarities
###################################################
library("vegan")
dl.adonis <- with(higgins, adonis(
  t(abundance) ~ width + temp + depth + velocity + substrate + habitat, 
  strata = higgins$species
))
print(dl.adonis$aov.tab, signif.stars = FALSE)


