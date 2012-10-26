rm(list = ls())

#install.packages(c('multitable', 'nlme', 'ggplot2', 'scales'))

library(multitable)
library(nlme)
library(ggplot2)
library(scales)

#########################
# LOAD DATA:
# 
# 
#########################

files <- paste("Croche",c("abundance","environment","timescales","traits"),"LONGFORMAT.csv",sep="_")
setwd("/users/stevenwalker/documents/manuscripts/bilinear/data/beatrix/")
croche <- read.multicsv(files,c("time","basin","taxon"))

#########################
# MANIPULATE DATA:
# 
# 
#########################

# reorder replication dimensions to 'time-basin-taxon'
croche <- aperm(croche,c(2,1,3))

# drop the last two basins and the 2007 data
croche <- dropdl(croche[7:16,1,])

# keep only these variables
croche <- croche[c("abundance","week","Length","Predator.protection.")]

# create taxon variables
croche[["taxon", match.dimids = "taxon"]] <- c(
	"Bosmina","Cal cope","Cal adults",
	"Cycl cope","Cycl adults","Daphnia cat",
	"Daphnia l&d","Holopedium","nauplii",
	"armoured rot","colonial rot","unprotected rot"
)

# sort the data alphabetically by taxon name
croche <- croche[, order(croche$taxon)]

# create a data frame to maniuplate for the appendix illlustration of data structure
croche0 <- croche
names(croche0) <- c('den', 'day', 'size', 'pp', 'taxon')
df <- as.data.frame(croche0)
rownames(df) <- NULL
head(df, n = 12L)
tail(df, n = 2L)

# standardize Thermocline.Depth and Length and week
croche[["scaled.week", shape = "week"]] <- as.vector(scale(croche$week))
croche[["scaled.Length", shape = "Length"]] <- as.vector(scale(croche$Length))

# create a variable combining taxon and length information
croche[["taxonlength", shape = "taxon"]] <- 
	paste("(",sprintf("%.2f",croche$Length),") ", croche$taxon,sep="")

# create a variable combining taxon, length, and predator protection information
croche[["taxonlengthprotect", shape = "taxon"]] <- 
	paste("(", croche$Predator.protection. ,") ", "(",sprintf("%.2f",croche$Length),") ", croche$taxon,sep="")

# NOT USED ANYMORE
croche[["rotarm", shape = "taxon"]] <- as.factor(dimnames(croche)[[2]] == "Rotif.Armoured")

# relative abundance
croche[["rel.abundance", shape = "abundance"]] <- 
	sweep(croche$abundance, 1 , apply(croche$abundance, 1, sum), "/")

# commuinity-weighted scaled length
croche[["cwm", match.dimids = "time"]] <- 
	as.vector(croche$rel.abundance %*% croche$scaled.Length)

# squareroot transformation
croche[["sqrt.abundance", shape="abundance"]] <- sqrt(croche$abundance)


#########################
# SPECIAL FUNCTIONS:
# for processing model results
# 
#########################

fm.dl <- function(fm, model.name){
	n <- dim(croche)[1]
	m <- dim(croche)[2]
	if(class(fm) == "lme") res <- matrix(resid(fm, type = "n"),n,m)
	else res <- matrix(resid(fm),n,m)
	data.list(
		mod = matrix(model.name,n,m),
		obs = croche$sqrt.abundance,
		fit = matrix(fitted(fm),n,m),
		res = res, 
		tax = paste("(",sprintf("%.2f",croche$Length),") ",
			croche$taxon,sep=""),
		len = croche$Length,
		match.dimids = list(c("n","m"),c("n","m"),c("n","m"),c("n","m"),"m","m")
	)
}
fm.df <- function(...) as.data.frame(fm.dl(...))


#########################
# RANDOM EFFECTS MODELS:
# this is the real meat
# 
#########################

setwd("/users/stevenwalker/documents/multitable/multitable/writing/")

croche.lme0.ML <- lme(sqrt.abundance ~ -1 + scaled.Length + I(scaled.Length^2), 
	data = as.data.frame(croche),
	random = ~ 1 | taxon,
	weights = varIdent(form = ~ 1 | taxon),
	method = "ML")

croche.lme.ML <- lme(sqrt.abundance ~ -1 + scaled.Length + I(scaled.Length^2),
	data = as.data.frame(croche),
	random = ~ scaled.week + I(scaled.week^2) | taxon,
	weights = varIdent(form = ~ 1 | taxon),
	method = "ML")

croche.lme2.ML <- update(croche.lme.ML, fixed. = . ~ . + scaled.Length:scaled.week)

croche.lme3.ML <- update(croche.lme.ML, fixed. = . ~ . + Predator.protection.:scaled.Length:scaled.week)

croche.lme4.ML <- lme(sqrt.abundance ~ -1 + scaled.Length + I(scaled.Length^2),
	data = as.data.frame(croche),
	random = ~ scaled.week + I(scaled.week^2) | taxon,
	method = "ML")

anova(croche.lme.ML, croche.lme2.ML) # p = 0.1594 -- no interaction effect
anova(croche.lme2.ML, croche.lme3.ML)
anova(croche.lme0.ML, croche.lme.ML)
anova(croche.lme0.ML, croche.lme2.ML)

croche.lm <- lm(sqrt.abundance ~ -1 + scaled.Length + I(scaled.Length^2), croche)

croche.lme0 <- lme(sqrt.abundance ~ -1 + scaled.Length + I(scaled.Length^2), 
	data = as.data.frame(croche),
	random = ~ 1 | taxon,
	weights = varIdent(form = ~ 1 | taxon),
	method = "REML")

croche.lme <- lme(sqrt.abundance ~ -1 + scaled.Length + I(scaled.Length^2),
	data = as.data.frame(croche),
	random = ~ scaled.week + I(scaled.week^2) | taxon,
	weights = varIdent(form = ~ 1 | taxon),
	method = "REML")

croche.lme2 <- update(croche.lme, fixed. = . ~ . + scaled.Length:scaled.week)

croche.lme3 <- update(croche.lme, fixed. = . ~ . + Predator.protection.:scaled.Length:scaled.week)


########COVARIANCE MATRIX INTERLUDE###########
diag(with(croche, cbind(1, scaled.week, scaled.week^2)) %*% getVarCov(croche.lme) %*% with(croche, rbind(1, scaled.week, scaled.week^2)))
##############################################

# add fitted values to the croche data
croche[["fitted.lme", shape = "abundance"]] <- 
	structure(croche.lme$fitted[,2], dim = c(10,12), names = NULL, label = NULL)
croche[["fitted.lme.fix", shape = "abundance"]] <- 
	structure(croche.lme$fitted[,1], dim = c(10,12), names = NULL, label = NULL)
croche[["fitted.lme2", shape = "abundance"]] <- 
	structure(croche.lme2$fitted[,2], dim = c(10,12), names = NULL, label = NULL)
croche[["fitted.lme2.fix", shape = "abundance"]] <- 
	structure(croche.lme2$fitted[,1], dim = c(10,12), names = NULL, label = NULL)
croche[["fitted.lme3", shape = "abundance"]] <- 
	structure(croche.lme3$fitted[,2], dim = c(10,12), names = NULL, label = NULL)
croche[["fitted.lme3.fix", shape = "abundance"]] <- 
	structure(croche.lme3$fitted[,1], dim = c(10,12), names = NULL, label = NULL)

croche[["fitted.lme.ML", shape = "abundance"]] <- 
	structure(croche.lme.ML$fitted[,2], dim = c(10,12), names = NULL, label = NULL)
croche[["fitted.lme.fix.ML", shape = "abundance"]] <- 
	structure(croche.lme.ML$fitted[,1], dim = c(10,12), names = NULL, label = NULL)
croche[["fitted.lme2.ML", shape = "abundance"]] <- 
	structure(croche.lme2.ML$fitted[,2], dim = c(10,12), names = NULL, label = NULL)
croche[["fitted.lme2.fix.ML", shape = "abundance"]] <- 
	structure(croche.lme2.ML$fitted[,1], dim = c(10,12), names = NULL, label = NULL)
croche[["fitted.lme3.ML", shape = "abundance"]] <- 
	structure(croche.lme3.ML$fitted[,2], dim = c(10,12), names = NULL, label = NULL)
croche[["fitted.lme3.fix.ML", shape = "abundance"]] <- 
	structure(croche.lme3.ML$fitted[,1], dim = c(10,12), names = NULL, label = NULL)

#################
# RANDOM EFFECTS MODEL PLOTS
#
#################

# organise fitted values for plotting
cdf <- as.data.frame(croche)
refdf <- rbind(
	data.frame(mdl = 1, typ = "cond", 
		week = cdf$week, 
		fit = cdf$fitted.lme,
		taxonlength = cdf$taxonlength),
	data.frame(mdl = 1, typ = "marg", 
		week = cdf$week,
		fit = cdf$fitted.lme.fix,
		taxonlength = cdf$taxonlength),
	data.frame(mdl = 0.5, typ = "cond", 
		week = cdf$week, 
		fit = cdf$fitted.lme2,
		taxonlength = cdf$taxonlength),
	data.frame(mdl = 0.5, typ = "marg", 
		week = cdf$week,
		fit = cdf$fitted.lme2.fix,
		taxonlength = cdf$taxonlength)
)
# plot them
ggplot(as.data.frame(croche)) + 
	facet_wrap( ~ taxonlength, ncol = 3) + 
	geom_point(aes(week, I(sqrt.abundance^2), group = taxon, size = Length),
		alpha = 0.4) + 
	geom_line(aes(week, I(fit^2), group = interaction(mdl, typ), 
		linetype = typ, colour = as.factor(mdl)), 
		stat = 'smooth', data = refdf, se = FALSE) + 
	scale_y_continuous("Density", trans = 'sqrt', 
		breaks = c(0.00, 0.01, 0.04)) + 
	scale_x_continuous("Day-of-year", breaks = c(200, 240, 280)) + 
	scale_size('Size (mm)', range = c(2, 5), 
		breaks = c(0.2, 0.7, 1.2)) + 
	scale_linetype('Prediction type', breaks = c('cond','marg'),
		guide = guide_legend(nrow = 2),
		labels = c(
			'with taxon effects (i.e. conditional)', 
			'without taxon effects (i.e. marginal))')) +
	scale_colour_manual('Predictive model', 
		breaks = as.factor(c(0.5, 1)), 
		values = c('red','black'),
		guide = guide_legend(nrow = 2),
		labels = c(
			'with size-day interaction', 
			'without size-day interaction')) + 
	opts(legend.position = 'top', legend.direction = 'vertical', 
		legend.box = 'horizontal')
ggsave("randomeffectsfit.pdf", height = 8, width = 6.5)


# organise fitted values for plotting
cdf <- as.data.frame(croche)
refdf <- rbind(
	data.frame(mdl = 1, typ = "cond", 
		week = cdf$week, 
		fit = cdf$fitted.lme2,
		taxonlengthprotect = cdf$taxonlengthprotect),
	data.frame(mdl = 1, typ = "marg", 
		week = cdf$week,
		fit = cdf$fitted.lme2.fix,
		taxonlengthprotect = cdf$taxonlengthprotect),
	data.frame(mdl = 0.5, typ = "cond", 
		week = cdf$week, 
		fit = cdf$fitted.lme3,
		taxonlengthprotect = cdf$taxonlengthprotect),
	data.frame(mdl = 0.5, typ = "marg", 
		week = cdf$week,
		fit = cdf$fitted.lme3.fix,
		taxonlengthprotect = cdf$taxonlengthprotect)
)
# plot them
ggplot(as.data.frame(croche)) + 
	facet_wrap( ~ taxonlengthprotect, ncol = 4) + 
	geom_point(aes(week, I(sqrt.abundance^2), group = taxonlengthprotect)) + 
	geom_line(aes(week, I(fit^2), group = interaction(mdl, typ), 
		linetype = typ, colour = as.factor(mdl)), 
		stat = 'smooth', data = refdf, se = FALSE) + 
	scale_y_continuous("Density", trans = 'sqrt', breaks = c(0.00, 0.01, 0.04)) + 
	scale_x_continuous("Day-of-year", breaks = c(200, 240, 280)) + 
	scale_linetype('Prediction type', breaks = c('cond','marg'),
		guide = guide_legend(nrow = 2),
		labels = c(
			'with taxon effects (i.e. conditional)', 
			'without taxon effects (i.e. marginal))')) +
	scale_colour_manual('Predictive model', breaks = as.factor(c(0.5, 1)), values = c('red','black'),
		guide = guide_legend(nrow = 2),
		labels = c(
			'with interactions', 
			'without interactions')) + 
	opts(legend.position = 'top', legend.direction = 'vertical', legend.box = 'horizontal')
ggsave("randomeffectsfitwithpredatorprotection.pdf", height = 8, width = 7.5)

# residual plot
clme <- fm.df(croche.lme, "lme")
ggplot(clme) +
	facet_wrap( ~ tax, ncol = 3) +
	geom_abline(slope = 0, intercept = 0, alpha = 0.5) +
	geom_point(aes(fit, res)) +
	scale_x_continuous("Fitted values") +
	scale_y_continuous("Normalised residuals", breaks = c(-1, 0, 1, 2))
ggsave("randomeffectsresiduals.pdf", height = 7, width = 6.5)

#########################
# FIXED EFFECTS MODELS:
# 
# 
#########################

lmX <- lm(sqrt.abundance ~ scaled.week, croche)
lmZ <- lm(sqrt.abundance ~ scaled.Length, croche)
lmT <- lm(sqrt.abundance ~ taxon, croche)
lmXbyZ2 <- lm(sqrt.abundance ~ scaled.week * (scaled.Length + I(scaled.Length^2)), croche)
lmZ2 <- lm(sqrt.abundance ~ scaled.Length + I(scaled.Length^2), croche)
lmX2byT <- lm(sqrt.abundance ~ -1 + taxon + (scaled.week + I(scaled.week^2)):taxon, croche)

# organise fitted values and residuals for plotting
fms.df <- rbind(
	fm.df(lmX,"den ~ day"),
	fm.df(lmZ,"den ~ size"),
	fm.df(lmZ2,"den ~ size + size^2"),
	fm.df(lmXbyZ2,"den ~ day * (size + size^2)"),
	fm.df(lmT,"den ~ taxon"),
	fm.df(lmX2byT,"den ~ taxon * (day + day^2)")
)
# plot them
ggplot(fms.df) +
	facet_wrap( ~ mod, ncol = 2) + 
	geom_abline(slope = 0, intercept = 0, alpha = 0.5) +
	geom_point(aes(fit, res, shape = tax, colour = tax), alpha = 0.6) +
	scale_x_continuous("Fitted values", breaks = c(0.05, 0.15, 0.25)) +
	scale_y_continuous("Residuals", breaks = c(0, 0.1, 0.2)) + 
	scale_shape_manual("Taxon (length, mm)", values = 1:12) + 
	scale_colour_discrete("Taxon (length, mm)") + 
	opts(legend.position = 'top', legend.direction = 'vertical', legend.box = 'horizontal') + 
	guides(col = guide_legend(nrow = 4), shape = guide_legend(nrow = 4))
ggsave("fixedeffectsresidualsBYtaxa.pdf", height = 7, width = 5.2)
# plot them in a different way
ggplot(fms.df) + 
	facet_wrap( ~ tax, ncol = 3) + 
	geom_abline(slope = 0, intercept = 0, alpha = 0.5) + 
	geom_point(aes(fit, res, shape = mod, colour = mod), alpha = 0.6) + 
	scale_x_continuous("Fitted values", breaks = c(0.05, 0.15, 0.25)) +
	scale_y_continuous("Residuals", breaks = c(0, 0.1, 0.2)) + 
	scale_shape("Model") + 
	scale_colour_discrete("Model") +
	opts(legend.position = 'top', legend.direction = 'vertical', legend.box = 'horizontal') + 
	guides(col = guide_legend(nrow = 2), shape = guide_legend(nrow = 2))
ggsave("fixedeffectsresidualsBYmodel.pdf", height = 7, width = 6)

#########################
# CWM MODEL FITS:
# community-weighted means
# 
#########################

# store fitted values from full-data-models
croche[["cwm.lme",shape = "cwm"]] <- as.vector(
sweep(croche$fitted.lme^2,1,apply(croche$fitted.lme^2,1,sum),"/") %*% croche$scaled.Length)
croche[["cwm.lme2",shape = "cwm"]] <- as.vector(
sweep(croche$fitted.lme2^2,1,apply(croche$fitted.lme2^2,1,sum),"/") %*% croche$scaled.Length)

# adjust them using error propagation theory
partfirstderivs <- -(croche$fitted.lme %*% outer(croche$scaled.Length,croche$scaled.Length,"-")) / matrix(rowSums(croche$fitted.lme)^2,10,12)
partsecondderivs <- -(2  * partfirstderivs)  / matrix(rowSums(croche$fitted.lme),10,12)
residvars <- sapply(getVarCov(croche.lme, type = "conditional", 1:12),function(x)x[1])
croche[["cwm.lme.corr", shape = "cwm.lme"]] <- croche$cwm.lme + as.vector(0.5 * partsecondderivs %*% residvars)
croche[["cwm.lme.sd", shape = "cwm.lme"]] <- as.vector(sqrt((partfirstderivs^2) %*% residvars))
# adjust the next model using error propagation theory
partfirstderivs <- -(croche$fitted.lme2 %*% outer(croche$scaled.Length,croche$scaled.Length,"-")) / matrix(rowSums(croche$fitted.lme2)^2,10,12)
partsecondderivs <- -(2  * partfirstderivs)  / matrix(rowSums(croche$fitted.lme2),10,12)
residvars <- sapply(getVarCov(croche.lme2, type = "conditional", 1:12),function(x)x[1])
croche[["cwm.lme2.corr", shape = "cwm.lme2"]] <- croche$cwm.lme2 + as.vector(0.5 * partsecondderivs %*% residvars)
croche[["cwm.lme2.sd", shape = "cwm.lme2"]] <- as.vector(sqrt((partfirstderivs^2) %*% residvars))

# store fitted values from summarised-data-model
croche.cwm.lm <- lm(cwm ~ week, dropdl(croche[,1]))

# organise data for plotting
cwmdf <- rbind(
	data.frame(typ = "lin", cwm = croche.cwm.lm$fitted, week = croche$week),
	data.frame(typ = "lme", cwm = croche$cwm.lme.corr, week = croche$week),
	data.frame(typ = "lme2", cwm = croche$cwm.lme2.corr, week = croche$week)
)

# test summarised data model (significant)
anova(croche.cwm.lm)

# back-transform response data to a meaningful scale
cnt <- attr(scale(croche$Length), 'scaled:center')
scl <- attr(scale(croche$Length), 'scaled:scale')
cwmdf$cwm <- (cwmdf$cwm * scl) + cnt
croche[['cwm.unscaled', shape = 'cwm']] <- (croche$cwm * scl) + cnt

# graph data
ggplot(cwmdf) +
	stat_smooth(aes(week, cwm, linetype = typ), se = FALSE, colour = 'black', alpha = 0.5, geom = 'line') + 
	scale_x_continuous("Day-of-year") +
	scale_y_continuous("Community-weighted mean body length (mm)") + 
	scale_linetype("Predictive model", 
		labels = c(
			"Summarised data model",
			"Full data model (without size-day interaction)",
			"Full data model (with size-day interaction)"
		), breaks = c("lin","lme","lme2")) + 
	geom_point(aes(week, cwm.unscaled), data = as.data.frame(dropdl(croche[,1]))) + 
	geom_abline(slope = 0, intercept = min(croche$Length), size = 1) + 
	opts(legend.position = 'top', legend.direction = 'vertical')
ggsave("cwmfits.pdf", height = 5, width = 4.5)



# organise data again to evaluate error propagation correction
cwmdf <- rbind(
	data.frame(typ = "first", cwm = croche$cwm.lme, week = croche$week),
	data.frame(typ = "second", cwm = croche$cwm.lme.corr, week = croche$week)
)
cwmdf$cwm <- (cwmdf$cwm * scl) + cnt

# graph them
ggplot(cwmdf) +
	stat_smooth(aes(week, cwm, linetype = typ), se = FALSE, colour = 'black', alpha = 0.5, geom = 'line') + 
	scale_x_continuous("Day-of-year") +
	scale_y_continuous("Community-weighted mean body length (mm)") + 
	scale_linetype("Estimate",
		labels = c("plug-in","bias-corrected"),
		breaks = c("first","second")
	) +
	geom_point(aes(week, cwm.unscaled), data = as.data.frame(dropdl(croche[,1]))) + 
	opts(legend.position = 'top', legend.direction = 'vertical')
ggsave("cwmcorrection.pdf", height = 5, width = 4.5)

# summarised data themselves
croche.sum <- data.frame(day = croche$week, cwm = croche$cwm.unscaled)
