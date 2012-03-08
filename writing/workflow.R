rm(list = ls())

library(multitable)
library(nlme)
library(ggplot2)
library(tensor)

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

# drop the first two basins and the 2007 data
croche <- dropdl(croche[7:16,3,])

# keep only these variables
croche <- croche[c("abundance","week","Length")]

# create taxon variables
croche[["taxon", match.dimids = "taxon"]] <- c(
	"D cat","D long & dent","Bosmina long","Holo gib",
	"Cal cope","Cycl cope","nauplii","Cal adults",
	"Cycl adults","Rot colonial","Rot armoured",
	"Rot unprotected"
)

# sort the data alphabetically by taxon name
croche <- croche[, order(croche$taxon)]

# standardize Thermocline.Depth and Length and week
croche[["scaled.week", shape = "week"]] <- as.vector(scale(croche$week))
croche[["scaled.Length", shape = "Length"]] <- as.vector(scale(croche$Length))

# create a variable combining taxon and length information
croche[["taxonlength", shape = "taxon"]] <- 
	paste("(",sprintf("%.2f",croche$Length),") ", croche$taxon,sep="")

croche[["rotarm", shape = "taxon"]] <- as.factor(dimnames(croche)[[2]] == "Rotif.Armoured")

croche[["rel.abundance", shape = "abundance"]] <- 
	sweep(croche$abundance, 1 , apply(croche$abundance, 1, sum), "/")

# commuinity-weioghted scaled length
croche[["cwm", match.dimids = "time"]] <- 
	as.vector(croche$rel.abundance %*% croche$scaled.Length)

croche[["sqrt.abundance", shape="abundance"]] <- sqrt(croche$abundance)


#########################
# SPECIAL FUNCTIONS:
# 
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
# 
# 
#########################

setwd("/users/stevenwalker/documents/multitable/multitable/writing/")

croche.lme <- lme(sqrt.abundance ~ -1 + scaled.Length + I(scaled.Length^2),
	data = as.data.frame(croche),
	random = ~ scaled.week + I(scaled.week^2) | taxon,
	weights = varIdent(form = ~ 1 | taxon),
	method = "ML")

croche.lme2 <- update(croche.lme, fixed. = . ~ . + scaled.Length:scaled.week)

anova(croche.lme, croche.lme2) # p = 0.1395 -- no interaction effect

croche[["fitted.lme", shape = "abundance"]] <- 
	structure(fitted(croche.lme), dim = c(10,12), names = NULL, label = NULL)
croche[["fitted.lme.back", shape = "abundance"]] <- 
	structure(fitted(croche.lme)^2, dim = c(10,12), names = NULL, label = NULL)

ggplot(as.data.frame(croche)) + 
	facet_wrap( ~ taxonlength, ncol = 3) + 
	geom_point(aes(week, I(sqrt.abundance^2), group = taxon)) + 
	stat_smooth(aes(week, I(fitted.lme^2)), n = 100, se = FALSE, colour = 'black', alpha = 0.5, geom = 'line') + 
	#geom_point(aes(week, I(fitted.lme^2), group = taxon), colour = 'blue') + 
	scale_y_continuous("Abundance", trans = 'sqrt') + 
	scale_x_continuous("Julian day", breaks = c(180, 220, 260, 300))
ggsave("randomeffectsfit.pdf", height = 7, width = 6.5)

clme <- fm.df(croche.lme, "lme")
ggplot(clme) +
	facet_wrap( ~ tax, ncol = 3) +
	geom_abline(slope = 0, intercept = 0, alpha = 0.5) +
	geom_line(aes(fit, res)) +
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

fms.df <- rbind(
	fm.df(lmX,"abnd ~ day"),
	fm.df(lmZ,"abnd ~ size"),
	fm.df(lmZ2,"abnd ~ size + size^2"),
	fm.df(lmXbyZ2,"abnd ~ day * (size + size^2)"),
	fm.df(lmT,"abnd ~ taxon"),
	fm.df(lmX2byT,"abnd ~ taxon * (day + day^2)")
)

ggplot(fms.df) +
	facet_wrap( ~ mod, ncol = 2) + 
	geom_abline(slope = 0, intercept = 0, alpha = 0.5) +
	geom_point(aes(fit, res, shape = tax), alpha = 0.6) +
	scale_x_continuous("Fitted values") +
	scale_y_continuous("Residuals") + 
	scale_shape_manual("Taxon (length, mm)", values = 1:12)
ggsave("fixedeffectsresiduals.pdf", height = 6, width = 6.5)


#########################
# CWM MODEL FITS:
#
# 
#########################

croche[["cwm.lme",shape = "cwm"]] <- as.vector(
sweep(croche$fitted.lme^2,1,apply(croche$fitted.lme^2,1,sum),"/") %*% croche$scaled.Length)

partfirstderivs <- -(croche$fitted.lme %*% outer(croche$scaled.Length,croche$scaled.Length,"-")) / matrix(rowSums(croche$fitted.lme)^2,10,12)
partsecondderivs <- -(2  * partfirstderivs)  / matrix(rowSums(croche$fitted.lme),10,12)
residvars <- sapply(getVarCov(croche.lme, type = "conditional", 1:12),function(x)x[1])
croche[["cwm.lme.corr", shape = "cwm.lme"]] <- croche$cwm.lme + as.vector(0.5 * partsecondderivs %*% residvars)
croche[["cwm.lme.sd", shape = "cwm.lme"]] <- as.vector(sqrt((partfirstderivs^2) %*% residvars))

cwmdf <- rbind(
	data.frame(typ = "lin", cwm = lm(cwm ~ week, dropdl(croche[,1]))$fitted, week = croche$week),
	#data.frame(typ = "quad", cwm = lm(cwm ~ week + I(week^2), dropdl(croche[,1]))$fitted, week = croche$week),
	data.frame(typ = "lme", cwm = croche$cwm.lme.corr, week = croche$week)
)

cnt <- attr(scale(croche$Length), 'scaled:center')
scl <- attr(scale(croche$Length), 'scaled:scale')

cwmdf$cwm <- (cwmdf$cwm * scl) + cnt
croche[['cwm.unscaled', shape = 'cwm']] <- (croche$cwm * scl) + cnt

ggplot(cwmdf) +
	stat_smooth(aes(week, cwm, linetype = typ), se = FALSE, colour = 'black', alpha = 0.5, geom = 'line') + 
	scale_x_continuous("Jullian day") +
	scale_y_continuous("Community-weighted mean body length (mm)") + 
	scale_linetype("Predictive model", labels = c("Summarised data model","Full data model"), breaks = c("lin","lme")) + 
	geom_point(aes(week, cwm.unscaled), data = as.data.frame(dropdl(croche[,1]))) + 
	geom_abline(slope = 0, intercept = min(croche$Length), size = 1)
ggsave("cwmfits.pdf", height = 4.5, width = 6.5)




vc <- sapply(getVarCov(croche.lme, type = 'conditional', individuals = 1:12), diag)
pd <- matrix(predict(croche.lme, level = 1), 10, 12)
dimnames(pd) <- dimnames(vc)
matrix(rnorm(10*12, sd = sqrt(vc)), 10, 12)


fitted(glm(abundance ~ -1 + I(scaled.Length^2) + (1 + scaled.week + I(scaled.week^2)):taxon, croche, family = quasipoisson))^2



#########################
# GLS MODEL FITS:
#
# 
#########################

croche.gls <- gls(sqrt.abundance ~ -1 + I(scaled.Length^2) + (taxon:(scaled.week + I(scaled.week^2))),
	data = as.data.frame(croche),
	weights = varIdent(form = ~ 1 | taxon))

croche[["fitted.gls", shape = "abundance"]] <- 
	structure(fitted(croche.gls), dim = c(10,12), names = NULL, label = NULL)
croche[["fitted.gls.back", shape = "abundance"]] <- 
	structure(fitted(croche.gls)^2, dim = c(10,12), names = NULL, label = NULL)

ggplot(as.data.frame(croche)) + 
	facet_wrap( ~ taxonlength, ncol = 3) + 
	geom_point(aes(week, I(sqrt.abundance^2), group = taxon)) + 
	stat_smooth(aes(week, I(fitted.gls^2)), n = 100, se = FALSE, colour = 'black', alpha = 0.5, geom = 'line') + 
	scale_y_continuous("Abundance", trans = 'sqrt') + 
	scale_x_continuous("Julian day", breaks = c(180, 220, 260, 300))