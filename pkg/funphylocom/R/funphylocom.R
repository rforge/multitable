library(picante)

#' Functional traits, phylogenies, communities, simulations
#'
#' Simulates the influence of functional traits and phylogenies on
#' ecological communities, via the \code{\link{fpcomSims}} function. 
#' 
#' @docType package
#' @name funphylocom
#' @aliases funphylocom package-funphylocom
NULL

#' Functional-phylogenetic community simulations
#'
#' Simulations for assessing the relative importance of phylogenetic 
#' versus functional information for understanding variation in
#' community composition.
#'
#' Simulations are based on the following procedure:
#' \describe{
#'
#'	\item{Phylogeny}{A tree is simulated using \code{rcoal} with
#'	default settings.}
#'
#'	\item{Trait evolution}{Two traits are simulated under Brownian motion. 
#'	One trait is called 'observed' and the other 'unknown'. Both traits
#'  influence species' probabilies of occurrence, but only the 'observed'
#'	trait is included in the output. The idea behind this distinction is that
#'	information about the 'unknown' trait will possibly be present in
#'	the phylogeny, which is assumed known.}
#'
#'	\item{Probabilities of occurrence}{For each species at each site, these
#'	probabilities depend logistically on two gradients; one is observed
#'	and the other is unknown, each corresponding to the observed and unknown
#'	traits (see the \code{sim.envObserved} and \code{sim.envUnknown} arguments).
#'	The corresponding traits are the logit-scale slopes of these logistic
#'	curves -- in other words each species depends on the gradient differently
#'	depending on their traits. When \code{p} equals one (zero), only the observed
#'	(unknown) gradient and trait determine probability of occurrence. When 
#'	\code{p} is between zero and one, both the observed and unknown
#'	trait-gradient combinations have some influence.}
#'
#'	\item{Occurrence}{Each species is present at each site with probability
#'	given by the corresponding probability of occurrence.}
#'
#' }
#'
#' @param n Number of sites to simulate
#' @param m Number of species to simulate
#' @param p Number between 0 and 1 giving the relative importance of
#'	observed versus unknown traits in determining community structure
#' @param diverg.obs a vector with m elements giving the divergence
#'	effects on the observed trait for each species
#' @param diverg.unk a vector with m elements giving the divergence
#'	effects on the unknown trait for each species
#' @param sim.envObserved Numeric vector of simulated values for the 
#'	observed environmental variables
#' @param sim.envUnknown Numeric vector of simulated values for the 
#'	unknown environmental variables
#' @param site.names Character vector of site names
#' @param spp.names Character vector of species names
#' @return An object of class \code{fpcomSims} with components:
#'	\item{comm}{An n-by-m matrix of presences and absences}
#'	\item{probs}{An n-by-m matrix of probabilities of occurrence}
#'	\item{traits}{A length-m vector of values for the observed trait}
#'	\item{env}{A length-n vector of values for the observed gradient}
#'	\item{tree}{A phylo object with the phylogenetic tree relating
#'	the m species}
#' @export
fpcomSims <- function(n, m, p = 0.5,
	diverg.obs = rep(0, m),
	diverg.unk = rep(0, m),
	sim.envObserved = as.vector(scale(rnorm(n))),
	sim.envUnknown = as.vector(scale(rnorm(n))),
	site.names = numnames(n,"site"), 
	spp.names = numnames(m,"sp")
){
	
	names(sim.envObserved) <- names(sim.envUnknown) <- site.names
	
	# simulate evolutionary history
	sim.tree <- rcoal(m, tip.label = spp.names)
	sim.tree$tip.label <- spp.names
	
	# store the resulting traits for the species
	sim.traits.obs <- as.vector(t(chol(vcv(sim.tree))) %*% rnorm(m, sd = 1))
	sim.traits.unk <- as.vector(t(chol(vcv(sim.tree))) %*% rnorm(m, sd = 1))
	sim.traits.obs <- sim.traits.obs + diverg.obs
	sim.traits.unk <- sim.traits.unk + diverg.unk
	names(sim.traits.obs) <- spp.names

	# simulate the contemporary communities
	sim.eta <- (p*outer(sim.envObserved, sim.traits.obs)) + 
			   ((1-p)*outer(sim.envUnknown, sim.traits.unk))
	sim.eta <- scale(as.vector(sim.eta))
	dim(sim.eta) <- c(n, m)
	sim.p <- exp(sim.eta)/(1+exp(sim.eta))
	sim.comm <- matrix(rbinom(n*m, 1, sim.p), n, m)
	dimnames(sim.comm) <- dimnames(sim.p) <- list(site.names, spp.names)

	ecosysfunc <- (p*sim.envObserved) + ((1-p)*sim.envUnknown)

	out <- list(comm = sim.comm, 
		probs = sim.p,
		traits = structure(sim.traits.obs, unknown = sim.traits.unk),
		env = sim.envObserved,
		ecosysfunc = ecosysfunc,
		tree = sim.tree,
		tuning = p)
	class(out) <- "fpcomSims"
	return(out)
}

update.fpcomSims <- function(object, nnew = 1, 
	sim.envObserved = rnorm(nnew),
	sim.envUnknown = rnorm(nnew),
	site.names = numnames(length(object$env) + nnew, "site"),
	spp.names = numnames(length(object$traits),"sp"), ...){

	m <- ncol(object$probs)
	
	sim.traits.obs <- structure(object$traits, unknown = NULL)
	sim.traits.unk <- attr(object$traits, 'unknown')
	p <- object$tuning

	# simulate the new contemporary communities
	sim.eta <- (p*outer(sim.envObserved, sim.traits.obs)) + 
			   ((1-p)*outer(sim.envUnknown, sim.traits.unk))
	sim.eta <- scale(as.vector(sim.eta))
	dim(sim.eta) <- c(nnew, m)
	sim.p <- exp(sim.eta)/(1+exp(sim.eta))
	sim.comm <- matrix(rbinom(nnew*m, 1, sim.p), nnew, m)
	
	sim.p <- rbind(object$probs, sim.p)
	sim.comm <- rbind(object$comm, sim.comm)
	sim.envObserved <- c(object$env, sim.envObserved)
		
	dimnames(sim.comm) <- dimnames(sim.p) <- list(site.names, spp.names)
	names(sim.envObserved) <- site.names
	
	ecosysfunc <- sim.envObserved + sim.envUnknown
	
	out <- list(comm = sim.comm, 
		probs = sim.p,
		traits = object$traits,
		env = sim.envObserved,
		ecosysfunc = ecosysfunc,
		tree = object$tree,
		tuning = p)
	class(out) <- "fpcomSims"
	return(out)
}

#' Plot fpcomSims objects
#'
#' This plot method can produce four different graphical summaries of the
#' output of the \code{\link{fpcomSims}} function.
#'
#' \describe{
#'
#'	\item{\code{plottype} equals \code{"distance"}}{A scatterplot
#'	of the phylogenetic versus functional distances between the species 
#'	pairs are produced. Species numbers are used to label the points. The
#'	\code{\link{cophenetic}} function is used to compute the phylogenetic
#'	distances and the \code{\link{dist}} function is used to compute the
#'	functional Euclidean distances. Both distances are standardised such
#'	that the maximum distance is one.}
#'	
#'	\item{\code{plottype} equals \code{"traitgram"}}{A \code{\link{traitgram}}
#'	is produced, which combines both the phylogeny and the observed trait.}
#'
#'	\item{\code{plottype} equals \code{"gradient"}}{A scatterplot of the 
#'	probabilities of occurrence versus the observed gradient is produced.
#'	The species are identified by their numbers. Note that these probabilities
#'	of occurrence depend only on the two gradients and the two traits, and
#'	therefore are not subject to variation among sites with identical gradient
#'	values. Such variation is expressed in the \code{comm} element of
#'	\code{fpcomSims} objects, which gives not probability of occurrence but
#'	occurrence itself. Note that if the \code{p} argument to \code{fpcomSims}
#'	is one, then only the observed trait and gradient determine probability of
#'	occurrence and the plot consists of perfect sigmoid curves.  But if \code{p}
#'	is zero then only the unknown gradient and trait determine probability of
#'	occurrence and the plot is just noise. In this latter case we would expect
#'	phylogenetic distance to provide more important information about communities.}
#'
#'	\item{\code{plottype} equals \code{"ordination"}}{A special type of 
#'	ordination of the species is produced. This ordination is based on the
#'	probabilities of occurrence and not on occurrence itself, and so it is able
#'	to fully explain all variation on two axes (if \code{p} is not either zero
#'	of one). Therefore, such an ordination is not possible in practice with real
#'	data but it is useful in this case for fully representing distances between
#'	species in species distribution space. Technically it is a singular value
#'	decomposition of the logit transformed probabilities of occurrence. Note
#'	well the percentages of variation explained by the two axes, and that this
#'	ordination is gauranteed to have 100 percent of the variation explained by
#'	the first two axes.}
#' }
#'
#' @param x An \code{fpcomSims} object
#' @param y Not used at the moment
#' @param cex.add The cex graphics parameter for additional graphical 
#'	elements not produced by the \code{\link{plot}} command
#' @param plottype The type of plot to produce -- see details
#' @param ... Additional parameters to be passed to \code{\link{plot}}
#' @method plot fpcomSims
#' @return No return value, called for its side-effect of producing a plot.
#' @export
plot.fpcomSims <- function(x,y,cex.add=0.8,
	plottype=c("distance","traitgram","gradient","ordination"),...){
	
	if(plottype[1]=="distance"){
		PD <- cophenetic(x$tree)/max(cophenetic(x$tree))
		PD <- PD[order(row.names(PD)),order(row.names(PD))]
		FD <- as.matrix(dist(x$traits))/max(as.matrix(dist(x$traits)))
		plot(PD,FD,type="n",
			xlab="phylogenetic distance",
			ylab="functional distance",
			...)
		text(as.dist(PD),as.dist(FD),
			paste(as.dist(col(PD)),as.dist(row(FD)),sep=","),
			cex=cex.add)
	}
	
	if(plottype[1]=="traitgram"){
		traitgram(x$traits,x$tree,...)
	}
	
	if(plottype[1]=="gradient"){
		plot(x$env,x$probs[,1],type="n",ylim=c(0,1),
			xlab="gradient",
			ylab="probability of occurrence",
			...)
		for(i in 1:ncol(x$comm)){
			text(x$env,x$probs[,i],i,cex=cex.add)
		}
	}
	
	if(plottype[1]=="ordination"){
		x.ord <- svd(log(x$probs/(1-x$probs)))
		plot(x.ord$v[,1:2]%*%diag(x.ord$d[1:2]),type="n",asp=1,
			xlab=paste("Ordination axis I (",
				100*round(x.ord$d[1]/sum(x.ord$d),2),"%)",sep=""),
			ylab=paste("Ordination axis II (",
				100*round(x.ord$d[2]/sum(x.ord$d),2),"%)",sep=""),
			...)
		text(x.ord$v[,1:2]%*%diag(x.ord$d[1:2]),labels=names(x$traits),cex=cex.add)
	}
}



#' Generates numbered names
#'
#' Generates numbered names based on a character prefix that will
#' be alphanumerically sorted as expected.
#'
#' @param n Number of names
#' @param prefix Character prefix to go in front of the numbers
#' @return A character vector of numbered names.
#' @export
numnames <- function(n,prefix="name"){
	n <- as.integer(n)
	if(n < 1) stop("number of names must be one or more")
	zeropad.code <- paste("%0",nchar(n),".0f",sep="")
	numpart <- sprintf(zeropad.code,1:n)
	paste(prefix,numpart,sep="")
}

#' Tail of a list
#'
#' utility function to make extractions of the 
#' trait values at the tips easier
#'
#' @param x A list
#' @param n Number of lines
#' @param ... Not used
#' @return The first \code{n} elements of each list element.
#' @method tail list
tail.list <- function(x,n=6L,...){
	sapply(x,function(xi)rev(rev(xi)[seq_len(n)]))
}

#' Functional phylogenetic distance
#'
#' Computates a functional phylogenetic distance matrix from 
#' phylogenetic and functional distance matrices and two
#' weighting parameters.
#'
#' @param PD A phylogenetic distance matrix.
#' @param FD A functional distance matrix.
#' @param a A number between 0 and 1 giving the amount of weight
#'	to put on \code{PD} relative to \code{FD}.
#' @param p A number giving the \code{p}-norm.
#' @param ord Order rows and columns of the matrices?  (defaults
#'  to \code{TRUE}).
#' @return A distance matrix.
#' @note This function is not very user friendly yet.  There are
#'	no doubt many use cases that I've ignored.
#' @export
FPD <- function(PD, FD, a, p, ord = TRUE){
	PD <- as.matrix(PD)
	FD <- as.matrix(FD)
	if(
		is.null(rownames(FD)) ||
		is.null(colnames(FD)) ||
		is.null(rownames(PD)) ||
		is.null(colnames(PD))
	) stop('distance matrices must have row and column names')
	if(
		any(rownames(FD) != colnames(FD)) ||
		any(rownames(PD) != colnames(PD))
	) stop('row and column names must match for distance matrices')
	if(ord){
		FD <- FD[order(rownames(FD)), order(rownames(FD))]
		PD <- PD[order(rownames(PD)), order(rownames(PD))]
	}
	if(
		any(rownames(FD) != rownames(PD)) ||
		any(colnames(FD) != colnames(PD))
	) stop('FD and PD must have same row and column names')

	((a*(PD^p)) + ((1-a)*(FD^p)))^(1/p)
}

#' Rao's quadratic entropy
#'
#' Computes Rao's quadratic entropy from a distance matrix and
#' community matrix.
#'
#' @param D A species by species distance matrix.
#' @param X A sites by species community matrix.
#' @param ord Order rows and columns of the matrices? (defaults
#'  to \code{TRUE}).  Note that sites are never ordered.
#' @return A vector of diversity indices (one for each site).
#' @export
rao <- function(X, D, ord = TRUE){
	if(
		is.null(rownames(D)) ||
		is.null(colnames(D))
	) stop('distance matrix must have row and column names')
	if(is.null(colnames(X)))
		stop('community matrix must have column (i.e. species) names')
	if(any(rownames(D) != colnames(D)))
		stop('row and column names must match for distance matrix')
	if(ord){
		X <- X[, order(colnames(X))]
		D <- D[order(rownames(D)), order(rownames(D))]
	}
	if(any(colnames(X) != colnames(D)))
		stop('species names must match')
	
	rs <- apply(X, 1, sum)
	X.rel <- sweep(X, 1, rs, FUN = '/')
	X.rel[is.nan(X.rel)] <- 0
	out <- apply(X.rel, 1, function(x) x %*% D %*% x)/2

	names(out) <- rownames(X)
	return(out)
}

#' Functional phylogenetic diversity generalised linear model
#'
#' Calculate the deviance for a generalised linear model using
#' functional phylogenetic diversity indices as predictors.
#'
#' @param ap A vector with two numbers (a and p), see \code{\link{FPD}}.
#' @param x A community matrix
#' @param y An ecosystem function (or other site characteristic being
#'	used as a response variable)
#' @param PD A phylogenetic distance matrix.
#' @param FD A functional distance matrix.
#' @param index Character string indicating the diversity index used 
#'	to combine distances and community data. Currently, either \code{'mpd'}
#'	or \code{'rao'}.
#' @param ... Additional arguments to pass to \code{\link{glm}}.
#' @return A deviance value.
#' @export
FPDglm_ap <- function(ap, x, y, PD, FD, index = 'mpd', ...){
	# TODO: make the 'index' argument more generalizable by
	# allowing user supplied functions
	FPD. <- FPD(PD, FD, ap[1], ap[2])
	if(index == 'rao') fpd <- rao(x, FPD.)
	else if(index == 'mpd') fpd <- mpd.(x, FPD.)
	else stop('index not recognised')
	glm(y ~ fpd, ...)
}

mpd. <- function (samp, dis, abundance.weighted = FALSE) 
{
    N <- dim(samp)[1]
    mpd <- numeric(N)
    for (i in 1:N) {
        sppInSample <- names(samp[i, samp[i, ] > 0])
        if (length(sppInSample) > 1) {
            sample.dis <- dis[sppInSample, sppInSample]
            if (abundance.weighted) {
                sample.weights <- t(as.matrix(samp[i, sppInSample, 
                  drop = FALSE])) %*% as.matrix(samp[i, sppInSample, 
                  drop = FALSE])
                mpd[i] <- weighted.mean(sample.dis, sample.weights)
            }
            else {
                mpd[i] <- mean(sample.dis[lower.tri(sample.dis)])
            }
        }
        else {
            mpd[i] <- 0  # used to be NA in picante.  only change here.
        }
    }
    mpd
}


#' Grid search functional phylogenetic diversity generalised linear model
#'
#' Calculate deviances over a grid for generalised linear models using
#' functional phylogenetic diversity indices as predictors.
#'
#' @param a A vector of numbers between 0 and 1 giving the amount of 
#'	weight to put on \code{PD} relative to \code{FD}.
#' @param p A vector of numbers giving the \code{p}-norm. (BUT p must
#'	for now be a single number!)
#' @param x A community matrix
#' @param y An ecosystem function (or other site characteristic being
#'	used as a response variable)
#' @param PD A phylogenetic distance matrix.
#' @param FD A functional distance matrix.
#' @param ... Additional arguments to pass to \code{\link{glm}}.
#' @return data frame with three columns (a, p, and deviances).
#' @export
FPDglm_grid <- function(a, p, x, y, PD, FD, ...){
	aps <- merge(a, p)
	glms <- lapply(as.data.frame(t(aps)),
		FPDglm_ap,
		x = x, y = y, 
		PD = PD, FD = FD,
		...)
	loglikes <- sapply(glms, logLik)
	slopes <- sapply(glms, function(xx) xx$coefficients[2])
	likelihood <- exp(loglikes - max(loglikes))
	posterior <- likelihood/(sum(mean(diff(a))*likelihood))
	surf <- data.frame(
		a = aps$x, p = aps$y, 
		loglikes, slopes, posterior
	)
	return(surf)
}




#' Draw Ackerly traitgram and return phylogenetic and functional distances
#' 
#' Draws Ackerly's \code{\link{traitgram}}, but also returns
#' information about the distance relationships implied by the traitgram.
#' 
#' @param x See \code{\link{traitgram}}
#' @param phy See \code{\link{traitgram}}
#' @param ... Additional arguments to \code{\link{traitgram}}
#' @param plot Should the traitgram be plotted?
#' @param a Optional, see \code{\link{FPD}}
#' @param p Optional, see \code{\link{FPD}}
#' @return A plot (if \code{plot = TRUE}) and an object of 
#'	class \code{traitgram} with elements:
#'	\item{traits}{The trait values}
#'	\item{tree}{The phylogenetic tree}
#'	\item{PD}{A phylogenetic distance matrix (computed with
#'	\code{\link{cophenetic}})}
#'  \item{FD}{A functional distance matrix}
#'	\item{FPD}{A distance matrix combining both phylogenetic
#'	and functional distances (see \code{\link{FPD}})}
#' @export
traitgram2 <- function(x, phy, ..., plot = TRUE, a, p){
	# make it work if PD and FD names are factor and character
	# respectively or vice versa.
	if(!is.numeric(x)) stop('the trait, x, must be numeric')
	attr(x, 'dim') <- NULL
	tl <- phy$tip.label
	if(is.null(tl)) stop('phylogenetic tips must have labels')
	if(length(x) != length(tl)) stop('x must be a vector of traits')
	if(is.null(names(x))) stop('x must have names')
	
	PD <- cophenetic(phy)/max(cophenetic(phy))
	PD <- PD[order(row.names(PD)),order(row.names(PD))]
	FD <- as.matrix(dist(x))/max(as.matrix(dist(x)))
	FD <- FD[order(row.names(FD)),order(row.names(FD))]
	if(plot) traitgram(x, phy, ...)
	out <- structure(
		list(traits = x, tree = phy, PD = PD, FD = FD), 
		class = 'traitgram'
	)
	if(!missing(a) && !missing(p))
		out$FPD <- FPD(PD, FD, a, p)
	return(out)
}

#' Plot traitgram objects
#'
#' Method for plotting objects of class traitgram 
#'
#' @param x A traitgram object
#' @param ... Arguments to pass to \code{\link{traitgram}}
#' @return No return value, plot produced
#' @method plot traitgram
#' @export
plot.traitgram <- function(x, ...)
	traitgram(x$traits, x$tree, ...)

#' Highest posterior density region for a
#'
#' Find points on a grid within a 100\code{p}\% highest posterior 
#' density region for the tuning parameter, a
#'
#' The 100\code{p}\% highest posterior density region for 'a' is 
#' the subset of the interval between 0 and 1, which contains 
#' 100\code{p}\% of the probability.
#'
#' @param a_grid A vector of (preferably evenly spaced) 'a' 
#'	values (between 0 and 1).
#' @param posterior The values of the posterior density at
#'	each point in \code{a_grid}.
#' @param level Size of the highest posterior density region.
#' @return A data frame with two columns:  the values of the
#'	grid within the hpd region and the value of the posterior
#'	at each point in this grid.
#' @export
a.hpd <- function(a_grid, posterior, level = 0.95){
	out <- data.frame(a = a_grid, posterior = posterior)
	if(level == 1L) return(out)
	if(level == 0L) return(out[-(1:length(posterior)),])
	dscrt.post <- posterior/sum(posterior)
	post.ord <- order(-posterior)
	hpd.levels <- rep(0, length(posterior))
	for(n in 1:length(posterior)){
		hpd.levels[n] <- sum(dscrt.post[post.ord[1:n]])
		if(hpd.levels[n] > level) break
	}
	n <- n - 1
	post.ord <- sort(post.ord[1:n])
	hpd.points <- a_grid[post.ord]
	out <- out[post.ord, ]
	print(hpd.levels[n])
	return(out)
}


a.postsim <- function(a_grid, posterior, n = 1){
	dscrt.post <- posterior/sum(posterior)
	replicate(n, a_grid[min(which(runif(1) < cumsum(dscrt.post)))])
}

