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
#'	\item{Phylogeny}{A tree is simulated using \code{\link{rcoal}} with
#'	default settings.}
#'
#'	\item{Trait evolution}{Two traits are simulated under Brownian motion 
#'	(OU to come soon), using the \code{\link{ouSim}} function. One trait 
#'	is called 'observed' and the other 'unknown'. Both traits influence 
#'	species' probabilies of occurrence, but only the 'observed' trait is
#' 	included in the output. The idea behind this distinction is that information
#'	about the 'unknown' trait will possibly be present in the phylogeny, which 
#'	is assumed known.}
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
#' @param ouObserved Named list of arguments to pass to \code{\link{ouSim}}
#'	for simulating the observed trait (default to BM)
#' @param ouUnknown Named list of arguments to pass to \code{\link{ouSim}}
#'	for simulating the unknown trait (default to BM)
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
fpcomSims <- function(n, m, p = 0.5,
	ouObserved = list(),
	ouUnknown = list(),
	sim.envObserved = as.vector(scale(rnorm(n))),
	sim.envUnknown = as.vector(scale(rnorm(n))),
	site.names = numnames(n,"site"), 
	spp.names = numnames(m,"sp")
){
	
	names(sim.envObserved) <- names(sim.envUnknown) <- site.names
	
	# simulate evolutionary history
	sim.tree <- rcoal(m,tip.label=spp.names)
	sim.tree$tip.label <- spp.names
	#sim.ou.obs <- do.call(ouSim,c(bquote(sim.tree),ouObserved))  #ouSim(sim.tree)
	#sim.ou.unk <- do.call(ouSim,c(bquote(sim.tree),ouUnknown))  #ouSim(sim.tree)
	
	# store the resulting traits for the species
	#sim.traits.obs <- tail(sim.ou.obs$branchList,1)[match(1:m,sim.tree$edge[,2])]
	#sim.traits.unk <- tail(sim.ou.unk$branchList,1)[match(1:m,sim.tree$edge[,2])]
	sim.traits.obs <- as.vector(t(chol(vcv(sim.tree))) %*% rnorm(m, sd = 0.5))
	sim.traits.unk <- as.vector(t(chol(vcv(sim.tree))) %*% rnorm(m, sd = 0.5))
	names(sim.traits.obs) <- spp.names

	# simulate the contemporary communities
	sim.eta <- (p*outer(sim.envObserved,sim.traits.obs)) + 
			   ((1-p)*outer(sim.envUnknown,sim.traits.unk))
	sim.eta <- scale(as.vector(sim.eta))
	dim(sim.eta) <- c(n,m)
	sim.p <- exp(sim.eta)/(1+exp(sim.eta))
	sim.comm <- matrix(rbinom(n*m,1,sim.p),n,m)
	dimnames(sim.comm) <- dimnames(sim.p) <- list(site.names,spp.names)

	out <- list(comm = sim.comm, 
		probs = sim.p,
		traits = sim.traits.obs,
		env = sim.envObserved,
		tree = sim.tree)
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
#' @return A distance matrix.
#' @note This function is not very user friendly yet.  There are
#'	no doubt many use cases that I've ignored.
FPD <- function(PD,FD,a,p){
	FD <- FD[order(row.names(FD)),order(row.names(FD))]
	PD <- PD[order(row.names(PD)),order(row.names(PD))]
	((a*(PD^(1/p))) + ((1-a)*(FD^(1/p))))^p
}

raoFPD <- function(PD, FD, a, p, Y)
	Y %*% FPD(PD, FD, a, p) %*% t(Y)





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
traitgram2 <- function(x, phy, ..., plot = TRUE, a, p){
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
plot.traitgram <- function(x, ...)
	traitgram(x$traits, x$tree, ...)

