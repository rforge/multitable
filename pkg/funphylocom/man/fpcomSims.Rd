\name{fpcomSims}
\alias{fpcomSims}
\title{Functional-phylogenetic community simulations}
\usage{
  fpcomSims(n, m, p = 0.5, ouObserved = list(),
    ouUnknown = list(),
    sim.envObserved = as.vector(scale(rnorm(n))),
    sim.envUnknown = as.vector(scale(rnorm(n))),
    site.names = numnames(n, "site"),
    spp.names = numnames(m, "sp"))
}
\arguments{
  \item{n}{Number of sites to simulate}

  \item{m}{Number of species to simulate}

  \item{p}{Number between 0 and 1 giving the relative
  importance of observed versus unknown traits in
  determining community structure}

  \item{ouObserved}{Named list of arguments to pass to
  \code{\link{ouSim}} for simulating the observed trait
  (default to BM)}

  \item{ouUnknown}{Named list of arguments to pass to
  \code{\link{ouSim}} for simulating the unknown trait
  (default to BM)}

  \item{sim.envObserved}{Numeric vector of simulated values
  for the observed environmental variables}

  \item{sim.envUnknown}{Numeric vector of simulated values
  for the unknown environmental variables}

  \item{site.names}{Character vector of site names}

  \item{spp.names}{Character vector of species names}
}
\value{
  An object of class \code{fpcomSims} with components:
  \item{comm}{An n-by-m matrix of presences and absences}
  \item{probs}{An n-by-m matrix of probabilities of
  occurrence} \item{traits}{A length-m vector of values for
  the observed trait} \item{env}{A length-n vector of
  values for the observed gradient} \item{tree}{A phylo
  object with the phylogenetic tree relating the m species}
}
\description{
  Simulations for assessing the relative importance of
  phylogenetic versus functional information for
  understanding variation in community composition.
}
\details{
  Simulations are based on the following procedure:
  \describe{

  \item{Phylogeny}{A tree is simulated using
  \code{\link{rcoal}} with default settings.}

  \item{Trait evolution}{Two traits are simulated under
  Brownian motion (OU to come soon), using the
  \code{\link{ouSim}} function. One trait is called
  'observed' and the other 'unknown'. Both traits influence
  species' probabilies of occurrence, but only the
  'observed' trait is included in the output. The idea
  behind this distinction is that information about the
  'unknown' trait will possibly be present in the
  phylogeny, which is assumed known.}

  \item{Probabilities of occurrence}{For each species at
  each site, these probabilities depend logistically on two
  gradients; one is observed and the other is unknown, each
  corresponding to the observed and unknown traits (see the
  \code{sim.envObserved} and \code{sim.envUnknown}
  arguments).  The corresponding traits are the logit-scale
  slopes of these logistic curves -- in other words each
  species depends on the gradient differently depending on
  their traits. When \code{p} equals one (zero), only the
  observed (unknown) gradient and trait determine
  probability of occurrence. When \code{p} is between zero
  and one, both the observed and unknown trait-gradient
  combinations have some influence.}

  \item{Occurrence}{Each species is present at each site
  with probability given by the corresponding probability
  of occurrence.}

  }
}

