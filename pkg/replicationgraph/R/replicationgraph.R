#' Replication graph
#'
#' Visualise multiple table replication structure as a bipartite graph.
#'
#' @param replicationmatrix A binary matrix with rows and columns representing
#'	dimensions of replication and variables respectively.
#' @param dimensionnames Optional character vector giving names for the
#'	dimensions of replication.  If \code{\link{missing}}, the 
#'	\code{\link{rownames}} of \code{replicationmatrix} are used.
#' @param variablenames Optional character vector giving names for the
#'	variables.  If \code{\link{missing}}, the \code{\link{colnames}} of
#'	\code{replicationmatrix} are used.
#' @param edgespace Number between 0 and 1 giving the width of the space for
#'	the edges of the graph.
#' @param boxheight Number greater than zero giving the height of the boxes 
#'	representing the nodes of the graph.  Numbers that are too large to fit in
#'	the plot region will be automatically reduced.
#' @param cex Text size.
#' @param lwd Edge width.
#' @export
#' @examples 
#' s <- matrix(c(1, 1), 2, 1, byrow = TRUE, dimnames = list(c('sites',
#'  'species'), c('abundance')))
#' replicationgraph(s)
#' s <- matrix(c(1, 1, 0, 1), 2, 2, byrow = TRUE, dimnames = list(c('sites', 
#'  'species'), c('envrnmnt', 'abundance')))
#' replicationgraph(s)
#' s <- matrix(c(1, 1, 0, 0, 1, 1), 2, 3, byrow = TRUE, dimnames = list(c('sites', '
#'  species'), c('envrnmnt', 'abundance', 'traits')))
#' replicationgraph(s)
#' s <- matrix(c(1, 1, 1, 0, 1, 1), 2, 3, byrow = TRUE, dimnames = list(c('sites', 
#'  'species'), c('envrnmnt', 'abundance', 'traits')))
#' replicationgraph(s)
#' replicationgraph(s, lwd = 10)
#' replicationgraph(s, cex = 3)
#' replicationgraph(s, edgespace = 0.1)
#' replicationgraph(s, boxheight = 0)
#' replicationgraph(s, 
#'  variablenames = c('envrnmntl\nvrbls','species\nabdncs','species\ntraits'))
replicationgraph <- function(
	replicationmatrix,
	dimensionnames,
	variablenames, 
	edgespace = 0.3, 
	boxheight = 1/(max(dim(replicationmatrix)) + 1), 
	cex = 1, lwd = 1){

	s <- replicationmatrix
	if(!missing(dimensionnames)) rownames(s) <- dimensionnames
	if(!missing(variablenames)) colnames(s) <- variablenames

	dimbox <- c((1-edgespace)/2, boxheight)
	varbox <- c((1-edgespace)/2, boxheight)
	xaxis <- c((1-edgespace)/4, 1 - (1-edgespace)/4)


	# try to get a better order with a correspondence analysis
	s <- s[order(corresp(s)$rscore), order(corresp(s)$cscore), drop = FALSE]

	ndims <- nrow(s)
	nvars <- ncol(s)

	maxboxheight <- 1/(max(nvars, ndims) + 1)
	if(boxheight > maxboxheight)
		boxheight <- maxboxheight

	# these spots need work
	if(ndims < nvars){
		if(ndims > 1){
			dimspots <- seq(
				(1 - ndims * boxheight)/(ndims + 1) + (boxheight/2),
				1 - (1 - ndims * boxheight)/(ndims + 1) + (boxheight/2),
				(1 - ndims * boxheight)/(ndims + 1) + boxheight
			)
		} else{
			dimspots <- 0.5
		}	
		if(nvars > 1){
			varspots <- seq(0, 1, 1/(nvars - 1))
		} else{
			varspots <- 0.5
		}
	} else if(nvars < ndims){
		if(nvars > 1){
			varspots <- seq(
				(1 - nvars * boxheight)/(nvars + 1) + (boxheight/2),
				1 - (1 - nvars * boxheight)/(nvars + 1) + (boxheight/2),
				(1 - nvars * boxheight)/(nvars + 1) + boxheight
			)
		} else{
			varspots <- 0.5
		}
		if(ndims > 1){
			dimspots <- seq(0, 1, 1/(ndims - 1))
		} else{
			dimspots <- 0.5
		}
	} else{
		if(ndims > 1){
			dimspots <- seq(0, 1, 1/(ndims - 1))
		} else{
			dimspots <- 0.5
		}
		if(nvars > 1){
			varspots <- seq(0, 1, 1/(nvars - 1))
		} else{
			varspots <- 0.5
		}	
	}


	par(mar = rep(1, 4))
	plot(c(0, 1), c(-boxheight, 1+boxheight), 
		type = 'n', xaxt = 'n', yaxt = 'n', 
		xlab = '', ylab = '', bty = 'n')



	for(i in seq_along(dimspots)){
		xleft <- xaxis[1] - dimbox[1]/2
		ybottom <- dimspots[i] - boxheight/2
		xright <- xaxis[1] + dimbox[1]/2
		ytop <- dimspots[i] + boxheight/2
		rect(xleft, ybottom, xright, ytop,
			col = grey(0.9), border = NA)
		text(xaxis[1], dimspots[i], labels = rownames(s)[i], adj = c(0.5, 0.5), cex = cex)
	}

	for(i in seq_along(varspots)){
		xleft <- xaxis[2] - dimbox[1]/2
		ybottom <- varspots[i] - boxheight/2
		xright <- xaxis[2] + dimbox[1]/2
		ytop <- varspots[i] + boxheight/2
		rect(xleft, ybottom, xright, ytop,
			col = grey(0.9), border = NA)
		text(xaxis[2], varspots[i], labels = colnames(s)[i], adj = c(0.5, 0.5), cex = cex)
	}

	for(i in 1:ndims){
		for(j in 1:nvars){
			if(s[i, j])
				segments(xaxis[1] + dimbox[1]/2, dimspots[i], xaxis[2] - varbox[1]/2, varspots[j],
					col = grey(0.5), lwd = lwd)
		}
	}

}