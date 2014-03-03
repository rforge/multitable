#'Create dataset of class \code{catch}
#'
#'Creates \code{catch} objects out of information on gillnet catches.  These
#'objects are required for use with the software in this package.
#'
#'
#'@aliases make.catch plot.catch
#'@param mesh a vector of gillnet mesh sizes (one for each fish).
#'@param lens a vector of fish lengths (one for each fish).
#'@param counts optional.  not important for now.  I'll explain later.
#'@param binsize positive number giving the size of the histogram bins that
#'fish lengths are categorized into.
#'@param data.name name of the data set.
#'@param x an object of class \code{catch}.
#'@param y not used...only present for consistency with default S3 method.
#'@param mesh.pos a character string indicating where to print the mesh sizes.
#'see \code{\link{legend}} for more details.
#'@param y.incr increment of the y-axis scales
#'@param cex.mesh relative point size of text indicating mesh sizes.
#'@param cex.fork relative point size of text indicating fork lengths.
#'@param cex.main relative point size of main title.
#'@param cex.xaxis relative point size of x-axis.
#'@param cex.yaxis relative point size of y-axis.
#'@param mar margins of each individual plot.
#'@param oma margins of overall plot.
#'@param line.fork line for the text indicating fork lengths.
#'@param line.main line for the main title.
#'@param \dots additional arguments to pass to \code{\link{par}}.
#'@return An object of class \code{catch} with components:
#'@returnItem counts a number-of-length-bins by number-of-mesh-sizes matrix of
#'fish counts.
#'@returnItem lens a vector of the mid-point lengths corresponding to the rows
#'of counts (note difference between input lens).
#'@returnItem mesh a vector of the raw (i.e. uncorrected by \code{perimeter.factor}
#'in \code{\link{fit.catch}}) mesh sizes corresponding to the columns of counts 
#'(note difference between input mesh).  
#'@author Steve Walker
#'@seealso \code{\link{fit.catch}}, \code{\link{plot.catch}}
#'@export
make.catch <-
function(mesh, lens, counts, binsize=20, data.name = deparse(substitute(lens))){
	
	if((!is.vector(mesh))|(!is.vector(lens)))stop("mesh and size must be vectors")
	
	if(missing(counts)){
	
		if(length(mesh)!=length(lens))stop(
			"mesh and lens must have the same number of entries when counts is missing"
		)
		if(binsize<1)stop("binsize must be positive")
			
		breaks <- fish.bins(lens,binsize)
		meshes <- sort(unique(mesh))

		n.bins <- length(breaks)-1
		n.meshes <- length(meshes)

		counts <- matrix(nrow=n.bins,ncol=n.meshes)
	
		for(k in 1:n.meshes){
			tmp.lens <- lens[mesh==meshes[k]]
			tmp.hist <- hist(tmp.lens,breaks=breaks,plot=FALSE)
			counts[,k] <- tmp.hist$counts
		}
		
		output <- list(counts=counts,lens=tmp.hist$mids,mesh=meshes,data.name=data.name)
	
	}
	
	else{
		if(binsize!=20)warning("supplied binsize not used because counts is provided")
		if(length(lens)!=dim(counts)[1])stop("counts must have as many rows as elements in size")
		if(length(mesh)!=dim(counts)[2])stop("counts must have as many columns as elements in mesh")
                
		output <- list(counts=counts,lens=lens,mesh=mesh,data.name=data.name)
	}
	
	class(output) <- "catch"
	return(output)

}

#' @S3method plot catch 
#' @method plot catch 
#' @rdname make.catch
#' @export plot.catch
plot.catch <-
  function(x, y, mesh.pos = "topleft", y.incr = 200, 
           cex.mesh = 1, cex.fork = 1, cex.main = 1.5, cex.xaxis = 1, cex.yaxis = 1,
           mar = c(1, 4, 0, 1), oma = c(4, 0, 4, 0), 
           line.fork = 2, line.main = 2, ...){
    
    lens <- x$lens
    n.mesh <- length(x$mesh)
    n.bins <- length(lens)
    halfbinsize <- abs(lens[2]-lens[1])/2
    
    #xname <- paste(deparse(substitute(x), 500), collapse = "\n")
    
    mesh.leg <- round.char(x$mesh, 1)
    
    par(mfrow = c(n.mesh, 1), mar = mar, oma = oma, ...)
    
    for(k in 1:n.mesh){
      plot(range(lens),c(0,max(x$counts)),type="n",xaxt="n",yaxt="n",ylab="")
      y.scale <- seq(from=0,to=max(x$counts),by=y.incr)
      axis(2,y.scale, labels = y.scale, las = 1, cex.axis = cex.yaxis)
      for(j in 1:n.bins){
        rect(lens[j]-halfbinsize,0,lens[j]+halfbinsize,x$counts[j,k])
      }
      legend(mesh.pos,legend=paste(mesh.leg[k],"mm mesh",sep=""),bty="n", cex = cex.mesh)
    }
    axis(1, at = lens, labels = lens, las = 3, cex.axis = cex.xaxis)
    
    mtext('Fork length (mm)', cex = cex.fork, side = 1, outer = TRUE, line = line.fork)
    mtext(paste("Histograms for",x$data.name,"catch data"), 
          cex = cex.main, side = 3, outer = TRUE, line = line.main)
  }

round.char <- function(x, digits = 0){
  if(digits < 0) digits <- 0
  rd <- round(digits)
  nc <- max(nchar(trunc(x))) + rd + 1
  fmt <- paste('%0', nc, '.', rd, 'f', sep = '')
  sprintf(fmt, x)
}
