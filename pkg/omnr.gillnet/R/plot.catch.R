plot.catch <-
function(x,y,mesh.pos="topleft",y.incr=200,...){
	
	#######################################################################
	#
	# plot.catch(): creates a plot of the data in "catch" objects.
	#	**note that this function was designed for data with roughly 8
	#    mesh sizes.  this is a plot() method for objects of class
	#    "catch".
	#
	# REQUIRES: make.catch()
	# REQUIRED BY: na
	#
	# x: an object of class "catch"
	# mesh.pos: where to put the mesh sizes
	# y.incr: increment of the y-axis scales
	#
	# lens: vector of midpoints of fish length classes
	# n.mesh: number of mesh sizes
	# n.bins: number of fish length classes
	# halfbinsize: half the size of a fish-length bin
	# xname: string containing the name of the "catch" object
	# y.scale: vector of numbers to be plotted on the y-axes
	# 
	#######################################################################

	lens <- x$lens
	n.mesh <- length(x$mesh)
	n.bins <- length(lens)
	halfbinsize <- abs(lens[2]-lens[1])/2
	
	#xname <- paste(deparse(substitute(x), 500), collapse = "\n")

	par(mfrow=c(n.mesh+2,1),mar=c(1,4,0.5,1))

	plot(c(0,1),c(0,1),type="n",xaxt="n",yaxt="n",bty="n",xlab="",ylab="")
	text(0.5,0.5,paste("Histograms for",x$data.name,"catch data"),cex=2)

	for(k in 1:n.mesh){
		plot(range(lens),c(0,max(x$counts)),type="n",xaxt="n",yaxt="n",ylab="")
		y.scale <- seq(from=0,to=max(x$counts),by=y.incr)
		axis(2,y.scale, labels = y.scale, las = 1)
		for(j in 1:n.bins){
			rect(lens[j]-halfbinsize,0,lens[j]+halfbinsize,x$counts[j,k])
		}
		legend(mesh.pos,legend=paste(x$mesh[k],"mm mesh",sep=""),bty="n")
	}
	axis(1, at = lens, labels = lens, las = 3)
	plot(c(0,1),c(0,1),type="n",xaxt="n",yaxt="n",bty="n",xlab="",ylab="")
	text(0.5,0.5,"Fork length (mm)")
}

