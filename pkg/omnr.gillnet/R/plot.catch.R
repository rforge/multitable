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
