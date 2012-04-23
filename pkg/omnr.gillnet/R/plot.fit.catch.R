plot.fit.catch <- function(x,y,xlab="Fork length (mm)", ylab="Relative selection probability", resolution=500, plot.type = c("selcurve","totalselcurve","residuals","observedvrsexpected"), max.cex=2, min.cex=0.5, leg.pos = "topleft", data.name=NULL, justtangle = TRUE,...){
	
	if(justtangle && (!x$tangle)){
		warning("no fitted tangle parameter, changing justtangle = FALSE")
		justtangle <- FALSE
	}
	
	if(is.null(data.name)) data.name <- x$catch$data.name
	
	plot.type <- match.arg(plot.type)
	
	switch(plot.type,
		selcurve = {
			n.mesh <- length(x$catch$mesh)

			up <- max(x$catch$lens)
			lw <- min(x$catch$lens)
			l <- seq(lw,up,(up-lw)/(resolution-1))
			l <- l%x%t(rep(1,n.mesh))
			m <- rep(1,resolution)%x%t(x$catch$mesh*x$perimeter)  # BUG FIX:
																# added the perimeter factor
																# multiplication

			if(x$tangle) par(mfrow=c(2,1),mar=c(4,4,2,1))
			else par(mfrow=c(1,1),mar=c(4,4,2,1))
			xx <- range(x$catch$lens)
			yy <- c(0,1)
			plot(xx,yy,type="n",xlab=xlab,ylab=ylab,las=1,...)
			title(main=paste(data.name,"(",x$sel.curve()$name(),", no tangle parameter)"))
	
			r <- getSelcurve(x, l, m, tangle = FALSE)
	
			for(i in 1:n.mesh){
				lines(l[,1],r[,i],col=i)
			}
			if(x$tangle){
				plot(xx,yy,type="n",xlab=xlab,ylab=ylab,las=1,...)
				title(main=paste(data.name,"(",x$sel.curve()$name(),", with tangle parameter)"))
				
				r <- getSelcurve(x, l, m, tangle = TRUE)
				for(i in 1:n.mesh){
					lines(l[,1],r[,i],col=i)
				}
			}

		},
		totalselcurve = {
			selcurve.notangle <- getTotalSelcurve(x, tangle = FALSE)
		
			plot(x$catch$lens, selcurve.notangle, ylim = c(0,1),
				type="l", xlab = xlab, ylab = ylab, las = 1, ...)
			title(main =
				paste(data.name," total selection curve (",x$sel.curve()$name(), ")", 
				sep = ""))
		
			if(x$tangle){
				selcurve.tangle <- getTotalSelcurve(x, tangle = TRUE)
				lines(x$catch$lens, selcurve.tangle, lty = 2)
				legend(leg.pos, legend = c("no", "yes"), 
					title = "tangle?",
					lty = c(1,2), bty = "n")
			}
		},
		residuals = {
			
			if(justtangle){
				par(mfrow=c(1,1))
				grandmax <- max(x$res$tangle)
				scaleddevs <- x$res$tangle/grandmax
				cexs <- (max.cex*scaleddevs) + (min.cex*(1-scaleddevs))
			}
			else if(x$tangle){
				par(mfrow=c(2,1),mar=c(4,4,2,1))
				grandmax <- max(c(x$res$notangle,x$res$tangle))
				scaleddevs <- x$res$notangle/grandmax
				cexs <- (max.cex*scaleddevs) + (min.cex*(1-scaleddevs))
			}
			else{
				par(mfrow=c(1,1))
				grandmax <- max(c(x$res$notangle))
				scaleddevs <- x$res$notangle/grandmax
				cexs <- (max.cex*scaleddevs) + (min.cex*(1-scaleddevs))
			}
			plot(x$l,x$m,col=grey(1-cexs),pch=16,#cex=cexs,
				xlab="Fish length classes",ylab="Mesh sizes")
			title(main=paste(data.name,"(",x$sel.curve()$name(),", no tangle parameter)"))
		
			if(x$tangle && (!justtangle)){
				scaleddevs <- x$res$tangle/grandmax
				cexs <- (max.cex*scaleddevs) + (min.cex*(1-scaleddevs))
				plot(x$l,x$m,col=grey(1-cexs),pch=16,#cex=cexs,
				xlab="Fish length classes",ylab="Mesh sizes")
				title(main=paste(data.name,"(",x$sel.curve()$name(),", with tangle parameter)"))
			}
		},
		observedvrsexpected = {
			if(x$tangle) par(mfrow=c(2,1),mar=c(4,4,2,1))
			else par(mfrow=c(1,1),mar=c(4,4,2,1))
			
			plot(x$mu$notangle, x$catch$counts,
				xlab = "Fitted values",
				ylab = "Observed values")
			title(main=paste(data.name,"(",x$sel.curve()$name(),", no tangle parameter)"))
			xr <- range(x$mu$notangle)
			xs <- seq(xr[1],xr[2],1/resolution)
			pe <- cbind(qpois(0.05/2,xr), qpois(1-0.05/2,xr))
			lines(xr,pe[,1], lty = 2)
			lines(xr,pe[,2], lty = 2)
			abline(a=0,b=1)
			
			if(x$tangle){
				plot(x$mu$tangle, x$catch$counts,
					xlab = "Fitted values",
					ylab = "Observed count data")
				title(main=paste(data.name,"(",x$sel.curve()$name(),", with tangle parameter)"))
				xr <- range(x$mu$tangle)
				xs <- seq(xr[1],xr[2],1/resolution)
				pe <- cbind(qpois(0.05/2,xr), qpois(1-0.05/2,xr))
				lines(xr,pe[,1], lty = 2)
				lines(xr,pe[,2], lty = 2)
				abline(a=0,b=1)
			}
		}
	)
}