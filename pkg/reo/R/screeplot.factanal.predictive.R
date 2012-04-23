screeplot.factanal.predictive <-
function(x,stat="deviance",shift=TRUE,plot=TRUE,ylab,main,grey.int=0.8,...){

	if(missing(ylab)) ylab <- stat
	stat <- get(stat)
	if(missing(main)) main <- paste("Screeplot for",deparse(substitute(x), 500), collapse = "\n")  # taken from hist.default

	fa <- x
	
	n <- fa$n.obs
	p <- fa$n.vars
	d <- fa$factors
	
	ds <- 1:floor(p + 0.5 - sqrt((2*p) + 0.25))
	measures <- rep(0,length(ds))
	for(k in ds){
		fa.tmp <- factanal.predictive(fa$x,k)
		measures[k] <- stat(fa.tmp,...)
	}
	fa.tmp <- factanal.predictive(fa$x,0)
	min.meas <- stat(fa.tmp,...)
	fa.tmp <- factanal.predictive(fa$x,max(ds)+1)
	max.meas <- stat(fa.tmp,...)
	
	measures <- c(min.meas,measures,max.meas)
	n.measures <- length(measures)
	if(shift){measures <- measures - min(measures)}
	names(measures) <- c("null",ds,"full")

	if(plot){
		plot(1:n.measures,measures,type="n",xaxt="n",main=main,xlab="Number of axes",ylab=ylab,las=1)
		if(grey.int < 1){
			rect(-1,0,1+n.measures,10,col=grey(grey.int),border=FALSE)
			box()
		}
		lines(1:n.measures,measures)
		axis(1,2:(n.measures-1),ds)
		axis(1,c(1,n.measures),c("null","full"))
		invisible(measures)
	}
	else{
		return(measures)
	}
	
}

