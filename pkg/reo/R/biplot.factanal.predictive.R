biplot.factanal.predictive <- function(x,newdata,axes=c(1,2),which.var=1:x$n.vars,...){
	fa <- x
	if(missing(newdata)||is.null(newdata)) Y <- fa$x[,which.var,drop=FALSE]
	else Y <- as.matrix(newdata)[,which.var,drop=FALSE]
	
	axis.scrs <- scores(fa,Y,which.var=which.var)[,axes]
	
	if(length(axes)>2){
		warning("only first two supplied axes are plotted")
		axes <- axes[1:2]
	}
	if(fa$factors==1){
		warning("only one factor in model -- ignore RANDOM second axis!")
		n <- fa$n.obs
		p <- fa$n.vars
		x <- cbind(runif(n,-1,1),axis.scrs)
		y <- cbind(runif(p,-1,1),fa$loadings)
		biplot(x,y,xlab="DO NOT INTERPRET THIS AXIS -- RANDOM JITTER ONLY!",...)
	}
	else{
		biplot(axis.scrs,fa$loadings[which.var,axes,drop=FALSE],...)
	}
}

