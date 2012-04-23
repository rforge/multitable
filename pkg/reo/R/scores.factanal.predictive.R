scores.factanal.predictive <- function(x,newdata,which.var=1:x$n.vars,...){

	p <- x$n.vars
	
	if((x$factors>0)&!(x$factors>p + 0.5 - sqrt((2*p) + 0.25))){
	
		if(missing(newdata)||is.null(newdata)) Y <- x$x[,which.var,drop=FALSE]
		else Y <- as.matrix(newdata)[,which.var,drop=FALSE]
		B <- x$B[which.var,,drop=FALSE]
		C <- x$Cov[which.var,which.var,drop=FALSE]
		Xhat <- t(B)%*%solve(C)%*%t(scale(Y,scale=FALSE))
	
		return(t(Xhat))
	}
	else return(NULL)
}

