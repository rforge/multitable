screeplot.ltm.ecol <- function(x,which.lambdas,lambda.cex=1,lambda.las=2,reg.cex=0.7,print.reg=TRUE,xlab,ylab,...){
	
	if(is.null(x$cvic)||is.null(x$B)) stop("Model must include fits to both full and cross-validated data")
	if(missing(which.lambdas)) which.lambdas=1:length(x$lambda)
	if(missing(xlab)) xlab <- "Complexity (parameters per species)"
	if(missing(ylab)) ylab <- "CVIC"
	
	cmplx <- x$complexity[which.lambdas]
	cvic <- x$cvic$cvic[which.lambdas]-min(x$cvic$cvic[which.lambdas])
	lambda <- x$lambda[which.lambdas]
	which.lambda <- which.min(cvic[which.lambdas])
	
	plot(cmplx,cvic-min(cvic),type="l",
		xlab=xlab,ylab=ylab,...)
	if(print.reg){
		axis(3,at=cmplx,labels=lambda,cex.axis=lambda.cex,las=lambda.las)
		mtext(expression(paste("Regularization parameter, ",lambda)),line=2.5,cex=reg.cex)
	}
	abline(v=cmplx[which.lambda],lty=2,lwd=0.5)
	invisible(cvic)
}