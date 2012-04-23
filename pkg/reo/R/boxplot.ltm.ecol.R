boxplot.ltm.ecol <- function(x,newdata=NULL,responses0=numeric(0),responses1=1,predictors=(1:x$m)[-c(responses0,responses1)],which.lambda="optimum",...){
	
	if(is.null(predictors)) predictors <- numeric(0)
	
	if(missing(newdata)||is.null(newdata)){
		Y <- x$Y
	}
	else{
		Y <- as.matrix(newdata)
	}
	
	predictions <- predict.ltm.ecol(x,newdata,responses0=responses0,responses1=responses1,predictors=predictors,which.lambda=which.lambda)	
	YYv <- apply(Y[,responses1,drop=FALSE],1,prod)*apply(1-Y[,responses0,drop=FALSE],1,prod)
	YYt <- apply(x$Y[,responses1,drop=FALSE],1,prod)*apply(1-x$Y[,responses0,drop=FALSE],1,prod)
	n1 <- sum(YYv)
	n0 <- sum(1-YYv)
	p0 <- mean(YYt)
	boxplot(predictions~YYv,horizontal=TRUE,boxwex=0.5,las=1,ylim=c(0,1),width=c(n0,n1),...)
	abline(v=p0,lty=3)
	abline(v=n1/(n1+n0))
	invisible(predictions)
}
