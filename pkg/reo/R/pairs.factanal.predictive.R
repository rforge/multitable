pairs.factanal.predictive <-
function(x,vars=1:x$n.vars,ls=FALSE,...){

	fa <- x
	Y <- fa$x
	IDs <- matrix(NA,x$n.vars,x$n.vars)
	diag(IDs) <- apply(Y,2,mean)
	Y <- rbind(IDs,Y)
	
	panel.factanal <- function(x,y){
	
		which.x <- which(!is.na(x[1:fa$n.vars]))
		which.y <- which(!is.na(y[1:fa$n.vars]))
	
		x <- x[-c(1:fa$n.vars)]
		y <- y[-c(1:fa$n.vars)]
	
		points(x,y)
	
		y.predict <- predict(fa,responses=which.y,predictors=which.x)

		a <- y.predict$intercept.coefs
		b <- y.predict$slope.coefs
		s <- sqrt(y.predict$residual.covariance)
		
		C <- fa$Cov[c(which.x,which.y),c(which.x,which.y)]
		mu <- apply(fa$x[,c(which.x,which.y)],2,mean)
		
		abline(a=a,b=b,lwd=3)
		abline(a=a+(1.96*s),b)
		abline(a=a-(1.96*s),b)
		if(ls) abline(lm(y~x),lty=2,lwd=2)
	
	}
	
	pairs(Y[,vars],panel=panel.factanal)

}

