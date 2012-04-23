wls.step.lasso <- function(y,n,X,b.in,eta.on,eta.off=0,g2=length(y),lambda=1,constr="none",eps=1e-08){
	
	mu <- 1/(1+exp(-eta.off-eta.on))
	w <- n*mu*(1-mu)
	m <- sum(w*(X^2))
	Z <- eta.on + ((y-(n*mu))/w)
	dim(Z) <- c(g2,1)
	
	b <- sum(w*X*Z)/sum(w*(X^2))
	if(!is.finite(b)) return(list(0,TRUE))

	eta.on <- X%*%b
	mu <- 1/(1+exp(-eta.off-eta.on))
	w <- n*mu*(1-mu)
	m <- sum(w*(X^2))
	gamma <- lambda/m
	if(!is.finite(gamma)) return(list(0,FALSE))
	return(list(soft.thresh(b,lambda/m,constr),FALSE))
}

soft.thresh <- function(b,gamma,constr="none"){
	if(constr=="none"){
		if(abs(b)<gamma)return(0)
		if(b>0)return(b-gamma)
		return(b+gamma)
	}
	if(constr=="neg"){
		if(b > -gamma)return(0)
		return(b+gamma)
	}
}

initial.B <- function(Y,n,method="ca.ls"){
	XX <- scale(corresp(Y,2)$rscore)
	XX <- cbind(1,XX[,1],XX[,1]^2,XX[,2],XX[,2]^2)	
	mcsY <- min(max(colSums(Y)),n-1)
	B <- log(mcsY/(n-mcsY))*t((2*Y)-1)%*%XX%*%solve(t(XX)%*%XX)
	return(B)
}

