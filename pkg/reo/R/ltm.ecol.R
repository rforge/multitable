ltm.ecol <- function(Y,full=TRUE,cvic=FALSE,g=10,lambda=c(0.1,1.1,2.1,3.1,4.1),maxit=200,digits=1,verbose=TRUE,B0=NULL){

	Y <- as.matrix(Y)
	if(!is.matrix(Y)) stop("Y not coercible to an object of class matrix")

	if(is.null(colnames(Y))) colnames(Y) <- 1:ncol(Y)
	if(is.null(rownames(Y))) rownames(Y) <- 1:nrow(Y)

	if((!(sum(colSums(Y)<2))==0)||(!(sum(rowSums(Y)<2))==0)||(sum(colSums(Y)==nrow(Y))>0)){
		warning("some (overly rare and / or common) species and / or (overly species poor) sites have been removed")
		chk <- TRUE
		while(chk){
			prvsvs <- !(colSums(Y)==nrow(Y))
			Y <- Y[,prvsvs]
			rares <- !(colSums(Y)<2)
			Y <- Y[,rares]
			site.out <- !(rowSums(Y)<2)
			Y <- Y[site.out,]
			if(!((!(sum(colSums(Y)<2))==0)||(!(sum(rowSums(Y)<2))==0)||(sum(colSums(Y)==nrow(Y))>0))) chk <- FALSE
		}
	}

	mc <- match.call()

	x <- seq(-3,3,6/(g-1))
	q <- dnorm(x)/sum(dnorm(x))
	q2 <- q%x%q
	
	X1 <- cbind(x,x^2)%x%rep(1,g)
	X2 <- cbind(rep(x,g),rep(x^2,g))
	X <- t(cbind(1,X1,X2))

	n <- nrow(Y)
	m <- ncol(Y)
	
	out <- list()
	
	if(full){
		Y.fit <- ltm.ecol.fit(Y,X,q2,lambda=lambda,maxit=maxit,digits=digits,verbose=verbose,B0=B0,n=n,m=m)
		Y.fit$deviance <- -2*Y.fit$L
		Y.fit$complexity<- sapply(Y.fit$B,function(BB)sum(BB!=0))/m
		Y.fit$B <- lapply(Y.fit$B,function(BB){
			out <- BB
			out[,2] <- sign(sum(sign(BB[,2])))*BB[,2]
			out[,4] <- sign(sum(sign(BB[,4])))*BB[,4]
			rownames(out) <- colnames(Y)
			colnames(out) <- c("(Intercept)","axis I","axis I^2","axis II","axis II^2")
			return(out)
		})
		out <- Y.fit
	}
	
	if(cvic){
		Bjack <- list()
		L.cv <- matrix(0,n,length(lambda))
		for(i in 1:n){
			Yt <- Y[-i,]
			Yv <- Y[i,,drop=FALSE]
			cat("cross-validating sample",i,"of",n,"\n")
			fit.tmp <- ltm.ecol.fit(Yt,X,q2,Yv,lambda,maxit,digits,verbose,B0=B0,n=n-1,m=m)
			L.cv[i,] <- fit.tmp$L
			Bjack[[i]] <- fit.tmp$B
		}
		out$cvic <- list(cvic = -2*colSums(L.cv),lambda=lambda,logliks=L.cv,Bjack=Bjack)
		out$cvic$opt.lambda <- lambda[which.min(out$cvic$cvic)]
		if(full){out$opt.B <- out$B[[which.min(out$cvic$cvic)]]}
	}
	out$Y <- Y
	out$n <- n
	out$m <- m
	out$X <- X
	out$x <- x
	out$q <- q
	out$q2 <- q2
	out$call <- mc
	
	class(out) <- "ltm.ecol"
	return(out)
}
