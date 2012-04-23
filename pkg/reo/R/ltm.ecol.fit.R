ltm.ecol.fit <- function(Y,X,q2,Yv=Y,lambda=c(0.1,1.1,2.1,3.1,4.1),
	maxit=200,digits=1,verbose=TRUE,B0=NULL,
	constr=c("none","none","neg","none","neg"),n=nrow(Y),m=ncol(Y)
){
	if(is.null(B0)) B0 <- initial.B(Y,n)
	B <- B0
	B[B[,3]>0,3] <- 0
	B[B[,5]>0,5] <- 0

	eta <- B%*%X
	post <- post.sites.ord(eta,Y,q2)
	N <- colSums(post)
	R <- t(Y)%*%post

	B.list <- list()
	instab <- rep(0,length(lambda))

	L.out <- rep(0,length(lambda))
	L.curr <- L0 <- round(attr(post,"marg"),digits) - 0.1
	cat("log-likelihood at initial values:",L0,"\n")

	if(verbose) print(format(c("lambda","params/spp","iters","log-like"),width=12,justify="right"),quote=FALSE)
	for(l in 1:length(lambda)){
		for(iter in 1:maxit){

			eta <- B%*%X
			post <- post.sites.ord(eta,Y,q2)
			N <- colSums(post)
			R <- t(Y)%*%post
	
			L <- round(attr(post,"marg"),digits)
			for(j in 1:m){
				eta.on <- B[,1,drop=FALSE]%*%X[1,,drop=FALSE]
				eta.off <- B[,-1,drop=FALSE]%*%X[-1,,drop=FALSE]
				B.tmp <- wls.step.lasso(
					R[j,],N,t(X[1,,drop=FALSE]),B[j,k],eta.on[j,],eta.off[j,],constr=constr[1],lambda=0
				)
				B[j,1] <- B.tmp[[1]]
				if(B.tmp[[2]]) instab[l] <- 1
			}
			for(k in 2:5){
				eta.on <- B[,k,drop=FALSE]%*%X[k,,drop=FALSE]
				eta.off <- B[,-k,drop=FALSE]%*%X[-k,,drop=FALSE]
				for(j in 1:m){
					B.tmp <- wls.step.lasso(
						R[j,],N,t(X[k,,drop=FALSE]),B[j,k],eta.on[j,],eta.off[j,],constr=constr[k],lambda=lambda[l]
					)
					B[j,k] <- B.tmp[[1]]
					if(B.tmp[[2]]) instab[l] <- 1
				}
			}
			if(instab[l]==1){
				B <- B0
				break
			}
			if(L==L.curr){
				break
			}
			else L.curr <- L
		}
		if(instab[l]==0){
			eta <- B%*%X
			L.out[l] <- attr(post.sites.ord(eta,Yv,q2),"marg")
		}
		else L.out[l] <- L0
		if(verbose){
			rprt <- c(format(lambda[l]),format(round(5-(sum(B==0)/m),1)),format(iter),format(round(L.out[l],1)))
			rprt <- format(rprt,width=12,justify="right")
			print(rprt,quote=FALSE)
		}
		B.list[[l]] <- B
	}
	if(verbose) cat("\n\n")
	return(list(B=B.list,L=L.out,lambda=lambda,instab=instab))
}