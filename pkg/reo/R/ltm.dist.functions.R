joint.sites.ord <- function(eta,Y,q,n=nrow(Y),m=ncol(Y),g2=ncol(eta)){
	A <- log(1+exp(eta))
	L <- (Y%*%eta) - (matrix(1,n,m)%*%A) + matrix(log(q),n,g2,byrow=TRUE)
	return(L)
}

marg.sites <- function(eta,Y,q,n=nrow(Y),m=ncol(Y),g2=ncol(eta)){
	L <- joint.sites.ord(eta,Y,q,n=n,m=m,g2=g2)
	return(log(rowSums(exp(L))))
}

post.sites.ord <- function(eta,Y,q,n=nrow(Y),m=ncol(Y),g2=ncol(eta)){
	L <- joint.sites.ord(eta,Y,q,n=n,m=m,g2=g2)
	L.obs <- sum(log(rowSums(exp(L))))
	L <- sweep(L,1,apply(L,1,max))
	out <- sweep(exp(L),1,rowSums(exp(L)),"/")
	attr(out,"marg") <- L.obs
	return(out)
}
