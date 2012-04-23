scores.ltm.ecol <- function(x,newdata,which.lambda="optimum",which.spp=1:x$m,...){
	
	if(missing(newdata)||is.null(newdata)) Y <- x$Y[,which.spp,drop=FALSE]
	else Y <- as.matrix(newdata)[,which.spp,drop=FALSE]
	n <- nrow(Y)
	if (!is.null(x$cvic) && (which.lambda == "optimum")) 
        B <- x$opt.B[which.spp,,drop=FALSE]
    	else {
        if (which.lambda == "optimum") {
            warning("optimum lambda can only be computed if cvic is calculcated, first lambda in list is used")
            B <- x$B[[1]][which.spp,,drop=FALSE]
        }
        if (which.lambda != "optimum") 
            B <- x$B[[which.lambda]][which.spp,,drop=FALSE]
    	}
	xx <- x$x
	X <- x$X
	q <- x$q
	g <- length(q)
	eta <- B%*%X
	post <- post.sites.ord(eta,Y,q%x%q)
	post <- aperm(array(post,c(n,g,g)),c(1,3,2))
	post1 <- apply(post,c(1,2),sum)
	post2 <- apply(post,c(1,3),sum)
	axes1 <- rowSums(post1 * matrix(rep(xx),n,g,byrow=TRUE))
	axes2 <- rowSums(post2 * matrix(rep(xx),n,g,byrow=TRUE))
	axes <- cbind(axes1,axes2)
	return(axes)
	
}
