biplot.ltm.ecol <- function(x,newdata,spp=1:object$m,arrows="none",onespp=FALSE,which.lambda="optimum",g=75,cex.sites=1,cex.newsites=1, ...){
	
	if(onespp&&length(spp)>1) stop("Only allowed one species when onespp=TRUE")

	object <- x
	
	Y <- object$Y

	x <- seq(-3,3,6/(g-1))
	X1 <- cbind(1,x,x^2)%x%rep(1,g)
	X2 <- matrix(x,g^2,1)
	X1 <- cbind(X1,X2)
	X2 <- X2^2
	X <- t(cbind(X1,X2))
	
	axes <- scores.ltm.ecol(object,which.lambda=which.lambda,plot=FALSE)
	axesnew <- scores.ltm.ecol(object,newdata,which.lambda=which.lambda,plot=FALSE)
	
	if (!is.null(object$cvic) && (which.lambda == "optimum")) 
         B <- object$opt.B
     else {
         if (which.lambda == "optimum") {
             warning("optimum lambda can only be computed if cvic is calculcated, first lambda in list is used")
             B <- object$B[[1]]
         }
         if (which.lambda != "optimum") 
             B <- object$B[[which.lambda]]
     } 
	
	eta <- B%*%X
	
	if(onespp){
		plot(axes,xlim=c(-3,3),ylim=c(-3,3),pch=1,col=grey(0.5),asp=1,cex=cex.sites,
			xlab="Axis I",ylab="Axis II",las=1,main=paste(colnames(Y)[spp],collapse=","),
			cex.axis=1.2,cex.lab=1.2)
		points(axesnew,pch=0,col=grey(0.5),cex=cex.newsites)
		abline(v=0,lwd=0.5)
		abline(h=0,lwd=0.5)
		P <- array(0,c(g,g,length(spp)))
		for(k in 1:length(spp)){
			P[,,k] <- t(matrix(1/(1+exp(-eta[spp[k],])),g,g))
		}
		P <- exp(apply(log(P),c(1,2),sum))
		contour(x,x,P,add=TRUE,col="red",lwd=0.5)
		return()
	}

	if(arrows=="input") frm <- to <- list()
	if((arrows!="input")&(arrows!="none")){
		frm <- arrows$frm
		to <- arrows$to
	}
	
	plot(axes,xlim=c(-3,3),ylim=c(-3,3),pch=1,col=grey(0.5),asp=1,cex=cex.sites,
		xlab="Axis I",ylab="Axis II",las=1,
		cex.axis=1.2,cex.lab=1.2)
	points(axesnew,pch=0,col=grey(0.5),cex=cex.newsites)
	abline(v=0,lwd=0.5)
	abline(h=0,lwd=0.5)
	
	if(arrows=="input") frm <- to <- list()
	
	for(k in spp){
		contour(x,x,t(matrix(eta[k,],g,g)),main=colnames(Y)[k],nlevels=1,
			zlim=c(0,0),add=TRUE,labels=colnames(Y)[k],col="red",lwd=0.7,
			lty=1,method="edge",labcex=0.6,vfont=c("sans serif", "bold"))
		if(arrows=="input"){
			frm[[k]] <- locator(1)
			if(frm[[k]]$y<3){
				to[[k]] <- locator(1)
				arrows(frm[[k]]$x,
					frm[[k]]$y,
					to[[k]]$x,
					to[[k]]$y,
					length=0.05,col="red",lwd=0.5)
			}
		}
		if((arrows!="input")&(arrows!="none")){
			if(frm[[k]]$y<3){
				arrows(frm[[k]]$x,
					frm[[k]]$y,
					to[[k]]$x,
					to[[k]]$y,
					length=0.05,col="red",lwd=0.5)
			}
		}

	}
	if(arrows=="input") return(list(frm=frm,to=to))	
}
