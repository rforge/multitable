predict.factanal.predictive <-
function(object, newdata, responses = c(1), predictors = (1:object$n.vars)[-responses], ...){

	Y.old <- as.matrix(object$x)
	n <- object$n.obs
	p <- object$n.vars

	if(missing(newdata) || is.null(newdata)){Y.new <- Y.old}
	else{Y.new <- as.matrix(newdata)}
	
	mu <- apply(Y.old,2,mean)
	
	if(length(responses)==p){
		return(matrix(mu,n,p,byrow=TRUE))
	}
	
	muR <- mu[responses]
	muP <- mu[predictors]

	C <- object$Cov
	colnames(Y.old) <- colnames(Y.new) <- colnames(C)

	YP <- Y.new[,predictors,drop=FALSE]
	YP.center <- sweep(YP,2,muP)
	
	CPR <- C[predictors,responses,drop=FALSE]
	CPP <- C[predictors,predictors,drop=FALSE]
	CRR <- C[responses,responses,drop=FALSE]
	CRP <- C[responses,predictors,drop=FALSE]
	
	slope.coefs <- solve(CPP)%*%CPR
	intercept.coefs <- muR - (muP%*%slope.coefs)
	muRbarP.center <- YP.center%*%slope.coefs
	muRbarP <- sweep(muRbarP.center,2,muR,FUN="+")
	
	residual.covariance <- CRR - (CRP%*%solve(CPP)%*%CPR)
	
	output <- list(conditional.means=muRbarP,slope.coefs=slope.coefs,intercept.coefs=intercept.coefs,residual.covariance=residual.covariance)
	
	return(output)
	
}

