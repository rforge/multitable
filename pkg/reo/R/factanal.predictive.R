factanal.predictive <- function(x,factors,...){	
	call.pred <- match.call()
	iscovmat <- sum(!is.na(pmatch(names(call.pred),"covmat")))>0
	
	if(iscovmat)stop("covmat must not be specified because the covariance matrix must be computed from x")

	if(class(x)=="formula")stop("x cannot be a formula")
	
	n <- nrow(x)
	p <- ncol(x)

	S <- diag(apply(x,2,sd))
	
	factors <- round(factors)
	
	if(factors>p + 0.5 - sqrt((2*p) + 0.25)){
		output <- list()
		output$factors <- "full"
		output$n.obs <- n
		output$Cor <- cor(x)
		class(output) <- "factanal.predictive"
	}
	if(factors==0){
		output <- list()
		output$factors <- 0
		output$n.obs <- n
		output$Cor <- diag(1,p,p)
		class(output) <- "factanal.predictive"
	}
	if(factors<0){
		stop("can't have negative numbers of factors")
	}
	if((factors>0)&!(factors>p + 0.5 - sqrt((2*p) + 0.25))){
		output <- factanal(x,factors,scores="none",...)
		L <- output$loadings
		psi <- output$uniquenesses
		output$Cor <- (L%*%t(L)) + diag(psi)
		output$B <- S%*%L
		rownames(output$B) <- colnames(output$Cor)
		class(output) <- c("factanal","factanal.predictive")
	}

	output$call.pred <- call.pred
	output$n.vars <- p
	output$x <- as.matrix(x)
	output$Cov <- S%*%output$Cor%*%S
	colnames(output$Cov) <- rownames(output$Cov) <- colnames(output$Cor)
	output$scores <- scores(output)
	
	return(output)
}
