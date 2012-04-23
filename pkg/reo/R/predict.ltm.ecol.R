predict.ltm.ecol <- function(object,newdata,responses0=numeric(0),responses1=c(1),predictors=(1:object$m)[-c(responses0,responses1)],which.lambda="optimum",...){

	Y.old <- as.matrix(object$Y)
	n <- object$n
	m <- object$m
	X <- object$X
	if(!is.null(object$cvic)&&(which.lambda=="optimum")) B <- object$opt.B
	else{
		if(which.lambda=="optimum"){
			warning("optimum lambda can only be computed if cvic is calculcated, first lambda in list is used")
			B <- object$B[[1]]
		}
		if(which.lambda!="optimum") B <- object$B[[which.lambda]]
	}
	q2 <- object$q2
	
	if(missing(newdata)||is.null(newdata)){
		Y.new <- Y.new.targ <- Y.old
	}
	else{
		Y.new <- Y.new.targ <- as.matrix(newdata)
	}
	if(is.null(predictors)){
		rownames(Y.new) <- rownames(Y.new.targ) <- NULL
		Y.new <- Y.new.targ <- Y.new[1,,drop=FALSE]
	}
	Y.new.targ[,responses0] <- 0
	Y.new.targ[,responses1] <- 1
	allvars <- unique(c(responses0,responses1,predictors))
	L.allvars <- marg.sites(B[allvars,,drop=FALSE]%*%X,Y.new.targ[,allvars,drop=FALSE],q2)
	L.predictors <- marg.sites(B[predictors,,drop=FALSE]%*%X,Y.new.targ[,predictors,drop=FALSE],q2)
	out <- exp(L.allvars-L.predictors)
	return(out)
}