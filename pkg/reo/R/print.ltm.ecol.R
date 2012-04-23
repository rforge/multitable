print.ltm.ecol <- function(x,digits=1,which.lambda="optimum", ...){

	if(!is.null(x$cvic)){
		cat("Regularization path:\n")
		printout <- data.frame(lambda=as.character(x$cvic$lambda),CVIC=round(x$cvic$cvic,1))
		print(printout,zero.print="0")
		cat("\n","Optimal lambda: ", x$cvic$opt.lambda)
		cat("\n")
	}

	if(is.null(x$B)) return()

	if(!is.null(x$cvic)&&(which.lambda=="optimum")) B <- x$opt.B
	else{
		if(which.lambda=="optimum"){
			warning("optimum lambda can only be computed if cvic is calculcated, first lambda in list is used")
			B <- x$B[[1]]
		}
		if(which.lambda!="optimum") B <- x$B[[which.lambda]]
	}
	
	Y <- x$Y

	rownames(B) <- colnames(Y)
	
	B.disp <- format(round(B,digits))
	B.disp[B==0] <- " "
	
	
	cat("\nCall:\n", deparse(x$call), "\n\n", sep = "")
	
	cat("Coefficients:\n")
	print(B.disp,quote=FALSE)

}