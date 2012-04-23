print.fit.catch <-
function(x,digits = max(3, getOption("digits") - 3),...){

	theta <- coef(x)

	cat("\nCall:\n", deparse(x$call), "\n\n", sep = "")
	
	cat("SELECTION CURVE: ",x$sel.curve()$name(),"\n\n")
	
	cat("SELECTION CURVE PARAMETERS:\n(without tangle parameter):\n")
     print.default(format(theta$notangle, digits = digits), print.gap = 2, 
            quote = FALSE)
     if(x$tangle){
     	cat("(with tangle parameters):\n")
     	print.default(format(theta$tangle, digits = digits), 
     		print.gap = 2,quote = FALSE)
     }
     
     cat("\nMODEL DEVIANCE:\n")
      print.default(format(deviance(x), digits = digits), print.gap = 2, 
            quote = FALSE)
     cat("\nCORRECTED AIC:\n")
     print.default(format(AIC(x), digits = digits), print.gap = 2, 
            quote = FALSE)
            
     invisible(x)
}

