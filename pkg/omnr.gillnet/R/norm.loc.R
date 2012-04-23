norm.loc <-
function(){
	loglinpredictors <- function(vecl,vecm){list(f1=vecl*vecm,f2=vecm^2)}
	off <- function(vecl,vecm){rep(0,length(vecl))}
	selcurvparams <- function(b1,b2){c(k=(-2*b2)/b1,sig2=(-2*b2)/(b1^2))}
	curv <- function(l,m,theta){exp(-((l-(theta[1]*m))^2)/(2*theta[2]))}
	peak <- function(m,theta){theta[1]*m}
	name <- function()"location normal"
	
	return(list(
		loglinpredictors=loglinpredictors,
		off=off,
		selcurvparams=selcurvparams,
		curv=curv,
		peak=peak,
		name=name)
	)
}

