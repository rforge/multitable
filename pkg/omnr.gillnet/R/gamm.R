gamm <-
function(){
	loglinpredictors <- function(vecl,vecm){
		f2 <- vecl/vecm
		f1 <- log(f2)
		list(f1=f1,f2=f2)
	}
	off <- function(vecl,vecm){rep(0,length(vecl))}
	selcurvparams <- function(b1,b2){c(alpha=b1+1,k=-1/b2)}
	curv <- function(l,m,theta){((l/((theta[1]-1)*theta[2]*m))^(theta[1]-1))*exp(theta[1]-1-(l/(theta[2]*m)))}
	peak <- function(m,theta){theta[2]*m*(theta[1]-1)}
	name <- function()"gamma"
	
	return(list(
		loglinpredictors=loglinpredictors,
		off=off,
		selcurvparams=selcurvparams,
		curv=curv,
		peak=peak,
		name=name)
	)
}

