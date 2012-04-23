lognorm <-
function(){
	loglinpredictors <- function(vecl,vecm){
		f2 <- log(vecm/min(vecm))
		f1 <- (log(vecl)*f2)-(0.5*(f2^2))
		list(f1=f1,f2=f2)
	}
	off <- function(vecl,vecm){rep(0,length(vecl))}
	selcurvparams <- function(b1,b2){c(mu=(1-b2)/b1,sig2=1/b1)}
	curv <- function(l,m,theta){
		m <- m/min(m)
		(m/l)*exp(theta[1]-(theta[2]/2)-(((log(l)-theta[1]-log(m))^2)/(2*theta[2])))
	}
	#peak <- function(m,theta){exp(theta[1]+log(m/min(m))-(m*theta[2]))}
	peak <- function(m,theta){exp(theta[1]+log(m/min(m))-theta[2])}
	name <- function()"lognormal"

	return(list(
		loglinpredictors=loglinpredictors,
		off=off,
		selcurvparams=selcurvparams,
		curv=curv,
		peak=peak,
		name=name)
	)
}

