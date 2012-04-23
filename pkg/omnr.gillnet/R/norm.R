norm <-
function(){
	loglinpredictors <- function(vecl,vecm){
		f1 <- vecl/vecm
		f2 <- f1^2
		list(f1=f1,f2=f2)
	}
	off <- function(vecl,vecm){rep(0,length(vecl))}
	selcurvparams <- function(b1,b2){c(k1=-b1/(2*b2),k22=-1/(2*b2))}
	curv <- function(l,m,theta){exp(-((l-(theta[1]*m))^2)/(2*theta[2]*(m^2)))}
	peak <- function(m,theta){theta[1]*m}
	name <- function()"normal"
	
	return(list(
		loglinpredictors=loglinpredictors,
		off=off,
		selcurvparams=selcurvparams,
		curv=curv,
		peak=peak,
		name=name)
	)
}

