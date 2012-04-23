inv.gau <-
function(){
	loglinpredictors <- function(vecl,vecm){
		f1 <- vecl/vecm
		f2 <- 1/f1
		list(f1=f1,f2=f2)
	}
	off <- function(vecl,vecm){(3/2)*log(vecm)}
	selcurvparams <- function(b1,b2){c(k12=b2/b1,k2=-2*b2)}
	curv <- function(l,m,theta){
		
		k12 <- theta[1]
		k1 <- sqrt(k12)
		k2 <- theta[2]
		k22 <- k2^2
		
		k <- (k12/k2)*m*(sqrt((9/4)+(k22/k12))-(3/2))
		A <- -k2/(2*k12*m)
		B <- ((l-(k1*m))^2)/l
		C <- ((k-(k1*m))^2)/k
		((k/l)^(3/2))*exp(A*(B-C))
	}
	#peak <- function(m,theta){theta[1]*m}
	#peak <- function(m,theta){
	#	A <- -(6*(theta[1]^2)*m)/(4*theta[2])
	#	B <- ((2*theta[2])/(3*theta[1])^2)
	#	A * (1 + sqrt(1+B))
	#}
	peak <- function(m,theta){
		A <- m * sqrt(theta[1])
		B <- (-3*sqrt(theta[1])) / (2*theta[2])
		C <- (9*theta[1]) / (4*(theta[2]^2))
		A * (B + sqrt(1 + C))
	}
	name <- function()"inverse gaussian"

	return(list(
		loglinpredictors=loglinpredictors,
		off=off,
		selcurvparams=selcurvparams,
		curv=curv,
		peak=peak,
		name=name)
	)
}

