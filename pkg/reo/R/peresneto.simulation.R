peresneto.simulation <-
function(rmarg=rnorm,n=50,vars="nine",mat=1,...){

	data(peresneto)

	if(!((vars=="nine")||(vars=="eighteen"))){stop("not a valid peresneto matrix")}
	
	if(vars=="nine"){
		p <- 9
		set <- 1
	}
	else{
		p <- 18
		set <- 2
	}
	
	output <- matrix(rmarg(n*p,...),n,p) %*% chol(peresneto[[set]][[mat]])
	return(output)

}

