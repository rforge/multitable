big.kronecker <- function(x,...){
	if(length(x)==1) return(x[[1]])
	x[[1]] <- kronecker(x[[1]],x[[2]],...)
	x[[2]] <- NULL
	Recall(x,...)
}
