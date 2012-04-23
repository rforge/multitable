multdev <-
function(theta,l,m,sel.curve,y){
	
	# calculates the multinomial deviance for SELECT fitting
	
	phi. <- phi(theta,l,m,sel.curve)
	tosum <- y != 0  # only worry about non-zero data (avoids problems involving a deadly log(0) when the selection curve is numerically zero)
	
	-2*sum(y[tosum]*log(phi.[tosum]))
}


