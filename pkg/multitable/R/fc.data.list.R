fc.data.list <-
function(Y,X,Z,dnames=c("sites","species"),respname="community"){
	dl <- list(Y,as.data.frame(X),as.data.frame(Z))
	names(dl) <- c(respname,"","")
	data.list(dl,list(dnames,dnames[1],dnames[2]))
}

