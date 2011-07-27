fc.data.list <-
function(Y,X,Z,rep.dim.names=c("sites","species"),respname="community"){
	dl <- list(Y,as.data.frame(X),as.data.frame(Z))
	names(dl) <- c(respname,"","")
	data.list(dl,list(rep.dim.names,rep.dim.names[1],rep.dim.names[2]))
}

