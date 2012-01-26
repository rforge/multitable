dims_to_vars <- function(dl, dimids){
	all.dimids <- get.dimids(dl)
	if(missing(dimids)) dimids <- all.dimids
	dim.names <- dimnames(dl)[ match(dimids, all.dimids) ]
	
	for(i in seq_along(dimids))
		dl[[ dimids[i] , match.dimids = dimids[i] ]]  <- dim.names[[i]]
	
	return(dl)
}
