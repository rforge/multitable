dlmelt <- function(x){
	match.dimids <- attr(x, "match.dimids")
	unique.match.dimids <- unique(match.dimids)
	groups <- match(match.dimids, unique.match.dimids)
	out <- list()
	for(i in seq_along(unique(groups)))
		out[[i]] <- as.data.frame(dims_to_vars(x[groups == i, drop = FALSE, vextract = TRUE]))
	names(out) <- lapply(unique.match.dimids, paste, collapse = ".")
	return(out)
}
