with.data.list <- function(data, expr, func = "I", ...){
	func <- eval(as.name(func), enclos = parent.frame())
	eval(substitute(expr), func(data, ...), enclos = parent.frame())
}




within.data.list <- function(data, expr, ...){
	
	# CURRENTLY EXPERIMENTAL -- BETTER TO USE
	# [[<-.data.list and $<-.data.list INSTEAD.
	
	dimids <- names(attr(bm(data), "subsetdim"))
	
	# most of this code is just taken from within.list,
	# with my own annotations to try and understand it.
	
	parent <- parent.frame()
	
	# essentially this line converts the data list to
	# an 'environment'.  in particular it creates
	# an environment with the variables in data
	# as elements.  the subsetdim attribute remains
	# attached to each of those elements but the overall
	# attributes of the data list are gone (including
	# the names, and match.dimids, attributes)
	e <- evalq(environment(), data, parent)
	
	# evaluates the expr within the environment
	# created from the data list.
	eval(substitute(expr), e)
	
	data <- as.list(e)
	
	# here's the main change: instead of as.list (to
	# convert back to a list), use as.data.list.
	as.data.list(data, dimids = dimids)
}
