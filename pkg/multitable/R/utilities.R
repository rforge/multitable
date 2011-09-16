# multivariate version of outer (base)
#
# used in: dlcast, data.list.mold
#
mouter <- function(x,...){
	spouter <- function(x,y) outer(x,y,...)
	Reduce("spouter",x)
}