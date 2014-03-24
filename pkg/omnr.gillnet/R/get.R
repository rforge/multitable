#'Extract lengths and mesh sizes
#'
#'@param object A \code{catch}, \code{fit.catch}, \code{fit.catch.compare} or object.
#'@param perimeter.factor Factor by which to multiply the inputted mesh sizes to obtain
#'mesh perimeters, which are rquired by the analyses.  This argument is only used for
#'\code{catch} objects because mesh sizes in \code{fit.catch} and \code{fit.catch.compare} 
#'objects have already been converted to mesh perimeters.
#'@param ... Not used now.
#'@aliases get.lens.mesh get.lens get.mesh get.lens.catch
#'@name get.lens.mesh
#'@rdname get
NULL

#'@rdname get
#'@export
get.lens <- function(object) UseMethod('get.lens')

#'@rdname get
#'@export
get.mesh <- function(object, ...) UseMethod('get.mesh')

#'@method get.lens catch
#'@rdname get
#'@export
get.lens.catch <- function(object) object$lens

#'@method get.mesh catch
#'@rdname get
#'@export
get.mesh.catch <- function(object, perimeter.factor = 1, ...) object$mesh * perimeter.factor

#'@method get.lens fit.catch
#'@rdname get
#'@export
get.lens.fit.catch <- function(object) object$l[,1]

#'@method get.mesh fit.catch
#'@rdname get
#'@export
get.mesh.fit.catch <- function(object, ...) object$m[1,]

#'@method get.lens fit.catch.compare
#'@rdname get
#'@export
get.lens.fit.catch.compare <- function(object) object[[1]]$l[,1]

#'@method get.mesh fit.catch.compare
#'@rdname get
#'@export
get.mesh.fit.catch.compare <- function(object, ...) object[[1]]$m[1,]

#'Get QAICc-best \code{fit.catch} object in a \code{fit.catch.compare} object
#'
#'@param object A \code{fit.catch.compare} object
#'@return A \code{fit.catch} object
#'@export
get.best <- function(object)
  object[attr(object, "comparisonresults")$curveindicator][[1]]
