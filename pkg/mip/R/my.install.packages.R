#' Install packages 'my way'
#' 
#' User unfriendly way to download and install multiple
#' versions of R packages:  (1) download to specified local
#' 'repository' and (2) install to specified local library.
#' Only tested on mac OS 10.5.8.  \code{mip} is an alias for
#' \code{my.install.packages}.
#'
#' @param pkg Character string of the name of the package
#'  folder.
#' @param lib Character string of the path specifying the
#'  local library.
#' @param local.repo Character string of the path specifying
#'  the place (i.e. repository) to download \code{pkg}.
#' @param url.repo URL containing the package folder. If missing,
#'  then supply a specific valid command-line command for obtaining 
#'  the package.
#' @param cmd Specific valid command-line command for obtaining the
#'  package.  If \code{NULL}, then the \code{wget} shell command is
#'  used to obtain \code{url.repo}.
#' @param type Passed to \code{install.packages}.
#' @param dir.pkg If package directory is in a sub-folder of the 
#'  folder downloaded using \code{cmd}, then specify a character
#'  string with the name of this sub-folder.
#' @return Nothing is returned because it is called for its side
#'  effect of downloading and installing a package.
#' @export
#' @aliases mip
#' @examples
#' # create new library and repository
#' setwd('/users/stevenwalker/')
#' cdir <- getwd()
#' (MyRlib <- paste(cdir, '/TMP_MyRlib/', sep = ''))
#' (MyRrepo <- paste(cdir, '/TMP_MyRrepo/', sep = ''))
#' dir.create(MyRlib)
#' dir.create(MyRrepo)
#' 
#' # download (into MyRrepo) and install (into MyRlib) previous 
#' # version of the tensor package
#' my.install.packages(
#' 	pkg = 'tensor_1.4.tar.gz',
#' 	lib = MyRlib, local.repo = MyRrepo,
#' 	url.repo = 'http://cran.r-project.org/src/contrib/Archive/tensor/')
#' 
#' # two versions living together
#' library(tensor, lib.loc = MyRlib)
#' packageVersion('tensor') # 1.4 
#' detach('package:tensor', unload = TRUE)
#' packageVersion('tensor') # 1.5
#' 
#' # remove the library and repository
#' unlink(MyRlib, recursive = TRUE)
#' unlink(MyRrepo, recursive = TRUE)
my.install.packages <- function(pkg, lib, local.repo, url.repo, cmd = NULL, type = 'source', dir.pkg){

	# set directory to the desired local repository 
	cdir <- getwd()
	setwd(local.repo)
	on.exit(setwd(cdir))

	# download the package into the local repository
	if(is.null(cmd) & !missing(url.repo)){
		syscmd <- paste('wget ', url.repo, pkg, sep = '')
		system(syscmd)
	}
	else if(is.null(cmd)){
		stop('no url.repo or cmd, but one is required')
	}
	else {
		system(cmd)
	}
	
	if(!missing(dir.pkg)) local.repo <- paste(local.repo, dir.pkg, sep = '')
	
	# install the package into the local library
	install.packages(
		pkgs = paste(local.repo, pkg, sep = ''),
		repos = NULL, lib = lib, type = type)
}

mip <- my.install.packages
#' @export
