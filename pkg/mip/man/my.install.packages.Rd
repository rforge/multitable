\name{my.install.packages}
\alias{mip}
\alias{my.install.packages}
\title{Install packages 'my way'}
\usage{
  my.install.packages(pkg, lib, local.repo, url.repo,
    cmd = NULL, type = "source", dir.pkg)
}
\arguments{
  \item{pkg}{Character string of the name of the package
  folder.}

  \item{lib}{Character string of the path specifying the
  local library.}

  \item{local.repo}{Character string of the path specifying
  the place (i.e. repository) to download \code{pkg}.}

  \item{url.repo}{URL containing the package folder. If
  missing, then supply a specific valid command-line
  command for obtaining the package.}

  \item{cmd}{Specific valid command-line command for
  obtaining the package.  If \code{NULL}, then the
  \code{wget} shell command is used to obtain
  \code{url.repo}.}

  \item{type}{Passed to \code{install.packages}.}

  \item{dir.pkg}{If package directory is in a sub-folder of
  the folder downloaded using \code{cmd}, then specify a
  character string with the name of this sub-folder.}
}
\value{
  Nothing is returned because it is called for its side
  effect of downloading and installing a package.
}
\description{
  User unfriendly way to download and install multiple
  versions of R packages: (1) download to specified local
  'repository' and (2) install to specified local library.
  Only tested on mac OS 10.5.8.  \code{mip} is an alias for
  \code{my.install.packages}.
}
\examples{
# create new library and repository
setwd('/users/stevenwalker/')
cdir <- getwd()
(MyRlib <- paste(cdir, '/TMP_MyRlib/', sep = ''))
(MyRrepo <- paste(cdir, '/TMP_MyRrepo/', sep = ''))
dir.create(MyRlib)
dir.create(MyRrepo)

# download (into MyRrepo) and install (into MyRlib) previous
# version of the tensor package
my.install.packages(
	pkg = 'tensor_1.4.tar.gz',
	lib = MyRlib, local.repo = MyRrepo,
	url.repo = 'http://cran.r-project.org/src/contrib/Archive/tensor/')

# two versions living together
library(tensor, lib.loc = MyRlib)
packageVersion('tensor') # 1.4
detach('package:tensor', unload = TRUE)
packageVersion('tensor') # 1.5

# remove the library and repository
unlink(MyRlib, recursive = TRUE)
unlink(MyRrepo, recursive = TRUE)
}

