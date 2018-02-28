#'Gillnet size selectivity analysis
#'
#'Gillnet size selectivity analyses for Ontario Ministry of Natural Resources.
#'
#'\tabular{ll}{ Package: \tab omnr.gillnet\cr Type: \tab Package\cr Version:
#'\tab 1.2\cr Date: \tab 2018-02-27\cr License: \tab GPL-2\cr LazyLoad: \tab
#'yes\cr LazyData: \tab yes\cr } The most important functions are
#'\code{\link{make.catch}} and \code{\link{fit.catch}}.
#'
#'@name omnr.gillnet-package
#'@aliases omnr.gillnet-package omnr.gillnet
#'@docType package
#'@author Steve Walker
#'
#'Maintainer: Steve Walker <steve.walker@@utoronto.ca>
#'@references J.F. Bromaghin (2005) A versatile net selectivity model, with
#'application to Pacific salmon and freshwater species of the Yukon River,
#'Alaska. Fisheries Research 74: 157-168.
#'
#'R.B. Millar & R.J. Fryer (1999) Estimating the size-selection curves of towed
#'gears, traps, nets and hooks. Reviews in Fish Biology and Fisheries 9:
#'89-116.
#'@keywords package
NULL

#'Gillnet data
#'
#'Yellow Perch, Walleye, and Northern Pike gillnet data.
#'
#'
#'@name yellow.perch
#'@aliases yellow.perch walleye north.pike
#'@docType data
#'@format Three data frames, each with the following 8 variables.  \describe{
#'\item{list("MESH")}{mesh size (mm)} \item{list("TLEN")}{total length (mm)}
#'\item{list("FLEN")}{fork length (mm)} \item{list("RWT")}{a numeric vector}
#'\item{list("AGE")}{age at capture} \item{list("SEX")}{sex}
#'\item{list("MAT")}{a numeric vector} \item{list("PRJ_NUM")}{project number} }
#'The \code{yellow.perch}, \code{walleye}, and \code{north.pike} datasets each
#'contain 6033, 13534, and 2948 observations (i.e. fish) respectively.
#'@source Ontario Ministry of Natural Resources
#'@keywords datasets
#'@examples
#'
#'data(yellow.perch)
#'
NULL
