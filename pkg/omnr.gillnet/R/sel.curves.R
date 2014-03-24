#'Selection curve functions
#'
#'These functions generate various other functions for making computations
#'under various assumptions about the underlying selection curve.
#'
#'\code{sel.curves} is a list with the names of the five selection
#'curve functions.
#'
#'@aliases sel.curves sel.curve norm.loc norm lognorm gamm inv.gau
#'
#'@return For \code{norm.loc}, \code{norm}, \code{lognorm}, \code{gamm}, and
#'\code{inv.gau}, a list of functions:
#'@return loglinpredictors calculates the predictors used in log-linear
#'model fitting
#'@return off calculates the offset term for log-linear model fitting
#'@return selcurvparams calculates the parameters of the selection curve
#'from the coefficients of the terms in the log linear model
#'@return curv calculates a matrix of values from the selection curve from
#'two matrices of the same size giving the fish lengths and mesh sizes
#'@return peak calculates the peak of the selection curve for different
#'mesh sizes
#'@author Steve Walker
#'@seealso \code{\link{getSelcurve}}
#'@rdname sel.curves
#'@name sel.curves
#'@examples
#'
#'data(north.pike)
#'np <- make.catch(north.pike$MESH, north.pike$FLEN)
#'lapply(sel.curves, function(x) fit.catch(np, eval(x)))
#'
NULL

#' @rdname sel.curves
#' @export
norm <-
  function(){
    loglinpredictors <- function(vecl,vecm){
      f1 <- vecl/vecm
      f2 <- f1^2
      list(f1=f1,f2=f2)
    }
    off <- function(vecl,vecm){rep(0,length(vecl))}
    selcurvparams <- function(b1,b2){c(k1=-b1/(2*b2),k22=-1/(2*b2))}
    curv <- function(l,m,theta,...){exp(-((l-(theta[1]*m))^2)/(2*theta[2]*(m^2)))}
    peak <- function(m,theta,...){theta[1]*m}
    name <- function()"normal"
    
    return(list(
      loglinpredictors=loglinpredictors,
      off=off,
      selcurvparams=selcurvparams,
      curv=curv,
      peak=peak,
      name=name)
    )
  }

#' @rdname sel.curves
#' @export
norm.loc <-
  function(){
    loglinpredictors <- function(vecl,vecm){list(f1=vecl*vecm,f2=vecm^2)}
    off <- function(vecl,vecm){rep(0,length(vecl))}
    selcurvparams <- function(b1,b2){c(k=(-2*b2)/b1,sig2=(-2*b2)/(b1^2))}
    curv <- function(l,m,theta,...){exp(-((l-(theta[1]*m))^2)/(2*theta[2]))}
    peak <- function(m,theta,...){theta[1]*m}
    name <- function()"location normal"
    
    return(list(
      loglinpredictors=loglinpredictors,
      off=off,
      selcurvparams=selcurvparams,
      curv=curv,
      peak=peak,
      name=name)
    )
  }

#' @rdname sel.curves
#' @export
lognorm <-
  function(){
    loglinpredictors <- function(vecl,vecm){
      f2 <- log(vecm/min(vecm))
      f1 <- (log(vecl)*f2)-(0.5*(f2^2))
      list(f1=f1,f2=f2)
    }
    off <- function(vecl,vecm){rep(0,length(vecl))}
    selcurvparams <- function(b1,b2){c(mu=(1-b2)/b1,sig2=1/b1)}
    curv <- function(l,m,theta,...){
      dots <- list(...)
      if(!is.null(dots$mbaseline)) mbl <- dots$mbaseline
      else mbl <- min(m)
      m <- m/mbl
      (m/l)*exp(theta[1]-(theta[2]/2)-(((log(l)-theta[1]-log(m))^2)/(2*theta[2])))
    }
    #peak <- function(m,theta){exp(theta[1]+log(m/min(m))-(m*theta[2]))}
    peak <- function(m,theta,...){
      dots <- list(...)
      if(!is.null(dots$mbaseline)) mbl <- dots$mbaseline
      else mbl <- min(m)
      exp(theta[1]+log(m/mbl)-theta[2])
    }
    name <- function()"lognormal"
    
    return(list(
      loglinpredictors=loglinpredictors,
      off=off,
      selcurvparams=selcurvparams,
      curv=curv,
      peak=peak,
      name=name)
    )
  }


#' @rdname sel.curves
#' @export
gamm <-
  function(){
    loglinpredictors <- function(vecl,vecm){
      f2 <- vecl/vecm
      f1 <- log(f2)
      list(f1=f1,f2=f2)
    }
    off <- function(vecl,vecm){rep(0,length(vecl))}
    selcurvparams <- function(b1,b2){c(alpha=b1+1,k=-1/b2)}
    curv <- function(l,m,theta,...){((l/((theta[1]-1)*theta[2]*m))^(theta[1]-1))*exp(theta[1]-1-(l/(theta[2]*m)))}
    peak <- function(m,theta,...){theta[2]*m*(theta[1]-1)}
    name <- function()"gamma"
    
    return(list(
      loglinpredictors=loglinpredictors,
      off=off,
      selcurvparams=selcurvparams,
      curv=curv,
      peak=peak,
      name=name)
    )
  }

#' @rdname sel.curves
#' @export
inv.gau <-
  function(){
    loglinpredictors <- function(vecl,vecm){
      f1 <- vecl/vecm
      f2 <- 1/f1
      list(f1=f1,f2=f2)
    }
    off <- function(vecl,vecm){(3/2)*log(vecm)}
    selcurvparams <- function(b1,b2){c(k12=b2/b1,k2=-2*b2)}
    curv <- function(l,m,theta,...){
      
      k12 <- theta[1]
      k1 <- sqrt(k12)
      k2 <- theta[2]
      k22 <- k2^2
      
      k <- (k12/k2)*m*(sqrt((9/4)+(k22/k12))-(3/2))
      A <- -k2/(2*k12*m)
      B <- ((l-(k1*m))^2)/l
      C <- ((k-(k1*m))^2)/k
      ((k/l)^(3/2))*exp(A*(B-C))
    }
    #peak <- function(m,theta){theta[1]*m}
    #peak <- function(m,theta){
    #	A <- -(6*(theta[1]^2)*m)/(4*theta[2])
    #	B <- ((2*theta[2])/(3*theta[1])^2)
    #	A * (1 + sqrt(1+B))
    #}
    peak <- function(m,theta,...){
      A <- m * sqrt(theta[1])
      B <- (-3*sqrt(theta[1])) / (2*theta[2])
      C <- (9*theta[1]) / (4*(theta[2]^2))
      A * (B + sqrt(1 + C))
    }
    name <- function()"inverse gaussian"
    
    return(list(
      loglinpredictors=loglinpredictors,
      off=off,
      selcurvparams=selcurvparams,
      curv=curv,
      peak=peak,
      name=name)
    )
  }
