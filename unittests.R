library(multitable)

# test 1

rm(list = ls())
a1 <- matrix(runif(50),10,5)
a2 <- matrix(runif(50),10,5)
a3 <- matrix(runif(50),10,5)
a <- list(a1,a2,a3)
b <- runif(10)
(dl <- data.list(a,b)) # should ignore dnames.

#dimnames(dl)[[1]] <- letters[1:10]
#dimnames(dl)[[2]] <- LETTERS[1:5]

as.data.frame(dl)
variablize(dl)
aperm(dl,c(2,1))


# test 2 (Note difference between the first call to data.list, which should fail with error in check.dims identifying a problem with 'variable 3', and the second call, which doesn't fail but produces a dimensionally inconsistent set of variables.)

rm(list = ls())
A <- matrix(runif(50),10,5)
B <- matrix(runif(60),10,6)
a <- list(A,A,B)
b <- runif(10)
# this call to data.list should fail with error in check.dims identifying a problem with 'variable 3'.
(dl <- data.list(a,b,list(c("A","B"),"A"),check=TRUE))
# here check=FALSE, which will allow data.list to run without an error.
(dl <- data.list(list(a,b),list(c("A","B"),"A"),check=FALSE))
# however, not failing is a bad thing because a dimensionally-inconsistent data list is generated that cannot be subscripted appropriately. for example, here we cannot get at the sixth column of variable 3 because all other variables with dimension B are only replicated five times along that dimension. subsequently and error results.
dl[,6]
# but, worse is when as.data.frame is called because this doesn't lead to an error but gives a bizzare answer!
as.data.frame(dl)

# test 3

rm(list = ls())
A <- runif(20)
B <- matrix(runif(40),20,2)
data.list(A=A,B=B)
data.list(B=B,A=A)

# test 4

rm(list = ls())
A <- data.frame(A1 = runif(20),A2 = runif(20))
B <- matrix(runif(40),20,2)
data.list(A,B=B)
data.list(B=B,A)

# test 5

rm(list = ls())
n <- 5
m <- 4
p <- 3
q <- 2
A <- runif(p)
B <- matrix(runif(q*m),q,m)
C <- array(runif(n*m*p*q),c(n,m,p,q))
D <- runif(m)
E <- array(runif(m*p*q),c(m,p,q))

dl <- data.list(A=A,B=B,C=C,D=D,E=E)
dimnames(dl) <- list(paste("n",LETTERS[1:n],sep=""),
					paste("m",LETTERS[1:m],sep=""),
					paste("p",LETTERS[1:p],sep=""),
					paste("q",LETTERS[1:q],sep=""))


# test 6 (should result in error in make.rep.dim.names)

rm(list = ls())
n <- 2
m <- 2
p <- 2
q <- 2
A <- runif(p)
B <- matrix(runif(q*n),q,n)
C <- array(runif(n*m*p*q),c(n,m,p,q))
D <- runif(m)
E <- array(runif(m*p*q),c(m,p,q))
data.list(A=A,B=B,C=C,D=D,E=E)

# test 7

rm(list = ls())
n <- 2
m <- 2
p <- 2
q <- 2
A <- runif(p)
B <- matrix(runif(q*n),q,n)
C <- array(runif(n*m*p*q),c(n,m,p,q))
D <- runif(m)
E <- array(runif(m*p*q),c(m,p,q))
data.list(A=A,B=B,C=C,D=D,E=E,match.dnames=list("p",c("q","n"),c("n","m","p","q"),"m",c("m","p","q")))

# test 8

rm(list = ls())
data.list(A=runif(30),B=runif(20),C=runif(10))

# test 9

rm(list = ls())
l <- list(runif(5),matrix(runif(10),5,2))
as.data.list(l)

# test 10
rm(list = ls())
l <- list(letters[c(1,1,2,2,3,3)],runif(6),matrix(letters[1:18],6,3))
dl <- as.data.list(l)
