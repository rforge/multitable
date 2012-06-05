\name{graphical.AdjRsqr}
\alias{graphical.AdjRsqr}
\title{Graphical adjusted R$^2$}
\usage{
  graphical.AdjRsqr(c = 1, R2c = rep(0, length(c)),
    col = "black", lwd = 1, lty = 1)
}
\arguments{
  \item{c}{Adjustment exponent}

  \item{R2c}{Vector of c-hypotheses}

  \item{col}{Line colour}

  \item{lwd}{Line width}

  \item{lty}{Line type}
}
\value{
  NULL
}
\description{
  Diagram for graphically adjusting sample R$^2$ values
}
\examples{
##########################
#
# basic idea
#
##########################

graphical.AdjRsqr()

text(0.25, 0.75, expression(paste('positive adjusted ', R^2)), adj = 0.5)
text(0.75, 0.25, expression(paste('negative adjusted ', R^2)), adj = 0.5)

##########################
#
# illustrative example (positive case)
#
##########################

graphical.AdjRsqr()

R2 <- 0.8
R02 <- 0.4

d <- 0.03

segments(R02, 0, R02, R02, lty = 3, col = grey(0.7))
segments(0, R2, R02, R2, lty = 3, col = grey(0.7))
points(R02, R2, pch = 16, cex = 1.5)
segments(R02, R02, R02, 1, lwd = 3, lend = 1)
segments(R02-d, R02, R02-d, R2, col = grey(0.6), lwd = 3, lend = 1)

text(R02-d-d-d-d-0.5*d, (R2+R02)/2,
	expression('R'^2 - 'R'[0]^2),
	col = grey(0.6))
text(R02+d+d+d, (R2+1)/2,
	expression(1 - 'R'[0]^2),
	col = grey(0))

##########################
#
# illustrative example (negative case)
#
##########################

graphical.AdjRsqr()

R2 <- 0.3
R02 <- 0.4

d <- 0.03

points(R02, R2, pch = 16, cex = 1.5)
segments(R02, 0, R02, R2, lty = 2)
segments(0, R2, R02, R2, lty = 2)
segments(R02, R2, R02, 1, lwd = 3, lend = 1)
segments(R02-d, R02, R02-d, 1, col = grey(0.6))
segments(R02-d-d-d, R02, R02-d-d-d, R2, col = grey(0.6))

text(R02-d-d-d-d-d-0.5*d, (R2+R02)/2,
	expression('R'^2 - 'R'[0]^2),
	col = grey(0.6))
text(R02-d-d-d, (R02+1)/2,
	expression(1 - 'R'[0]^2),
	col = grey(0.6))

##########################
#
# predictive adjustments
#
##########################

cexs <- 0.8
ydisp <- 0.06
graphical.AdjRsqr(c = c(1, 2))

ypos <- 0.3
text(0.7, ypos, expression('R'['adj']^2 < 0), cex = cexs)
text(0.7, ypos - ydisp, expression('R'['pred']^2 < 0), cex = cexs)

ypos <- 0.65
text(0.5, ypos, expression('R'['adj']^2 > 0), cex = cexs)
text(0.5, ypos - ydisp, expression('R'['pred']^2 < 0), cex = cexs)

ypos <- 0.8
text(0.2, ypos, expression('R'['adj']^2 > 0), cex = cexs)
text(0.2, ypos - ydisp, expression('R'['pred']^2 > 0), cex = cexs)
}

