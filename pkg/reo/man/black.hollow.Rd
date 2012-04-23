\name{black.hollow}
\alias{black.hollow}
\alias{fish}
\alias{limn}
\alias{lat}
\docType{data}
\title{Black and Hollow data}
\description{
Data from Jackson's (1988) study of lakes in the Black and Hollow River watersheds.
}
\usage{
data(fish)
data(limn)
data(lat)
}
\format{
  \code{fish} is a presence-absence data frame with 52 observations (lakes) on the following 30 variables (species).
  \describe{
    \item{\code{PS}}{Pumpkinseed}
    \item{\code{YP}}{Yellow perch}
    \item{\code{WS}}{White sucker}
    \item{\code{CC}}{Creek chub}
    \item{\code{BB}}{Brown bullhead}
    \item{\code{NRD}}{Northern redbelly dace}
    \item{\code{GS}}{Golden shiner}
    \item{\code{BT}}{Brook trout}
    \item{\code{SB}}{Smallmouth bass}
    \item{\code{BNM}}{Bluntnose minnow}
    \item{\code{CS}}{Common shiner}
    \item{\code{PD}}{Pearl dace}
    \item{\code{FSD}}{Finescale dace}
    \item{\code{LB}}{Largemouth bass}
    \item{\code{BNS}}{Blacknose shiner}
    \item{\code{B}}{Burbot}
    \item{\code{FM}}{Fathead minnow}
    \item{\code{LT}}{Lake trout}
    \item{\code{BS}}{Brook stickleback}
    \item{\code{BD}}{Blacknose dace}
    \item{\code{ID}}{Iowa dace}
    \item{\code{C}}{Cisco}
    \item{\code{LC}}{Lake chub}
    \item{\code{RB}}{Rock bass}
    \item{\code{FF}}{Fallfish}
    \item{\code{BM}}{Brassy minnow}
    \item{\code{M}}{Muskellunge}
    \item{\code{RS}}{Rainbow smelt}
    \item{\code{LW}}{Lake whitefish}
    \item{\code{NSS}}{Nine spine stickleback}
  }
  \code{limn} is a limnological data frame with 52 observations (lakes) on the following 8 variables.
  \describe{
    \item{\code{area}}{log of lake area in ha}
    \item{\code{maxd}}{log of maximum depth in m}
    \item{\code{vol}}{log of lake volume in 10^4 m^3}
    \item{\code{shore}}{log of shoreline length in km}
    \item{\code{elev}}{log of elevation in m}
    \item{\code{pH}}{pH}
    \item{\code{ca}}{log of calcium concentration in mg/l}
    \item{\code{cond}}{log of conductivity in umhos cm}
  }
 \code{lat} is a data frame with 52 observations (lakes) on the following variable.
  \describe{
    \item{\code{Latitude}}{latitude}
  } 
}
\details{
Don Jackson collected these data for his 1988 University of Toronto Masters Thesis in Zoology.
}
\source{
Jackson (1988)
}
\examples{
  data(fish)
}
\keyword{datasets}
