setwd("/users/stevenwalker/documents/multitable/multitable/pkg/multitable/inst/doc/")
Sweave("/users/stevenwalker/documents/multitable/multitable/pkg/multitable/inst/doc/multitable.Rnw")


setwd("/users/stevenwalker/documents/multitable/multitable/pkg/")
Sweave("/users/stevenwalker/documents/multitable/multitable/pkg/multitable.Rnw")


files <- c("community_complex.csv","environment_complex.csv","traits_complex.csv")
dimids <- c("sites","years","species")
fc <- read.multicsv(files,dimids,fill=c(0,NA,NA))
