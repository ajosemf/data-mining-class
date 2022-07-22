# at Regulus
# source("data-mining-class/utils/myBasic.R")
# source("data-mining-class/utils/myGraphic.R")

# at Local
source("../utils/myBasic.R")
source("../utils/myGraphic.R")

# at Local
load("../data/bfd.rda")
colnames(bfd)


lista = bfd%>%dplyr::distinct(bfd$origin_icao)

listaMean=lapply(split(bfd$arrival_ceiling, bfd$origin_icao), mean, na.rm=TRUE)


