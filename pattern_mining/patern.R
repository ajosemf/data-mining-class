source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myBasic.R")
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myPreprocessing.R")
loadlibrary("arules")
loadlibrary("arulesViz")
loadlibrary("arulesSequences")
library(dplyr)
load('./preprocess.rda')

onlyDelay = dataframe%>%filter(Partida_Atrasada=="1" & Chegada_Atrasada=="1")


rules <- apriori(onlyDelay, parameter=list(supp = 0.5, conf = 0.9, minlen=2, maxlen= 10, target = "rules"), 
                 appearance=NULL, control=NULL)
inspect(rules)