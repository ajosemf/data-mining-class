source("utils/myGraphic.R")

loadlibrary("arules")
library(dplyr)

#--------------------------------------------------
#--------------------------------------------------
#                DEPARTURE
#--------------------------------------------------
#--------------------------------------------------

###################################################
# load data
load("data/preprocess.rda")
colnames(dataframe)

###################################################
# TODO: remove columns already discretized


###################################################
# TODO: discretization


###################################################
# ONLY FOR MVP: get only discretized columns
df = dataframe %>% dplyr::select(starts_with("ds_depart"), 
                                 Partida_Atrasada)
df$Partida_Atrasada = as.factor(df$Partida_Atrasada)
colnames(df)
rm(dataframe)

###################################################
# apriori

transactions = as(df, "transactions")
rules <- apriori(df, parameter=list(supp = 0.1, conf = 0.3, minlen=2, maxlen= 10, target = "rules"), 
                 appearance=list(rhs = c("Partida_Atrasada=1"), default="lhs"), control=NULL)
inspect(rules)

###################################################
# remove redundant rules

nrules <- rules[!is.redundant(rules)]
inspect(nrules)

###################################################
# process interesting measures

imrules <- interestMeasure(nrules, 
                           measure = c("support", "confidence", "lift", "chiSquared", "count"),
                           transactions = transactions)
imrules

###################################################
# merge rules and interesting measures on dataframe

nrules = as(nrules, "data.frame")
nrules$chiSquared = imrules$chiSquared
nrules

###################################################
# sort 

# by lift, confidence, support 
nrules[order(-nrules$lift, -nrules$confidence, -nrules$support),]

# by chiSquared, confidence, support 
nrules[order(-nrules$chiSquared, -nrules$confidence, -nrules$support),]


###################################################
# analysis


#--------------------------------------------------
#--------------------------------------------------
#                ARRIVAL
#--------------------------------------------------
#--------------------------------------------------

