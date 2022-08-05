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
# remove unnecessary columns and columns already discretized
dataframe[ , c('flight_id', 
               'origin_icao',
               'real_depart_date',
               'real_depart_hour',
               'departure_delay',
               'depart_wind_speed',
               'depart_wind_direction',
               'ds_depart_day_period',
               'destination_icao',
               'real_arrival_date',
               'real_arrival_hour',
               'real_duration',
               'arrival_delay',
               'arrival_wind_speed',
               'arrival_wind_direction',
               'situation_type'
               )] = list(NULL)


###################################################
# discretization

# date columns
dataframe$expected_depart_month = format(as.Date(dataframe$expected_depart_date, format="%Y/%m/%d"),"%m")
dataframe$expected_arrival_month = format(as.Date(dataframe$expected_arrival_date, format="%Y/%m/%d"),"%m")
dataframe[ , c('expected_depart_date', 'expected_arrival_date')] = list(NULL)


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

