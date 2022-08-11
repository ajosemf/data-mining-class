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
               'ds_arrival_day_period',
               'situation_type'
               )] = list(NULL)


###################################################
# discretization

# date columns
dataframe$expected_depart_month = format(as.Date(dataframe$expected_depart_date, format="%Y/%m/%d"),"%m")
dataframe$expected_arrival_month = format(as.Date(dataframe$expected_arrival_date, format="%Y/%m/%d"),"%m")
dataframe[ , c('expected_depart_date', 'expected_arrival_date')] = list(NULL)
dataframe$expected_depart_month = as.factor(dataframe$expected_depart_month)
dataframe$expected_arrival_month = as.factor(dataframe$expected_arrival_month)


# hour columns
dataframe$expected_depart_hour = as.numeric(dataframe$expected_depart_hour)
dataframe = dataframe %>% mutate(expected_depart_day_period = case_when(
  expected_depart_hour >= 23 | expected_depart_hour <= 4  ~ "night",
  expected_depart_hour >= 5  & expected_depart_hour <= 8  ~ "early morning",
  expected_depart_hour >= 9  & expected_depart_hour <= 10 ~ "mid morning",
  expected_depart_hour >= 11 & expected_depart_hour <= 12 ~ "late morning",
  expected_depart_hour >= 13 & expected_depart_hour <= 16 ~ "afternoon",
  expected_depart_hour >= 17 & expected_depart_hour <= 19 ~ "early evening",
  expected_depart_hour >= 20 & expected_depart_hour <= 22 ~ "late evening"
))

dataframe$expected_arrival_hour = as.numeric(dataframe$expected_arrival_hour)
dataframe = dataframe %>% mutate(expected_arrival_day_period = case_when(
  expected_arrival_hour >= 23 | expected_arrival_hour <= 4  ~ "night",
  expected_arrival_hour >= 5  & expected_arrival_hour <= 8  ~ "early morning",
  expected_arrival_hour >= 9  & expected_arrival_hour <= 10 ~ "mid morning",
  expected_arrival_hour >= 11 & expected_arrival_hour <= 12 ~ "late morning",
  expected_arrival_hour >= 13 & expected_arrival_hour <= 16 ~ "afternoon",
  expected_arrival_hour >= 17 & expected_arrival_hour <= 19 ~ "early evening",
  expected_arrival_hour >= 20 & expected_arrival_hour <= 22 ~ "late evening"
))

dataframe$expected_depart_day_period = as.factor(dataframe$expected_depart_day_period)
dataframe$expected_arrival_day_period = as.factor(dataframe$expected_arrival_day_period)
dataframe[ , c('expected_depart_hour', 'expected_arrival_hour')] = list(NULL)


# temperature columns
dataframe$ds_depart_temperature = ordered(cut(dataframe$depart_temperature, c(-Inf, 10, 18, 30, 36, Inf)),
                                          labels = c("very cold", "cold", "normal", "hot", "very hot"))
dataframe$depart_temperature = NULL

dataframe$ds_arrival_temperature = ordered(cut(dataframe$arrival_temperature, c(-Inf, 10, 18, 30, 36, Inf)),
                                           labels = c("very cold", "cold", "normal", "hot", "very hot"))
dataframe$arrival_temperature = NULL


# dew point columns
hist(dataframe$depart_dew_point)
hist(dataframe$arrival_dew_point)

set.seed(1)
dataframe$ds_depart_dew_point = discretize(dataframe$depart_dew_point,
                                           method = "cluster", 
                                           breaks = 5,
                                           ordered_result=TRUE)
set.seed(1)
dataframe$ds_arrival_dew_point = discretize(dataframe$arrival_dew_point,
                                            method = "cluster", 
                                            breaks = 5,
                                            ordered_result=TRUE)
plot(dataframe$ds_depart_dew_point)
plot(dataframe$ds_arrival_dew_point)
dataframe$depart_dew_point = NULL
dataframe$arrival_dew_point = NULL




###################################################
# get depart columns
df = dataframe %>% dplyr::select(airline_icao,
                                 linetype_code,
                                 starts_with("expected_depart"),
                                 starts_with("ds_depart"),
                                 Partida_Atrasada)
df$Partida_Atrasada = as.factor(df$Partida_Atrasada)
colnames(df)
rm(dataframe)

###################################################
# apriori

transactions = as(df, "transactions")
rules <- apriori(df, parameter=list(supp = 0.1, conf = 0.2, minlen=2, maxlen= 10, target = "rules"), 
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

