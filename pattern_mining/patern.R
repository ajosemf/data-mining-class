source("utils/myGraphic.R")

loadlibrary("arules")
library(dplyr)

#--------------------------------------------------
#--------------------------------------------------
#                PREPROCESSING
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


# expected duration column
hist(dataframe$expected_duration)
dataframe = dataframe %>% mutate(ds_expected_duration = case_when(
  expected_duration < 30                              ~ "[,30)",
  expected_duration >= 30 & expected_duration < 60    ~ "[30,60)",
  expected_duration >= 60 & expected_duration < 90    ~ "[60,90)",
  expected_duration >= 90 & expected_duration < 120   ~ "[90,120)",
  expected_duration >= 120 & expected_duration < 150  ~ "[120,150)",
  expected_duration >= 150 & expected_duration < 180  ~ "[150,180)",
  expected_duration >= 180 & expected_duration < 210  ~ "[180,210)",
  expected_duration >= 210                            ~ "[210,]"
))
dataframe$ds_expected_duration = as.factor(dataframe$ds_expected_duration)
dataframe$ds_expected_duration <- factor(dataframe$ds_expected_duration, 
                                         levels=c("[,30)", "[30,60)", "[60,90)", "[90,120)", "[120,150)", "[150,180)", "[180,210)", "[210,]"))
plot(dataframe$ds_expected_duration)
dataframe$expected_duration = NULL


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


# humidity columns
hist(dataframe$depart_humidity)
hist(dataframe$arrival_humidity)

set.seed(1)
dataframe$ds_depart_humidity = discretize(dataframe$depart_humidity,
                                           method = "cluster", 
                                           breaks = 5,
                                           ordered_result=TRUE)
set.seed(1)
dataframe$ds_arrival_humidity = discretize(dataframe$arrival_humidity,
                                           method = "cluster", 
                                           breaks = 5,
                                           ordered_result=TRUE)
plot(dataframe$ds_depart_humidity)
plot(dataframe$ds_arrival_humidity)
dataframe$depart_humidity = NULL
dataframe$arrival_humidity = NULL


# visibility columns
hist(dataframe$depart_visibility)
hist(dataframe$arrival_visibility)

set.seed(1)
dataframe$ds_depart_visibility = discretize(dataframe$depart_visibility,
                                            method = "cluster", 
                                            breaks = 5,
                                            ordered_result=TRUE)
set.seed(1)
dataframe$ds_arrival_visibility = discretize(dataframe$arrival_visibility,
                                             method = "cluster", 
                                             breaks = 5,
                                             ordered_result=TRUE)
plot(dataframe$ds_depart_visibility)
plot(dataframe$ds_arrival_visibility)
dataframe$depart_visibility = NULL
dataframe$arrival_visibility = NULL


# ceiling columns
hist(dataframe$depart_ceiling)
hist(dataframe$arrival_ceiling)

set.seed(1)
dataframe$ds_depart_ceiling = discretize(dataframe$depart_ceiling,
                                         method = "cluster", 
                                         breaks = 5,
                                         ordered_result=TRUE)
set.seed(1)
dataframe$ds_arrival_ceiling = discretize(dataframe$arrival_ceiling,
                                          method = "cluster", 
                                          breaks = 5,
                                          ordered_result=TRUE)
plot(dataframe$ds_depart_ceiling)
plot(dataframe$ds_arrival_ceiling)
dataframe$depart_ceiling = NULL
dataframe$arrival_ceiling = NULL


###################################################
# rename
names(dataframe)[names(dataframe) == 'arrival_cloudiness'] = 'ds_arrival_cloudiness'


###################################################
# stage data
save(dataframe, file="data/discretized.rda")


#--------------------------------------------------
#--------------------------------------------------
#                DEPARTURE
#--------------------------------------------------
#--------------------------------------------------

library(arules)
library(dplyr)
load("data/discretized.rda")


###################################################
# get depart columns
df = dataframe %>% dplyr::select(airline_icao,
                                 linetype_code,
                                 ds_expected_duration,
                                 starts_with("expected_depart"),
                                 starts_with("ds_depart"),
                                 Partida_Atrasada)
df$Partida_Atrasada = as.factor(df$Partida_Atrasada)
colnames(df)


###################################################
# Partida Atrasada = 1

transactions = as(df, "transactions")
rules <- apriori(df, parameter=list(supp = 0.1, conf = 0.4, minlen=2, maxlen= 10, target = "rules"),
                 appearance=list(rhs = c("Partida_Atrasada=1"), default="lhs"), control=NULL)
# inspect(rules)
rules <- rules[!is.redundant(rules)]  # remove redundant rules
measures = c("support", "confidence", "lift", "chiSquared", 'kulc', 'imbalance', "count")
imrules <- interestMeasure(rules, measure = measures, transactions = transactions)  # process interesting measures
rules = as(rules, "data.frame")  # merge rules and interesting measures on dataframe
rules$chiSquared = imrules$chiSquared
rules$kulc = imrules$kulc
rules$imbalance = imrules$imbalance
rules


###################################################
# Partida Atrasada = 0

transactions = as(df, "transactions")
rules <- apriori(df, parameter=list(supp = 0.1, conf = 0.8, minlen=2, maxlen= 10, target = "rules"),
                 appearance=list(rhs = c("Partida_Atrasada=0"), default="lhs"), control=NULL)
# inspect(rules)
rules <- rules[!is.redundant(rules)]  # remove redundant rules
measures = c("support", "confidence", "lift", "chiSquared", 'kulc', 'imbalance', "count")
imrules <- interestMeasure(rules, measure = measures, transactions = transactions)  # process interesting measures
rules = as(rules, "data.frame")  # merge rules and interesting measures on dataframe
rules$chiSquared = imrules$chiSquared
rules$kulc = imrules$kulc
rules$imbalance = imrules$imbalance
rules


#--------------------------------------------------
#--------------------------------------------------
#                ARRIVAL
#--------------------------------------------------
#--------------------------------------------------


###################################################
# get arrival columns
df = dataframe %>% dplyr::select(airline_icao,
                                 linetype_code,
                                 ds_expected_duration,
                                 starts_with("expected_arrival"),
                                 starts_with("ds_arrival"),
                                 Partida_Atrasada,
                                 Chegada_Atrasada)
df$Partida_Atrasada = as.factor(df$Partida_Atrasada)
df$Chegada_Atrasada = as.factor(df$Chegada_Atrasada)
colnames(df)


###################################################
# Chegada Atrasada = 1

transactions = as(df, "transactions")
rules <- apriori(df, parameter=list(supp = 0.1, conf = 0.6, minlen=2, maxlen= 10, target = "rules"),
                 appearance=list(rhs = c("Chegada_Atrasada=1"), default="lhs"), control=NULL)
# inspect(rules)
rules <- rules[!is.redundant(rules)]  # remove redundant rules
measures = c("support", "confidence", "lift", "chiSquared", 'kulc', 'imbalance', "count")
imrules <- interestMeasure(rules, measure = measures, transactions = transactions)  # process interesting measures
rules = as(rules, "data.frame")  # merge rules and interesting measures on dataframe
rules$chiSquared = imrules$chiSquared
rules$kulc = imrules$kulc
rules$imbalance = imrules$imbalance
rules


###################################################
# Chegada Atrasada = 0

transactions = as(df, "transactions")
rules <- apriori(df, parameter=list(supp = 0.3, conf = 0.8, minlen=2, maxlen= 10, target = "rules"),
                 appearance=list(rhs = c("Chegada_Atrasada=0"), default="lhs"), control=NULL)
# inspect(rules)
rules <- rules[!is.redundant(rules)]  # remove redundant rules
measures = c("support", "confidence", "lift", "chiSquared", 'kulc', 'imbalance', "count")
imrules <- interestMeasure(rules, measure = measures, transactions = transactions)  # process interesting measures
rules = as(rules, "data.frame")  # merge rules and interesting measures on dataframe
rules$chiSquared = imrules$chiSquared
rules$kulc = imrules$kulc
rules$imbalance = imrules$imbalance
rules


###################################################
# sort 

# by kulc, imbalance, lift, support, confidence 
rules[order(-rules$kulc, rules$imbalance, -rules$lift, -rules$support, -rules$confidence),]
