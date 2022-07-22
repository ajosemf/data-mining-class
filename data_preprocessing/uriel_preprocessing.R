#Load dataset
load("d:/bfd.rda")


#Remoção de colunas 
bfd$airline_name = NULL
bfd$origin_name = NULL
bfd$origin_country = NULL
bfd$destination_name = NULL
bfd$destination_country = NULL
bfd$depart_cloudiness = NULL
bfd$justification_description = NULL
bfd$justification_code = NULL
                       
library("tidyr")
library("tidyverse")

#DepartVis = bfd %>% select(depart_visibility)
trainDataframe = bfd %>% drop_na(depart_visibility)  
trainDataframe = trainDataframe %>% drop_na(depart_wind_direction) 
trainDataframe = trainDataframe %>% drop_na(arrival_visibility) 
trainDataframe = trainDataframe %>% drop_na(arrival_wind_direction)
trainDataframe = trainDataframe %>% drop_na(depart_ceiling)
trainDataframe = trainDataframe %>% drop_na(arrival_cloudiness)
trainDataframe = trainDataframe %>% drop_na(arrival_ceiling) 

dataframe = bfd %>% drop_na(depart_visibility)  
dataframe = dataframe %>% drop_na(depart_wind_direction) 
dataframe = dataframe %>% drop_na(arrival_wind_direction)





origens = dataframe %>% select()
obj = lapply(dataframe, mean, na.rm=TRUE)

aeroportos = dataframe %>% distinct(origin_icao)
mean(dataframe[dataframe$origin_icao, 'arrival_ceiling'],na.rm=TRUE)


MediaArrivalCeiling = lapply(split(dataframe$arrival_ceiling, dataframe$destination_icao), mean, na.rm=TRUE) 
MediaDepartCeiling = lapply(split(dataframe$depart_ceiling, dataframe$origin_icao), mean, na.rm=TRUE) 



library(dplyr)
library(tidyr)
dataframe2 = dataframe %>% group_by(destination_icao) %>% mutate_at(vars(arrival_ceiling), ~replace_na(., mean(., na.rm = TRUE)))

dataframe3 = dataframe2 %>% group_by(origin_icao) %>% mutate_at(vars(depart_ceiling), ~replace_na(., mean(., na.rm = TRUE)))


   


dataframe$arrival_visibility[dataframe$arrival_visibility == "NA" & dataframe$arrival_cloudiness == "NA"] <- "6.21"
                    
dataframe = dataframe %>% mutate(Partida_Atrasada = case_when(departure_delay > 0 ~ '1',departure_delay <=0 ~ '0'))
dataframe = dataframe %>% mutate(Chegada_Atrasada = case_when(arrival_delay > 0 ~ '1',arrival_delay <=0 ~ '0'))


library(caret)


#PREDIÇÃO ARRIVAL VISIBILITY
df_treino_AV <- trainDataframe[,c("arrival_cloudiness","arrival_ceiling","arrival_humidity","arrival_temperature","arrival_visibility")]
modelo_NA <- train(arrival_visibility ~ .,data=df_treino_AV, method="rpart", trControl = trainControl(method="cv", number= 5))
dataframe$arrival_visibility[is.na(dataframe$arrival_visibility)] <- predict(modelo_NA, dataframe[is.na(dataframe$arrival_visibility),])
sum(is.na(dataframe$arrival_visibility))


#PREDIÇÃO ARRIVAL CEILIING
df_treino_AC <- trainDataframe[,c("arrival_cloudiness","arrival_ceiling","arrival_humidity","arrival_temperature","arrival_visibility")]
modelo_NA <- train(arrival_ceiling ~ .,data=df_treino_AC, method="rpart", trControl = trainControl(method="cv", number= 5))
dataframe$arrival_ceiling[is.na(dataframe$arrival_ceiling)] <- predict(modelo_NA, dataframe[is.na(dataframe$arrival_ceiling),])
sum(is.na(dataframe$arrival_ceiling))

#PREDIÇÃO ARRIVAL CLOUDINESS
dataframe_ACL= dataframe %>% drop_na(arrival_cloudiness)
dataframe_ACL= dataframe_ACL %>% drop_na(arrival_ceiling)
dataframe_ACL= dataframe_ACL %>% drop_na(arrival_humidity)
dataframe_ACL= dataframe_ACL %>% drop_na(arrival_temperature)
dataframe_ACL= dataframe_ACL %>% drop_na(arrival_visibility)

df_treino_ACL <-dataframe_ACL[,c("arrival_cloudiness","arrival_ceiling","arrival_humidity","arrival_temperature","arrival_visibility")]
modelo_NA <- train(arrival_cloudiness ~ .,data=df_treino_ACL, method="rpart", trControl = trainControl(method="cv", number= 5))
dataframe$arrival_cloudiness[is.na(dataframe$arrival_cloudiness)] <- predict(modelo_NA, dataframe[is.na(dataframe$arrival_cloudiness),])
sum(is.na(dataframe$arrival_ceiling))

#PREDIÇÃO DEPART CEILING
df_treino_DC <- trainDataframe[,c("depart_ceiling","depart_humidity","depart_temperature","depart_visibility")]
modelo_NA <- train(depart_ceiling ~ .,data=df_treino_DC, method="rpart", trControl = trainControl(method="cv", number= 5))
dataframe$depart_ceiling[is.na(dataframe$depart_ceiling)] <- predict(modelo_NA, dataframe[is.na(dataframe$depart_ceiling),])
sum(is.na(dataframe$depart_ceiling))

## Import library
library(DataExplorer)


## Import library
library(ExPanDaR)

## Import library
library(dataMaid)

ExPanD(dataframe)
makeDataReport(dataframe, output = "html", replace = TRUE)  








