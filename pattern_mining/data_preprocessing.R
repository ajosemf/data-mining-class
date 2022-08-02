

library("tidyr")
library("tidyverse")
library("lubridate")
library(dplyr)
library(tidyr)
library(caret)
# at Local

load("./data/bfd.rda")
colnames(bfd)


#################################################################
#Data Cleaning 

#################################################
#Voos cancelados
# Os voos cancelados não são alvo de nossa análise, somente os que tiveram atraso
#contado voos cancelados
count_voos_canceled = sum((bfd %>% filter(situation_type == "CANCELADO")%>%count(flight_id))$n)
print(count_voos_canceled)
#retirando voos cancelados trazendo só os realizados
bfd = bfd %>% filter(situation_type == "REALIZADO")
sprintf("Foram retiradas %d linhas que representavam voos cancelados",count_voos_canceled)

##################################################
# arrival_ceiling analisando o  apesar dos outliers eu acho que se for metro pode existir, acho que vale referenciar

##################################################
# arrival_humidity (Percentage of relative humidity in the destination airport) acredito que não deveria ser maior igual a 100% , pois 100% é o ponto de saturação da água no ar acho q vale referenciar
#atribui a variavel por conta de possiveis mudanças
humidity_limit = 100 
#contar voos que passam o limite
count_voos_arrival_humidity_over =  sum((bfd %>% filter(arrival_humidity > humidity_limit)%>%count(flight_id))$n)
print(count_voos_arrival_humidity_over)
#Filtrando o bfd por um valor de umidade máximo
bfd = bfd %>% filter(arrival_humidity <= humidity_limit)
sprintf("Foram retiradas %d linhas que representavam valores de umidade mais altos que 100",count_voos_arrival_humidity_over)

##################################################
# depart_humidity ( Percentage of relative humidity in the airport of origin) acredito que não deveria ser maior igual a 100% , pois 100% é o ponto de saturação da água no ar acho q vale referenciar
#atribui a variavel por conta de possiveis mudanças
humidity_limit = 100 
#contar voos que passam o limite
count_voos_depart_humidity_over =  sum((bfd %>% filter(depart_humidity > humidity_limit)%>%count(flight_id))$n)
print(count_voos_depart_humidity_over)
#Filtrando o bfd por um valor de umidade máximo
bfd = bfd %>% filter(depart_humidity <= humidity_limit)
sprintf("Foram retiradas %d linhas que representavam valores de umidade mais altos que 100",count_voos_depart_humidity_over)

##################################################
# real_duration (Difference in minutes between real departure and arrival datetime ) acredito que não deveria ser pelo menos a nível do Brasíl 45 min 13H ou 780 min  , pois 100% é o ponto de saturação da água no ar acho q vale referenciar
#atribui a variavel por conta de possiveis mudanças
limit_min = 45
limit_max = 780
#contar voos que passam o limite
count_voos_real_duration_out =  sum((bfd %>% filter(real_duration < 45 | real_duration > 780)%>%count(flight_id))$n)
print(count_voos_real_duration_out)
#Filtrando o bfd por um valor de umidade máximo
bfd = bfd %>% filter(real_duration >= 45 & real_duration <= 780)
sprintf("Foram retiradas %d linhas que representavam valores de real duração além de possibilidades normais, que seria entre 45 e 780",count_voos_real_duration_out)

##################################################
# departure_delay (Difference in minutes between expected and real departure datetime; ) Pelo artigo do trabalho : Finally, the regulation of ANAC prohibits delays higher than 24 hours .
#atribui a variavel por conta de possiveis mudanças

#contar voos que passam o limite
count_voos_departure_delay_out =  sum((bfd %>% filter(departure_delay< -1440 | real_duration > 1440)%>%count(flight_id))$n)
print(count_voos_departure_delay_out)
#Filtrando o bfd por um valor de umidade máximo
bfd = bfd %>% filter(real_duration >= -1440 & real_duration <= 1440)
sprintf("Foram retiradas %d linhas que representavam valores de real duração além de possibilidades normais, que seria entre 45 e 780",count_voos_real_duration_out)



#Remoção de colunas 
bfd$airline_name = NULL
bfd$origin_name = NULL
bfd$origin_country = NULL
bfd$destination_name = NULL
bfd$destination_country = NULL
bfd$depart_cloudiness = NULL
bfd$justification_description = NULL
bfd$justification_code = NULL
bfd$depart_pressure = NULL
bfd$arrival_pressure = NULL
                       


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



dataframe = dataframe %>% mutate(Partida_Atrasada = case_when(departure_delay > 0 ~ '1',departure_delay <=0 ~ '0'))
dataframe = dataframe %>% mutate(Chegada_Atrasada = case_when(arrival_delay > 0 ~ '1',arrival_delay <=0 ~ '0'))
                    
#dataframe = dataframe %>% mutate(Partida_Atrasada = as.logical(case_when(departure_delay > 0 ~ 'TRUE',departure_delay <=0 ~ '')))
#dataframe = dataframe %>% mutate(Chegada_Atrasada = as.locical(case_when(arrival_delay > 0 ~ '1',arrival_delay <=0 ~ '0')))





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
sum(is.na(dataframe$arrival_cloudiness))

#PREDIÇÃO DEPART CEILING
df_treino_DC <- trainDataframe[,c("depart_ceiling","depart_humidity","depart_temperature","depart_visibility")]
modelo_NA <- train(depart_ceiling ~ .,data=df_treino_DC, method="rpart", trControl = trainControl(method="cv", number= 5))
dataframe$depart_ceiling[is.na(dataframe$depart_ceiling)] <- predict(modelo_NA, dataframe[is.na(dataframe$depart_ceiling),])
sum(is.na(dataframe$depart_ceiling))


#DS_ARRIVAL_WIND_DIRECTION NOT INFORMED RENAME TO NO WIND
dataframe$ds_arrival_wind_direction = as.character(dataframe$ds_arrival_wind_direction)
dataframe$ds_arrival_wind_direction[dataframe$ds_arrival_wind_direction =="Not Informed"] <- "NO WIND"
dataframe$ds_arrival_wind_direction = as.factor(dataframe$ds_arrival_wind_direction)

#DS_ARRIVAL_WIND_SPEED NOT INFORMED RENAME TO NO WIND
dataframe$ds_arrival_wind_speed = as.character(dataframe$ds_arrival_wind_speed)
dataframe$ds_arrival_wind_speed[dataframe$ds_arrival_wind_speed =="Not Informed"] <- "NO WIND"
dataframe$ds_arrival_wind_speed = as.factor(dataframe$ds_arrival_wind_speed)

#DS_DEPART_WIND_DIRECTION NOT INFORMED RENAME TO NO WIND
dataframe$ds_depart_wind_direction = as.character(dataframe$ds_depart_wind_direction)
dataframe$ds_depart_wind_direction[dataframe$ds_depart_wind_direction =="Not Informed"] <- "NO WIND"
dataframe$ds_depart_wind_direction = as.factor(dataframe$ds_depart_wind_direction)

#DS_DEPART_WIND_SPEED NOT INFORMED RENAME TO NO WIND
dataframe$ds_depart_wind_speed = as.character(dataframe$ds_depart_wind_speed)
dataframe$ds_depart_wind_speed[dataframe$ds_depart_wind_speed =="Not Informed"] <- "NO WIND"
dataframe$ds_depart_wind_speed = as.factor(dataframe$ds_depart_wind_speed)


#EXPECTED_DEPART_DATE TO DATE FORMAT
dataframe$expected_depart_date = ymd(dataframe$expected_depart_date)

#EXPECTED_REAL_DEPART_DATE TO DATE FORMAT
dataframe$real_depart_date = ymd(dataframe$real_depart_date)

#EXPECTED_ARRIVAL_DATE TO DATE FORMAT
dataframe$expected_arrival_date = ymd(dataframe$expected_arrival_date)

#EXPECTED_REAL_ARRIVAL_DATE TO DATE FORMAT
dataframe$real_arrival_date = ymd(dataframe$real_depart_date)


save(dataframe, file='my_data.rda')


sum(bfd$justification_code[bfd$justification_code == "N/A"])

Justcode = sum((bfd %>% filter(justification_code == "N/A")%>%count(justification_code))$n)


# ensure results are repeatable
set.seed(7)
# load the library
install.packages("mlbench")
library(mlbench)
library(caret)

dataframe$Chegada_Atrasada = as.logical(dataframe$Chegada_Atrasada)
dataframe$Partida_Atrasada = as.logical(dataframe$Partida_Atrasada)


# load the dataset
data(PimaIndiansDiabetes)
# prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
model <- train(dataframe$Partida_Atrasada~., data=dataframe, method="lvq", preProcess="scale", trControl=control)
# estimate variable importance
importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)

class(dataframe$Chegada_Atrasada)

