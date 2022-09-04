library(dplyr)
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myBasic.R")
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myPreprocessing.R")
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myClassification.R")
library(rpart)


library(caret)
library(tidyverse)

#Load Preprocessed data
load("../data/preprocess.rda")



#--------------------------------------------------------------#
#--------------------------------------------------------------#
#-------------------------DEPART PREDICTION--------------------#
#--------------------------------------------------------------#
#--------------------------------------------------------------#


#----------------------MAJORITY CLASS DEPART-------------------#

set.seed(1)
sr <- sample_random()
sr <- train_test(sr, dataframe)
depart_train = sr$train
depart_test = sr$test

tbl <- rbind(table(dataframe[,"Partida_Atrasada"]), 
             table(depart_train[,"Partida_Atrasada"]), 
             table(depart_test[,"Partida_Atrasada"]))
rownames(tbl) <- c("dataset", "training", "test")
head(tbl)

train_test <- function(model, depart_train, depart_test) {
  print(class(model)[1])
  
  model <- fit(model, depart_train)
  train_prediction <- predict(model, depart_train)
  
  depart_train_predictand = decodeClassLabels(depart_train[,"Partida_Atrasada"])
  train_eval <- evaluation.classification(depart_train_predictand, train_prediction)
  print(train_eval$metrics)
  plot(roc_curve(train_eval))
  
  test_prediction <- predict(model, depart_test)
  
  depart_test_predictand = decodeClassLabels(depart_test[,"Partida_atrasada"])
  test_eval <- evaluation.classification(depart_test_predictand, test_prediction)
  print(test_eval$metrics)
  plot(roc_curve(test_eval))
}

train_test(classification_majority("Chegada_Atrasada"), depart_train, depart_test)

classification_majority


# ---------------- DECISION TREE ----------------


#Feature Selection


DepartPrediction = dataframe[ , c('flight_id',
               'airline_icao',
               'origin_icao',
                'depart_temperature',
               'depart_dew_point',
               'depart_humidity',
               'depart_visibility',
               'depart_ceiling',
               'destination_icao',
               'arrival_temperature',
               'arrival_dew_point',
               'arrival_humidity',
               'arrival_visibility',
               'arrival_cloudiness',
               'arrival_ceiling',
               'Partida_Atrasada'
)]




DepartPrediction$Partida_Atrasada = as.numeric(DepartPrediction$Partida_Atrasada)


DepartPrediction$Partida_Atrasada <- ifelse(DepartPrediction$Partida_Atrasada ==1, "YES", "NO")

#TRAIN AND TEST SPLIT 

set.seed(123)

train.samples = DepartPrediction$Partida_Atrasada %>% 
  createDataPartition(p = .8, list = FALSE)

depart_train = DepartPrediction[train.samples,]
depart_teste = DepartPrediction[-train.samples, ]

str(DepartPrediction)




DT_Model = rpart(Partida_Atrasada~.,data=depart_train)
DT_Model

plot(DT_Model, margin = 0.1)
text(DT_Model, use.n = TRUE, pretty=TRUE, cex=0.8)

#DECISION TREE Prediction 
pred_depart = predict(DT_Model, newdata=depart_teste, type="class")
pred_depart
confusionMatrix(table(pred_depart, depart_teste$Partida_Atrasada),mode="everything")


#-------------------- KNN --------------------

#NORMALIZATION
normalize = function(x){
  return((x-min(x))/(max(x)-min(x)))
}


#Feature Selection

KNN_DepartPrediction = dataframe[ , c(
  'depart_temperature',
  'depart_dew_point',
  'depart_humidity',
  'depart_visibility',
  'depart_ceiling',
  'arrival_temperature',
  'arrival_dew_point',
  'arrival_humidity',
  'arrival_visibility',
  'arrival_ceiling',
  'Partida_Atrasada'
)]
KNN_DepartPrediction$Partida_Atrasada = as.numeric(KNN_DepartPrediction$Partida_Atrasada)

KNN_DepartPrediction.normalized = as.data.frame(lapply(KNN_DepartPrediction,normalize))

#TRAIN AND TEST SPLIT 
set.seed(123)

KNN_train.samples = KNN_DepartPrediction.normalized$Partida_Atrasada %>% 
  createDataPartition(p = .8, list = FALSE)

KNN_depart_train = KNN_DepartPrediction.normalized[KNN_train.samples,]
KNN_depart_teste = KNN_DepartPrediction.normalized[-KNN_train.samples, ]
str(KNN_DepartPrediction.normalized)


library(class)
pred_knn = knn(KNN_depart_train[,-11], KNN_depart_teste[,-11], cl = KNN_depart_train[,11], k=6)

confusionMatrix(table(KNN_depart_teste[,11], pred_knn),mode="everything")

colSums(is.na(KNN_depart_train))
colSums(is.na(KNN_depart_teste))
colSums(is.na(DepartPrediction))




# ------------- SVM --------------


SVM_DepartPrediction = dataframe[ , c(
                                  'airline_icao',
                                  'origin_icao',
                                  'depart_temperature',
                                  'depart_dew_point',
                                  'depart_humidity',
                                  'depart_visibility',
                                  'depart_ceiling',
                                  'destination_icao',
                                  'arrival_temperature',
                                  'arrival_dew_point',
                                  'arrival_humidity',
                                  'arrival_visibility',
                                  'arrival_cloudiness',
                                  'arrival_ceiling',
                                  'Partida_Atrasada'
)]



SVM_DepartPrediction$Partida_Atrasada <- ifelse(SVM_DepartPrediction$Partida_Atrasada ==1, "YES", "NO")

# TESTE SPLIT 
set.seed(123)

SVM_train.samples = SVM_DepartPrediction$Partida_Atrasada %>% 
  createDataPartition(p = .8, list = FALSE)

SVM_depart_train = SVM_DepartPrediction[SVM_train.samples,]
SVM_depart_teste = SVM_DepartPrediction[-SVM_train.samples, ]

str(SVM_DepartPrediction)

trctrl = trainControl(method = "repeatdcv", number = 5, repeats = 3)

library(kernlab)
SVM_Model = train(SVM_depart_train$Partida_Atrasa~.,data=SVM_depart_train, method="svmLinear")
SVM_Model

SVM_pred = predict(SVM_Model, newdata=SVM_depart_teste)
SVM_pred

confusionMatrix(table( SVM_depart_train$Partida_Atrasa, SVM_pred))


# --------------- Naive Bayes ------------------

NB_DepartPrediction = dataframe[ , c(
  'depart_temperature',
  'depart_dew_point',
  'depart_humidity',
  'depart_visibility',
  'depart_ceiling',
  'arrival_temperature',
  'arrival_dew_point',
  'arrival_humidity',
  'arrival_visibility',
  'arrival_ceiling',
  'Partida_Atrasada'
)]
NB_DepartPrediction$Partida_Atrasada = as.numeric(NB_DepartPrediction$Partida_Atrasada)


set.seed(123)

NB_train.samples = NB_DepartPrediction$Partida_Atrasada %>% 
  createDataPartition(p = .8, list = FALSE)

NB_depart_train = NB_DepartPrediction[NB_train.samples,]
NB_depart_teste = NB_DepartPrediction[-NB_train.samples, ]

str(NB_DepartPrediction)

NB_Model = naiveBayes(NB_depart_train$Partida_Atrasada~., data=NB_depart_train)
NB_Model

NB_Pred = predict(NB_Model, NB_depart_teste)
NB_Pred

confusionMatrix(table(NB_depart_teste$Partida_Atrasada, NB_Pred),mode="everything")

#--------------------------------------------------------------#
#--------------------------------------------------------------#
#------------------------ARRIVAL PREDICTION--------------------#
#--------------------------------------------------------------#
#--------------------------------------------------------------#


#---------------------MAJORITY CLASS ARRIVAL-------------------#

set.seed(1)
sr <- sample_random()
sr <- train_test(sr, dataframe)
arrival_train = sr$train
arrival_test = sr$test

tbl <- rbind(table(dataframe[,"Chegada_Atrasada"]), 
             table(arrival_train[,"Chegada_Atrasada"]), 
             table(arrival_test[,"Chegada_Atrasada"]))
rownames(tbl) <- c("dataset", "training", "test")
head(tbl)

train_test <- function(model, arrival_train, arrival_test) {
  print(class(model)[1])
  
  model <- fit(model, arrival_train)
  train_prediction <- predict(model, arrival_train)
  
  arrival_train_predictand = decodeClassLabels(arrival_train[,"Chegada_Atrasada"])
  train_eval <- evaluation.classification(arrival_train_predictand, train_prediction)
  print(train_eval$metrics)
  plot(roc_curve(train_eval))
  
  test_prediction <- predict(model, depart_test)
  
  arrival_test_predictand = decodeClassLabels(arrival_test[,"Chegada_Atrasada"])
  test_eval <- evaluation.classification(arrival_test_predictand, test_prediction)
  print(test_eval$metrics)
  plot(roc_curve(test_eval))
}

train_test(classification_majority("Chegada_Atrasada"), arrivalt_train, arrival_test)

classification_majority



# ---------------- DECISION TREE ----------------


#Feature Selection


DT_ArrivalPrediction = dataframe[ , c(
                                  'origin_icao',
                                  'depart_temperature',
                                  'depart_dew_point',
                                  'depart_humidity',
                                  'depart_visibility',
                                  'depart_ceiling',
                                  'destination_icao',
                                  'arrival_temperature',
                                  'arrival_dew_point',
                                  'arrival_humidity',
                                  'arrival_visibility',
                                  'arrival_cloudiness',
                                  'arrival_ceiling',
                                  'departure_delay',
                                  'Chegada_Atrasada'
)]




DT_ArrivalPrediction$Chegada_Atrasada = as.numeric(DT_ArrivalPrediction$Chegada_Atrasada)

DT_ArrivalPrediction$Chegada_Atrasada <- ifelse(DT_ArrivalPrediction$Chegada_Atrasada ==1, "YES", "NO")

#TRAIN AND TEST SPLIT 

set.seed(123)

train.samples = DT_ArrivalPrediction$Chegada_Atrasada %>% 
  createDataPartition(p = .8, list = FALSE)

arrival_train = DT_ArrivalPrediction[train.samples,]
arrival_teste = DT_ArrivalPrediction[-train.samples, ]

str(DT_ArrivalPrediction)




library(rpart)
DT_Model = rpart(Chegada_Atrasada~.,data=arrival_train)
DT_Model

plot(DT_Model, margin = 0.1)
text(DT_Model, use.n = TRUE, pretty=TRUE, cex=0.8)

#DECISION TREE Prediction 
pred_arrival = predict(DT_Model, newdata=arrival_teste, type="class")
pred_arrival
confusionMatrix(table(pred_arrival, arrival_teste$Chegada_Atrasada),mode="everything")


#-------------------- KNN --------------------

#NORMALIZATION
normalize = function(x){
  return((x-min(x))/(max(x)-min(x)))
}


#Feature Selection

KNN_ArrivalPrediction = dataframe[ , c(
  'depart_temperature',
  'depart_dew_point',
  'depart_humidity',
  'depart_visibility',
  'depart_ceiling',
  'arrival_temperature',
  'arrival_dew_point',
  'arrival_humidity',
  'arrival_visibility',
  'arrival_ceiling',
  'departure_delay',
  'Chegada_Atrasada'
)]

KNN_ArrivalPrediction$Chegada_Atrasada = as.numeric(KNN_ArrivalPrediction$Chegada_Atrasada)

KNN_ArrivalPrediction.normalized = as.data.frame(lapply(KNN_ArrivalPrediction,normalize))

#TRAIN AND TEST SPLIT 
set.seed(123)

KNN_train.samples = KNN_ArrivalPrediction.normalized$Chegada_Atrasada %>% 
  createDataPartition(p = .8, list = FALSE)

KNN_arrival_train = KNN_ArrivalPrediction.normalized[KNN_train.samples,]
KNN_arrival_teste = KNN_ArrivalPrediction.normalized[-KNN_train.samples, ]
str(KNN_ArrivalPrediction.normalized)


library(class)
pred_knn = knn(KNN_arrival_train[,-12], KNN_arrival_teste[,-12], cl = KNN_arrival_train[,12], k=6)

confusionMatrix(table(KNN_arrival_teste[,12], pred_knn),mode="everything")

colSums(is.na(KNN_depart_train))
colSums(is.na(KNN_depart_teste))
colSums(is.na(DepartPrediction))


# --------------- Naive Bayes ------------------

NB_ArrivalPrediction = dataframe[ , c(
  'depart_temperature',
  'depart_dew_point',
  'depart_humidity',
  'depart_visibility',
  'depart_ceiling',
  'arrival_temperature',
  'arrival_dew_point',
  'arrival_humidity',
  'arrival_visibility',
  'arrival_ceiling',
  'departure_delay',
  'Chegada_Atrasada'
)]

NB_ArrivalPrediction$Chegada_Atrasada = as.numeric(NB_ArrivalPrediction$Chegada_Atrasada)


set.seed(123)
NB_train.samples = NB_ArrivalPrediction$Chegada_Atrasada %>% 
  createDataPartition(p = .8, list = FALSE)

NB_arrival_train = NB_ArrivalPrediction[NB_train.samples,]
NB_arrival_teste = NB_ArrivalPrediction[-NB_train.samples, ]

str(NB_ArrivalPrediction)

library(e1071)

NB_Model = naiveBayes(Chegada_Atrasada~., data=NB_arrival_train)
NB_Model

NB_Pred = predict(NB_Model, NB_arrival_teste)
NB_Pred

confusionMatrix(table(NB_arrival_teste$Chegada_Atrasada, NB_Pred),mode="everything")
help("confusionMatrix")

