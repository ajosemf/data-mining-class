library(dplyr)


###################################################
# load data
load("data/mobility-2014-05-02.RData")
colnames(data)
nrow(data)  # 5.533.132
head(data)

#--------------------------------------------------
#--------------------------------------------------
#            EXPLORATORY ANALYSIS
#--------------------------------------------------
#--------------------------------------------------

# "lat",  "long", "date", "hour", "velocidade"

# missing data
which(is.na(data))
which(is.na(data$lat))
which(is.na(data$long))
which(is.na(data$date))
which(is.na(data$hour))
which(is.na(data$velocidade))

# data distribution
summary(data$lat)  # -23.07 a -22.70 (41 km approximately)
hist(data$lat)

summary(data$long) # -43.73 a -42.98 (77 km approximately)
hist(data$long)

summary(data$date)  # 2014-05-01 to 2014-05-02
dates = dplyr::select(data, date)
dates = dplyr::mutate(dates, date = as.Date(dates$date))
dplyr::count(dates, date)  # 2014-05-01: 7.139, 2014-05-02: 5.525.992

hist(data$hour)

summary(data$velocidade)  # 0.0 to 108.0 km/h
hist(data$velocidade)
boxplot(data$velocidade)
tmp = dplyr::filter(data, velocidade <= 5)
nrow(tmp) / nrow(data)  # 54% with velocidade <= 5 km/h


#--------------------------------------------------
#--------------------------------------------------
#       FEATURE SELECTION AND PREPROCESSING
#--------------------------------------------------
#--------------------------------------------------

# removes records associated to '2014-05-01'
data$date = as.Date(data$date)
data = dplyr::filter(data, date == '2014-05-02')

# select features
data = dplyr::select(data, c(lat, long, hour, velocidade))

# stage data
save(data, file="data/clustering.rda")


#--------------------------------------------------
#--------------------------------------------------
#         CLUSTERING / OUTLIER DETECTION
#--------------------------------------------------
#--------------------------------------------------


# install.packages("factoextra")
library(dplyr)
library(factoextra)
source("utils/kmod.R")

# load data
load("data/clustering.rda")

# found best k
set.seed(123)
data.sample = sample_n(data, 10000)
fviz_nbclust(data.sample, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)

# kmodR with sample
data.sample = scale(data.sample)
set.seed(123)
cl <- kmod(data.sample, 4, 10)
data[cl$L_index, ]  # outliers

# kmodR with all data
data.scaled = scale(data)
Sys.time()
cl <- kmod(data.scaled, 4, 10)
Sys.time()
data[cl$L_index, ]  # outliers
# lat     long hour velocidade
# 1645475 -22.70337 -43.6932    2       81.3
# 1660133 -22.70337 -43.6932    2       81.3
# 1955159 -22.70337 -43.6932    2       81.3
# 1971041 -22.70337 -43.6932    3       81.3
# 1978165 -22.70337 -43.6932    3       81.3
# 1985284 -22.70337 -43.6932    3       81.3
# 2285378 -22.70337 -43.6932    3       81.3
# 2292627 -22.70337 -43.6932    3       81.3
# 2299874 -22.70337 -43.6932    3       81.3
# 2307070 -22.70337 -43.6932    3       81.3

# stage outliers
idxs = cl$L_index
save(idxs, file = "data/clustering_outliers_idx.rda")
