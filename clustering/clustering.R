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

