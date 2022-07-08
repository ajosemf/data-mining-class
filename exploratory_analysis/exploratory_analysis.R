source("data-mining-class/utils/myBasic.R")
source("data-mining-class/utils/myGraphic.R")


###################################################
# config and load data
colors <- brewer.pal(11, 'Paired')
font <- theme(text = element_text(size=16))
loadlibrary("MASS")
loadlibrary("gridExtra")

load("data-mining-class/data/bfd.rda")
colnames(bfd)


###################################################
# plot and removing canceled flights
bfd_cancel = bfd %>% dplyr::filter(situation_type == "CANCELADO")
unique(bfd_cancel %>% dplyr::select(arrival_delay))
delay_cancel = bfd_cancel %>% dplyr::count(arrival_delay)
delay_cancel
delay_not_zero_cancel = delay_cancel %>% dplyr::filter(arrival_delay != 0)
delay_not_zero_cancel
sum(delay_not_zero_cancel$n)
barplot(delay_not_zero_cancel$n,
        main = "Arrival delays on canceled flights",
        xlab = "Arrival delay value",
        ylab = "# Arrival delay",
        names.arg = delay_not_zero_cancel$arrival_delay,
        col = "darkred")
bfd = bfd %>% dplyr::filter(situation_type != "CANCELADO")
bfd <- subset (bfd, select = -situation_type)
"situation_type" %in% colnames(bfd)


###################################################
# Delays analysis

grf <- plot.boxplot(bfd$arrival_delay,
                    label_x = "Arrival Delay", 
                    colors=colors[1]) + font
plot.size(6, 3)
plot(grf)

grf <- plot.hist(bfd %>% dplyr::select(arrival_delay), 
                 label_x = "Arrival Delay", 
                 color=colors[1]) + font
plot.size(4, 3)
plot(grf)

delay = bfd %>% dplyr::count(arrival_delay)
tail(delay)

tmp = bfd %>% dplyr::filter(arrival_delay == -525610)
tmp %>% dplyr::select(expected_depart_date,
                      expected_depart_hour,
                      real_depart_date, 
                      real_depart_hour, 
                      departure_delay,
                      expected_arrival_date,
                      expected_arrival_hour,
                      real_arrival_date,
                      real_arrival_hour,
                      arrival_delay)

head(delay %>% dplyr::filter(arrival_delay > 0))
tail(delay %>% dplyr::filter(arrival_delay > 0))

bfd = bfd %>% mutate(delayed = case_when(
  arrival_delay > 0 ~ 1,
  arrival_delay <= 0 ~ 0
))

delayed = bfd %>% dplyr::count(delayed)
delayed
barplot(delayed$n,
        main = "Delayed flights",
        xlab = "Delayed flight",
        ylab = "# Delayed flight",
        names.arg = c('no', 'yes'),
        col = "darkred")

(delayed %>% dplyr::filter(delayed == 0))$n / sum(delayed$n)  # 0.6896697 (NOT delayed frequency)
(delayed %>% dplyr::filter(delayed == 1))$n / sum(delayed$n)  # 0.3103303 (delayed frequency)


###################################################
# Meteorological analysis (DEPARTURE - density plots)

bfd$delayed = as.factor(bfd$delayed)

bfd = bfd %>% mutate(delayed_str = case_when(
  delayed == 0 ~ "no",
  delayed == 1 ~ "yes"
))

grfA <- plot.density.class(bfd %>% dplyr::select(delayed_str, depart_temperature), 
                           class_label="delayed_str", label_x = "temperature", color=colors[c(1,5)]) + font
grfB <- plot.density.class(bfd %>% dplyr::select(delayed_str, depart_dew_point), 
                           class_label="delayed_str", label_x = "dew point", color=colors[c(1,5)]) + font
grfC <- plot.density.class(bfd %>% dplyr::select(delayed_str, depart_humidity), 
                           class_label="delayed_str", label_x = "humidity", color=colors[c(1,5)]) + font
grfD <- plot.density.class(bfd %>% dplyr::select(delayed_str, depart_visibility), 
                           class_label="delayed_str", label_x = "visibility", color=colors[c(1,5)]) + font
grfE <- plot.density.class(bfd %>% dplyr::select(delayed_str, depart_ceiling), 
                           class_label="delayed_str", label_x = "ceiling", color=colors[c(1,5)]) + font
grfF <- plot.density.class(bfd %>% dplyr::select(delayed_str, depart_wind_speed), 
                           class_label="delayed_str", label_x = "wind_speed", color=colors[c(1,5)]) + font
plot.size(8, 24)
grid.arrange(grfA, grfB, grfC, grfD, grfE, grfF,
             ncol=3, nrow=2)


bfd %>% dplyr::count(depart_cloudiness)  # only NA
bfd %>% dplyr::count(depart_pressure)    # value 1015 on all objects


grfA <- plot.density.class(bfd %>% dplyr::select(delayed_str, ds_depart_wind_speed), 
                           class_label="delayed_str", label_x = "ds wind speed", color=colors[c(1,5)],
                           rotate_x=TRUE) + font
grfB <- plot.density.class(bfd %>% dplyr::select(delayed_str, depart_wind_direction), 
                           class_label="delayed_str", label_x = "wind direction", color=colors[c(1,5)],
                           rotate_x=TRUE) + font
grfC <- plot.density.class(bfd %>% dplyr::select(delayed_str, ds_depart_wind_direction), 
                           class_label="delayed_str", label_x = "ds wind direction", color=colors[c(1,5)],
                           rotate_x=TRUE) + font
grfD <- plot.density.class(bfd %>% dplyr::select(delayed_str, ds_depart_day_period), 
                           class_label="delayed_str", label_x = "ds day period", color=colors[c(1,5)],
                           rotate_x=TRUE) + font
plot.size(8, 8)
grid.arrange(grfA, grfB,
             ncol=2, nrow=1)

plot.size(8, 8)
grid.arrange(grfC, grfD,
             ncol=2, nrow=1)

###################################################
# Meteorological analysis (ARRIVAL - density plots)


bfd$delayed = as.factor(bfd$delayed)

bfd = bfd %>% mutate(delayed_str = case_when(
  delayed == 0 ~ "no",
  delayed == 1 ~ "yes"
))

grfA <- plot.density.class(bfd %>% dplyr::select(delayed_str, arrival_temperature), 
                           class_label="delayed_str", label_x = "temperature", color=colors[c(1,5)]) + font
grfB <- plot.density.class(bfd %>% dplyr::select(delayed_str, arrival_dew_point), 
                           class_label="delayed_str", label_x = "dew point", color=colors[c(1,5)]) + font
grfC <- plot.density.class(bfd %>% dplyr::select(delayed_str, arrival_humidity), 
                           class_label="delayed_str", label_x = "humidity", color=colors[c(1,5)]) + font
grfD <- plot.density.class(bfd %>% dplyr::select(delayed_str, arrival_visibility), 
                           class_label="delayed_str", label_x = "visibility", color=colors[c(1,5)]) + font
grfE <- plot.density.class(bfd %>% dplyr::select(delayed_str, arrival_ceiling), 
                           class_label="delayed_str", label_x = "ceiling", color=colors[c(1,5)]) + font
grfF <- plot.density.class(bfd %>% dplyr::select(delayed_str, arrival_wind_speed), 
                           class_label="delayed_str", label_x = "wind_speed", color=colors[c(1,5)]) + font
plot.size(8, 24)
grid.arrange(grfA, grfB, grfC, grfD, grfE, grfF,
             ncol=3, nrow=2)


bfd %>% dplyr::count(depart_cloudiness)  # only NA
bfd %>% dplyr::count(depart_pressure)    # value 1015 on all objects


grfA <- plot.density.class(bfd %>% dplyr::select(delayed_str, ds_arrival_wind_speed), 
                           class_label="delayed_str", label_x = "ds wind speed", color=colors[c(1,5)],
                           rotate_x=TRUE) + font
grfB <- plot.density.class(bfd %>% dplyr::select(delayed_str, arrival_wind_direction), 
                           class_label="delayed_str", label_x = "wind direction", color=colors[c(1,5)],
                           rotate_x=TRUE) + font
grfC <- plot.density.class(bfd %>% dplyr::select(delayed_str, ds_arrival_wind_direction), 
                           class_label="delayed_str", label_x = "ds wind direction", color=colors[c(1,5)],
                           rotate_x=TRUE) + font
grfD <- plot.density.class(bfd %>% dplyr::select(delayed_str, ds_arrival_day_period), 
                           class_label="delayed_str", label_x = "ds day period", color=colors[c(1,5)],
                           rotate_x=TRUE) + font
plot.size(8, 8)
grid.arrange(grfA, grfB,
             ncol=2, nrow=1)

plot.size(8, 8)
grid.arrange(grfC, grfD,
             ncol=2, nrow=1)



###################################################
# Meteorological analysis (DEPARTURE - scatter plots)

colnames(bfd)[c(14,15,16,18,20,21,22,23,24,25,50)]
bfd_departure = subset (bfd, select = c(14,15,16,18,20,21,22,23,24,25,50))
colnames(bfd_departure)

# bloco 1
tmp = subset (bfd_departure, select = c(1,2,3,11))
plot.size(12, 12)
grf <- plot.pair(data=tmp, cnames=colnames(tmp)[1:3], 
                 clabel='delayed_str', title="Flight Departure", colors=colors[c(1,5)])
grf

# bloco 2
tmp = subset (bfd_departure, select = c(4,5,6,11))
plot.size(12, 12)
grf <- plot.pair(data=tmp, cnames=colnames(tmp)[1:3], 
                 clabel='delayed_str', title="Flight Departure", colors=colors[c(1,5)])
grf

# bloco 3
tmp = subset (bfd_departure, select = c(7,8,9,10,11))
plot.size(12, 12)
grf <- plot.pair(data=tmp, cnames=colnames(tmp)[1:3], 
                 clabel='delayed_str', title="Flight Departure", colors=colors[c(1,5)])
grf

###################################################
# Meteorological analysis (ARRIVE - scatter plots)

colnames(bfd)[c(35,36,37,40,41,42,43,44,45,46,50)]
bfd_arrive = subset (bfd, select = c(35,36,37,40,41,42,43,44,45,46,50))
colnames(bfd_arrive)

# bloco 1
#tmp = subset (bfd_arrive, select = c(1,2,3,11))
plot.size(12, 12)
grf <- plot.pair(data=bfd_arrive, cnames=colnames(bfd_arrive)[1:10], 
                 clabel='delayed_str', title="Flight Arrive", colors=colors[c(1,5)])
grf

