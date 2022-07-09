# at Regulus
# source("data-mining-class/utils/myBasic.R")
# source("data-mining-class/utils/myGraphic.R")

# at Local
source("utils/myBasic.R")
source("utils/myGraphic.R")

# PNG functions
PLOTS_ROOT_PATH = "exploratory_analysis/plots/"
png.init = function(filename, pointsize=20) {
  filepath = paste(PLOTS_ROOT_PATH, filename)
  png(file=filepath,
      width=1280, 
      height=800,
      pointsize = pointsize)
}
png.save = function(){
  dev.off()
}

###################################################
# config and load data
colors <- brewer.pal(11, 'Paired')
font <- theme(text = element_text(size=16))
loadlibrary("MASS")
loadlibrary("gridExtra")

# at Regulus
# load("data-mining-class/data/bfd.rda")

# at Local
load("data/bfd.rda")
colnames(bfd)


###################################################
# plot and removing canceled flights
bfd_cancel = bfd %>% dplyr::filter(situation_type == "CANCELADO")

# DEPARTURE
unique(bfd_cancel %>% dplyr::select(departure_delay))
departure_delay_cancel = bfd_cancel %>% dplyr::count(departure_delay)
departure_delay_cancel
departure_delay_not_zero_cancel = departure_delay_cancel %>% dplyr::filter(departure_delay != 0)
departure_delay_not_zero_cancel
sum(departure_delay_not_zero_cancel$n)  # 39 canceled flights with delay (positive or negative) at departure

png.init(filename = "departure_delays_on_canceled_flights.png")
barplot(departure_delay_not_zero_cancel$n,
        main = "Departure delays on canceled flights",
        xlab = "Departure delay value",
        ylab = "# Departure delay",
        names.arg = departure_delay_not_zero_cancel$departure_delay,
        col = "darkred")
png.save()


# ARRIVAL
unique(bfd_cancel %>% dplyr::select(arrival_delay))
arrival_delay_cancel = bfd_cancel %>% dplyr::count(arrival_delay)
arrival_delay_cancel
arrival_delay_not_zero_cancel = arrival_delay_cancel %>% dplyr::filter(arrival_delay != 0)
arrival_delay_not_zero_cancel
sum(arrival_delay_not_zero_cancel$n)  # 45 canceled flights with delay (positive or negative) at arrival

png.init(filename = "arrival_delays_on_canceled_flights.png")
barplot(arrival_delay_not_zero_cancel$n,
        main = "Arrival delays on canceled flights",
        xlab = "Arrival delay value",
        ylab = "# Arrival delay",
        names.arg = arrival_delay_not_zero_cancel$arrival_delay,
        col = "darkred")
png.save()


# Removing canceled flights
bfd = bfd %>% dplyr::filter(situation_type != "CANCELADO")
bfd <- subset (bfd, select = -situation_type)
"situation_type" %in% colnames(bfd)


###################################################
# Delays analysis

# DEPARTURE
grf <- plot.boxplot(bfd$departure_delay,
                    label_x = "Departure Delay", 
                    colors=colors[1]) + font
plot.size(6, 3)
png.init(filename = "departure_boxplot_delay.png")
plot(grf)
png.save()

grf <- plot.hist(bfd %>% dplyr::select(departure_delay), 
                 label_x = "Departure Delay", 
                 color=colors[1]) + font
plot.size(4, 3)
png.init(filename = "departure_histogram_delay.png")
plot(grf)
png.save()


# ARRIVAL
grf <- plot.boxplot(bfd$arrival_delay,
                    label_x = "Arrival Delay", 
                    colors=colors[1]) + font
plot.size(6, 3)
png.init(filename = "arrival_boxplot_delay.png")
plot(grf)
png.save()

grf <- plot.hist(bfd %>% dplyr::select(arrival_delay), 
                 label_x = "Arrival Delay", 
                 color=colors[1]) + font
plot.size(4, 3)
png.init(filename = "arrival_histogram_delay.png")
plot(grf)
png.save()


# ARRIVAL OUTLIER ANALYSIS
head(bfd %>% dplyr::count(arrival_delay))
tmp = bfd %>% dplyr::filter(arrival_delay == -525610)
tmp %>% dplyr::select(expected_depart_date,
                      real_depart_date,
                      expected_depart_hour,
                      real_depart_hour, 
                      departure_delay,
                      expected_arrival_date,
                      real_arrival_date,
                      expected_arrival_hour,
                      real_arrival_hour,
                      arrival_delay)  # expected and real dates has 1 year distance, probably error on record


###################################################
# Build and analysis for targets features
# departure_delayed: {'no':0, 'yes':1}
# arrival_delayed:   {'no':0, 'yes':1}


# DEPARTURE
bfd = bfd %>% mutate(departure_delayed = case_when(
  departure_delay > 0 ~ 1,
  departure_delay <= 0 ~ 0
))
bfd$departure_delayed = as.factor(bfd$departure_delayed)
levels(bfd$departure_delayed) = c('no', 'yes')
tmp = bfd %>% dplyr::count(departure_delayed)
tmp
png.init(filename = 'departure_delayed_flights.png')
barplot(tmp$n,
        main = "Departure delayed flights",
        xlab = "Delayed flight",
        ylab = "# Delayed flight",
        names.arg = levels(tmp$departure_delayed),
        col = "darkred")
png.save()
(tmp %>% dplyr::filter(departure_delayed == 'no'))$n / sum(tmp$n)  # 0.6601358 (NOT delayed frequency)
(tmp %>% dplyr::filter(departure_delayed == 'yes'))$n / sum(tmp$n)  # 0.3398642 (delayed frequency)


# ARRIVAL
bfd = bfd %>% mutate(arrival_delayed = case_when(
  arrival_delay > 0 ~ 1,
  arrival_delay <= 0 ~ 0
))
bfd$arrival_delayed = as.factor(bfd$arrival_delayed)
levels(bfd$arrival_delayed) = c('no', 'yes')
tmp = bfd %>% dplyr::count(arrival_delayed)
tmp
png.init(filename = 'arrival_delayed_flights.png')
barplot(tmp$n,
        main = "Arrival delayed flights",
        xlab = "Delayed flight",
        ylab = "# Delayed flight",
        names.arg = levels(tmp$arrival_delayed),
        col = "darkred")
png.save()
(tmp %>% dplyr::filter(arrival_delayed == 'no'))$n / sum(tmp$n)  # 0.6896697 (NOT delayed frequency)
(tmp %>% dplyr::filter(arrival_delayed == 'yes'))$n / sum(tmp$n)  # 0.3103303 (delayed frequency)


###################################################
# Meteorological analysis (DEPARTURE - density plots)

grfA <- plot.density.class(bfd %>% dplyr::select(departure_delayed, depart_temperature), 
                           class_label="departure_delayed", label_x = "temperature", color=colors[c(1,5)]) + font
grfB <- plot.density.class(bfd %>% dplyr::select(departure_delayed, depart_dew_point), 
                           class_label="departure_delayed", label_x = "dew point", color=colors[c(1,5)]) + font
grfC <- plot.density.class(bfd %>% dplyr::select(departure_delayed, depart_humidity), 
                           class_label="departure_delayed", label_x = "humidity", color=colors[c(1,5)]) + font
grfD <- plot.density.class(bfd %>% dplyr::select(departure_delayed, depart_visibility), 
                           class_label="departure_delayed", label_x = "visibility", color=colors[c(1,5)]) + font
grfE <- plot.density.class(bfd %>% dplyr::select(departure_delayed, depart_ceiling), 
                           class_label="departure_delayed", label_x = "ceiling", color=colors[c(1,5)]) + font
grfF <- plot.density.class(bfd %>% dplyr::select(departure_delayed, depart_wind_speed), 
                           class_label="departure_delayed", label_x = "wind_speed", color=colors[c(1,5)]) + font
png.init(filename = 'departure_density_1.png')
plot.size(8, 24)
grid.arrange(grfA, grfB, grfC, grfD, grfE, grfF,
             ncol=3, nrow=2)
png.save()


bfd %>% dplyr::count(depart_cloudiness)  # only NA value
bfd %>% dplyr::count(depart_pressure)    # only 1015 value


grfA <- plot.density.class(bfd %>% dplyr::select(departure_delayed, ds_depart_wind_speed), 
                           class_label="departure_delayed", label_x = "ds wind speed", color=colors[c(1,5)],
                           rotate_x=TRUE) + font
grfB <- plot.density.class(bfd %>% dplyr::select(departure_delayed, depart_wind_direction), 
                           class_label="departure_delayed", label_x = "wind direction", color=colors[c(1,5)],
                           rotate_x=TRUE) + font
grfC <- plot.density.class(bfd %>% dplyr::select(departure_delayed, ds_depart_wind_direction), 
                           class_label="departure_delayed", label_x = "ds wind direction", color=colors[c(1,5)],
                           rotate_x=TRUE) + font
grfD <- plot.density.class(bfd %>% dplyr::select(departure_delayed, ds_depart_day_period), 
                           class_label="departure_delayed", label_x = "ds day period", color=colors[c(1,5)],
                           rotate_x=TRUE) + font

png.init(filename = 'departure_density_2.png')
plot.size(8, 8)
grid.arrange(grfA, grfB,
             ncol=2, nrow=1)
png.save()

png.init(filename = 'departure_density_3.png')
plot.size(8, 8)
grid.arrange(grfC, grfD,
             ncol=2, nrow=1)
png.save()


###################################################
# Meteorological analysis (ARRIVAL - density plots)

grfA <- plot.density.class(bfd %>% dplyr::select(arrival_delayed, arrival_temperature), 
                           class_label="arrival_delayed", label_x = "temperature", color=colors[c(1,5)]) + font
grfB <- plot.density.class(bfd %>% dplyr::select(arrival_delayed, arrival_dew_point), 
                           class_label="arrival_delayed", label_x = "dew point", color=colors[c(1,5)]) + font
grfC <- plot.density.class(bfd %>% dplyr::select(arrival_delayed, arrival_humidity), 
                           class_label="arrival_delayed", label_x = "humidity", color=colors[c(1,5)]) + font
grfD <- plot.density.class(bfd %>% dplyr::select(arrival_delayed, arrival_visibility), 
                           class_label="arrival_delayed", label_x = "visibility", color=colors[c(1,5)]) + font
grfE <- plot.density.class(bfd %>% dplyr::select(arrival_delayed, arrival_ceiling), 
                           class_label="arrival_delayed", label_x = "ceiling", color=colors[c(1,5)]) + font
grfF <- plot.density.class(bfd %>% dplyr::select(arrival_delayed, arrival_wind_speed), 
                           class_label="arrival_delayed", label_x = "wind_speed", color=colors[c(1,5)]) + font
png.init(filename = 'arrival_density_1.png')
plot.size(8, 24)
grid.arrange(grfA, grfB, grfC, grfD, grfE, grfF,
             ncol=3, nrow=2)
png.save()


bfd %>% dplyr::count(arrival_pressure)    # value 1015 on all objects


grfA <- plot.density.class(bfd %>% dplyr::select(arrival_delayed, ds_arrival_wind_speed), 
                           class_label="arrival_delayed", label_x = "ds wind speed", color=colors[c(1,5)],
                           rotate_x=TRUE) + font
grfB <- plot.density.class(bfd %>% dplyr::select(arrival_delayed, arrival_wind_direction), 
                           class_label="arrival_delayed", label_x = "wind direction", color=colors[c(1,5)],
                           rotate_x=TRUE) + font
grfC <- plot.density.class(bfd %>% dplyr::select(arrival_delayed, ds_arrival_wind_direction), 
                           class_label="arrival_delayed", label_x = "ds wind direction", color=colors[c(1,5)],
                           rotate_x=TRUE) + font
grfD <- plot.density.class(bfd %>% dplyr::select(arrival_delayed, ds_arrival_day_period), 
                           class_label="arrival_delayed", label_x = "ds day period", color=colors[c(1,5)],
                           rotate_x=TRUE) + font
grfE <- plot.density.class(bfd %>% dplyr::select(arrival_delayed, arrival_cloudiness), 
                           class_label="arrival_delayed", label_x = "cloudiness", color=colors[c(1,5)],
                           rotate_x=TRUE) + font

png.init(filename = 'arrival_density_2.png')
plot.size(8, 8)
grid.arrange(grfA, grfB,
             ncol=2, nrow=1)
png.save()

png.init(filename = 'arrival_density_3.png')
plot.size(8, 8)
grid.arrange(grfC, grfD,
             ncol=2, nrow=1)
png.save()

png.init(filename = 'arrival_density_4.png')
plot.size(8, 8)
grid.arrange(grfE,
             ncol=1, nrow=1)
png.save()


###################################################
# freeing memory

rm(grf, grfA, grfB, grfC, grfD, grfE, grfF)
rm(arrival_delay_cancel, 
   arrival_delay_not_zero_cancel,
   bfd_cancel,
   departure_delay_cancel,
   departure_delay_not_zero_cancel,
   tmp)
gc()  # garbage collector


###################################################
# Meteorological analysis (DEPARTURE - scatter plots)

cols = c(14,15,16,18,20,21,22,23,24,25,49)  # col 49: departure_delayed
colnames(bfd)[cols]  
tmp = subset (bfd, 
              select=cols)
meteorological_cols = colnames(tmp)[1:length(tmp)-1]
meteorological_cols

for (col_i in meteorological_cols[1:length(meteorological_cols)-1]){
  next_col_idx = grep(col_i, meteorological_cols) + 1  # grep() gets col index
  var_name_1 = gsub('ds_depart_', '', col_i)
  var_name_1 = gsub('depart_', '', col_i)
  for (col_j in meteorological_cols[next_col_idx:length(meteorological_cols)]){
    meteorological_variables = subset (tmp,
                                       select=c(col_i, col_j, 'departure_delayed'))
    plot.size(12, 12)
    grf = ggplot(meteorological_variables,
                 aes_string(x=col_i,
                            y=col_j)) +
      geom_point(aes_string(color = 'departure_delayed'), size=4) +
      scale_color_manual(values = colors[c(1,5)]) +
      theme_bw() +
      theme(text = element_text(size = 30))
    var_name_2 = gsub('ds_depart_', '', col_j)
    var_name_2 = gsub('depart_', '', col_j)
    filename_end = paste0(var_name_1, '_', var_name_2, '.png')
    filename = paste0('departure_scatter_', filename_end)
    png.init(filename = filename)
    plot(grf)
    png.save()
  }
}


###################################################
# Meteorological analysis (ARRIVAL - scatter plots)

cols = c(35,36,37,39,40,41,42,43,44,45,46,50)  # col 50: arrival_delayed
colnames(bfd)[cols]  
tmp = subset (bfd, 
              select=cols)
meteorological_cols = colnames(tmp)[1:length(tmp)-1]
meteorological_cols

for (col_i in meteorological_cols[1:length(meteorological_cols)-1]){
  next_col_idx = grep(col_i, meteorological_cols) + 1  # grep() gets col index
  var_name_1 = gsub('ds_arrival_', '', col_i)
  var_name_1 = gsub('arrival_', '', col_i)
  for (col_j in meteorological_cols[next_col_idx:length(meteorological_cols)]){
    meteorological_variables = subset (tmp,
                                       select=c(col_i, col_j, 'arrival_delayed'))
    plot.size(12, 12)
    grf = ggplot(meteorological_variables,
                 aes_string(x=col_i,
                            y=col_j)) +
      geom_point(aes_string(color = 'arrival_delayed'), size=4) +
      scale_color_manual(values = colors[c(1,5)]) +
      theme_bw() +
      theme(text = element_text(size = 30))
    var_name_2 = gsub('ds_arrival_', '', col_j)
    var_name_2 = gsub('arrival_', '', col_j)
    filename_end = paste0(var_name_1, '_', var_name_2, '.png')
    filename = paste0('arrival_scatter_', filename_end)
    png.init(filename = filename)
    plot(grf)
    png.save()
  }
}
