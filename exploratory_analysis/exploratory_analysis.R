source("data-mining-class/utils/myBasic.R")
source("data-mining-class/utils/myGraphic.R")


###################################################
# config and load data
colors <- brewer.pal(11, 'Paired')
font <- theme(text = element_text(size=16))
loadlibrary("MASS")

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


