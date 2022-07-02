source("data-mining-class/utils/myBasic.R")
source("data-mining-class/utils/myGraphic.R")


colors <- brewer.pal(11, 'Paired')
font <- theme(text = element_text(size=16))
loadlibrary("MASS")

load("data-mining-class/data/bfd.rda")

colnames(bfd)

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

