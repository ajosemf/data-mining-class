
library(DataExplorer)
library(dplyr)

## Import library
library(dataMaid)



# at Local
load("../data/bfd.rda")

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
count_voos_departure_delay_out =  sum((bfd %>% filter(departure_delay< -1440 | departure_delay > 1440)%>%count(flight_id))$n)
print(count_voos_departure_delay_out)
#Filtrando o bfd por um valor de umidade máximo
bfd = bfd %>% filter(real_duration >= -1440 & real_duration <= 1440)
sprintf("Foram retiradas %d linhas que representavam valores de real duração além de possibilidades normais, que seria entre -1440 e 1440 minutos",count_voos_real_duration_out)

##################################################
# arrive_delay (Difference in minutes between expected and real arrive datetime; ) Pelo artigo do trabalho : Finally, the regulation of ANAC prohibits delays higher than 24 hours .
#atribui a variavel por conta de possiveis mudanças

#contar voos que passam o limite
count_voos_arrive_delay_out =  sum((bfd %>% filter(arrive_delay< -1440 | arrive_delay > 1440)%>%count(flight_id))$n)
print(count_voos_departure_delay_out)
#Filtrando o bfd por um valor de umidade máximo
bfd = bfd %>% filter(real_duration >= -1440 & real_duration <= 1440)
sprintf("Foram retiradas %d linhas que representavam valores de real duração além de possibilidades normais, que seria entre -1440 e 1440",count_voos_real_duration_out)


################################################
# Teste Se tem alguém chegando depois 
count_voos_arrive_delay_out =  sum((bfd %>% filter(bfd$real_arrival_date - bfd$real_depart_date>0  )%>%count(flight_id))$n)


makeDataReport(bfd, output = "html", replace = TRUE)

