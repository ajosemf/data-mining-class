# at Regulus
# source("data-mining-class/utils/myBasic.R")
# source("data-mining-class/utils/myGraphic.R")

# at Local
source("../utils/myBasic.R")
source("../utils/myGraphic.R")

# PNG functions
PLOTS_ROOT_PATH = "data_preprocessing/plots/"
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
load("../data/bfd.rda")
colnames(bfd)


#################################################
#Data Cleaning 

#Voos cancelados
# Os voos cancelados não são alvo de nossa análise, somente os que tiveram atraso
#contado voos cancelados
count_voos_canceled = sum((bfd %>% filter(situation_type == "CANCELADO")%>%count(flight_id))$n)
print(count_voos_canceled)
#retirando voos cancelados trazendo só os realizados
bfd = bfd %>% filter(situation_type == "REALIZADOS")
sprintf("Foram retiradas %d linhas que representavam voos cancelados",count_voos_canceled)

#analisando o arrival ceiling apesar dos outliers eu acho que se for metro pode existir, acho que vale referenciar

# arrival_humidity acredito que não deveria ser maior igual a 100% , pois 100% é o ponto de saturação da água no ar acho q vale referenciar
