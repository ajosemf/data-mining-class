# at Regulus
# source("data-mining-class/utils/myBasic.R")
# source("data-mining-class/utils/myGraphic.R")

# at Local
source("utils/myBasic.R")
source("utils/myGraphic.R")

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
load("data/bfd.rda")
colnames(bfd)
