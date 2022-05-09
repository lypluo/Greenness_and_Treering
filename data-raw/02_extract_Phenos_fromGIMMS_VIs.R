########################################
##Aim: Extract the phenological metrics from GIMMS NDVI
#######################################

#-----------------
#(1)load the data:
#-----------------
load.path<-"./data/GIMMS/VIs/"
df.VIs<-read.csv(file=paste0(load.path,"GIMMS_NDVI3g.csv"),sep = ",")
df.VIs<-df.VIs[,-1]


#----------------------
#(2)To extract the phenology:
#trying to using phenofit R package: refer Kong et al., 2022:
#https://besjournals.onlinelibrary.wiley.com/doi/10.1111/2041-210X.13870
#----------------------
remotes::install_github("eco-hydro/phenofit")
library(phenofit)

#ask wenqi to QC file..
