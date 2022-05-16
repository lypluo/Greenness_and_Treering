########################################
##Aim: Extract the phenological metrics from GIMMS NDVI
#refer website:http://phenofit.top/articles/phenofit-procedures.html
#######################################
# remotes::install_github("eco-hydro/phenofit")
library(phenofit)
library(data.table)
library(dplyr)
library(ggplot2)
#-----------------
#(1)load the data:
#-----------------
load.path<-"./data/GIMMS/VIs/"
df.VIs<-read.csv(file=paste0(load.path,"GIMMS_NDVI3g.csv"),sep = ",")
df.VIs<-df.VIs[,-1]
head(df.VIs)
#tidy the dateformat:
df.VIs$date<-as.Date(df.VIs$date)

#----------------------
#(2)To extract the phenology:
#trying to using phenofit R package: refer Kong et al., 2022:
#https://besjournals.onlinelibrary.wiley.com/doi/10.1111/2041-210X.13870
#----------------------
#ask wenqi to QC file..-->do not obtained at the end..

#------------------
#a.using check_input--> is used to pre-process the input VI time series;
#------------------
VI.temp<-df.VIs %>%
  filter(Site=="TLG",Year>1981)
VI.input<-check_input(t=VI.temp$date,y=VI.temp$NDVI)
plot_input(VI.input)

#------------------
#b.using season_mov--> 
#conducts the rough fitting and performs growing season division
#------------------
VI_mov<-season_mov(VI.input,list(FUN="smooth_wWHIT",wFUN="wTSM"))
plot_season(VI.input,VI_mov)
#------------------
#c.using curvefits--> 
# implements the fine fitting and reconstructs the daily VI 
#series in every growing season;
#------------------
#facing some probelm here...
VI_fit<-curvefit(VI.input,VI_mov,
                 list(tout = t,
                 methods = c("Zhang","Gu")))

#---------------------
#d.get_pheno--> extracts the vegetation phenological metrics 
#from the reconstructed daily VI time series
#---------------------
VI_pheno<-get_pheno(VI_fit,TRS=c(0.2,0.5))



