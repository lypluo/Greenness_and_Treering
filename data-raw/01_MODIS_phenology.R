##############################
#Aim: tidy the phenology data from MODIS
##############################
library(lubridate)
#--------------
#(1)load the data
#--------------
#--site info---
site.info<-read.csv(paste0("./data-raw/download_reflectance_from_MODIS/","sites_coordinates_wenqi.csv"))
site.info<-site.info[,1:3]; names(site.info)<-c("ID","latitude","longitude")
#
base.path<-"./data-raw/download_reflectance_from_MODIS/downloaded_data/phenology/"

#---read the phenology based on DOY
sites<-site.info$ID
Pheno_DoY<-c()
for (i in 1:length(sites)) {
  temp<-read.csv(paste0(base.path,sites[i],"/Phenology.csv"))
  tt<-cbind(sitename=rep(sites[i],nrow(temp)),temp)
  Pheno_DoY<-rbind(Pheno_DoY,tt)
  rm(tt)
}

Pheno_Date<-c()
for (i in 1:length(sites)) {
  temp<-read.csv(paste0(base.path,sites[i],"/Phenology.csv"))
  tt<-cbind(sitename=rep(sites[i],nrow(temp)),temp)
  for (j in 4:ncol(tt)) {
    tt[,j]<-tt[,j]+as.Date(paste0(tt[,"Year"],"-01-01"))-1
  }
  Pheno_Date<-rbind(Pheno_Date,tt)
  rm(tt)
}
#---save the data:
write.csv(Pheno_Date,paste0("./data/MODIS/Phenology/Pheno_Date.csv"))
write.csv(Pheno_DoY,paste0("./data/MODIS/Phenology/Pheno_DoY.csv"))
