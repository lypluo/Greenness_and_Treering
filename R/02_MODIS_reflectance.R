##############################
#Aim: tidy the reflectance data from MODIS
##############################
library(lubridate)
library(dplyr)
library(tidyr)
library(reshape2)
#--------------
#(1)load the data
#--------------
#--site info---
site.info<-read.csv(paste0("./data-raw/download_reflectance_from_MODIS/","sites_coordinates_wenqi.csv"))
site.info<-site.info[,1:3]; names(site.info)<-c("ID","latitude","longitude")
#
base.path<-"./data-raw/download_reflectance_from_MODIS/downloaded_data/Reflectances/"

#---read the reflectances and calculate the VIs
sites<-site.info$ID
df_VIs<-c()
for (i in 1:length(sites)) {
  df_B1<-read.csv(paste0(base.path,sites[i],"/statistics_Nadir_Reflectance_Band1.csv"))
  df_B2<-read.csv(paste0(base.path,sites[i],"/statistics_Nadir_Reflectance_Band2.csv"))
  df_B3<-read.csv(paste0(base.path,sites[i],"/statistics_Nadir_Reflectance_Band3.csv"))
  df_B4<-read.csv(paste0(base.path,sites[i],"/statistics_Nadir_Reflectance_Band4.csv"))
  df_B5<-read.csv(paste0(base.path,sites[i],"/statistics_Nadir_Reflectance_Band5.csv"))
  df_B6<-read.csv(paste0(base.path,sites[i],"/statistics_Nadir_Reflectance_Band6.csv"))
  df_B7<-read.csv(paste0(base.path,sites[i],"/statistics_Nadir_Reflectance_Band7.csv"))
  ##merge 7 bands:
  df_B1<-df_B1[,c("date","mean")];names(df_B1)<-c("date","B1");df_B1$date<-as.Date(df_B1$date)
  df_B2<-df_B2[,c("date","mean")];names(df_B2)<-c("date","B2");df_B2$date<-as.Date(df_B2$date)
  df_B3<-df_B3[,c("date","mean")];names(df_B3)<-c("date","B3");df_B3$date<-as.Date(df_B3$date)
  df_B4<-df_B4[,c("date","mean")];names(df_B4)<-c("date","B4");df_B4$date<-as.Date(df_B4$date)
  df_B5<-df_B5[,c("date","mean")];names(df_B5)<-c("date","B5");df_B5$date<-as.Date(df_B5$date)
  df_B6<-df_B6[,c("date","mean")];names(df_B6)<-c("date","B6");df_B6$date<-as.Date(df_B6$date)
  df_B7<-df_B7[,c("date","mean")];names(df_B7)<-c("date","B7");df_B7$date<-as.Date(df_B7$date)
  #I found the df_Bands has duplicated dates and values-->hence first remove the duplicated dates
  df_B1<-df_B1[!duplicated(df_B1$date),]
  df_B2<-df_B2[!duplicated(df_B2$date),]
  df_B3<-df_B3[!duplicated(df_B3$date),]
  df_B4<-df_B4[!duplicated(df_B4$date),]
  df_B5<-df_B5[!duplicated(df_B5$date),]
  df_B6<-df_B6[!duplicated(df_B6$date),]
  df_B7<-df_B7[!duplicated(df_B7$date),]
  #
  t12<-left_join(df_B1,df_B2)
  t23<-left_join(t12,df_B3)
  t34<-left_join(t23,df_B4)
  t45<-left_join(t34,df_B5)
  t56<-left_join(t45,df_B6)
  temp<-left_join(t56,df_B7)
  #using the bands to calculate the VIs: NDVI and EVI-->according to Walther et al., 2021:
  #using mean to calculate VIs:
  NDVI<-c(temp$B2 - temp$B1)/c(temp$B2 + temp$B1)
  EVI<-2.5*c(temp$B2 - temp$B1)/c(temp$B2 +6*temp$B1-7.5*temp$B3+1)
  tt<-cbind(sitename=rep(sites[i],nrow(temp)),temp,NDVI=NDVI,EVI=EVI)
  df_VIs<-rbind(df_VIs,tt)
  #only keep when data is not NA
  df_VIs<-df_VIs[!is.na(df_VIs$date),]
  rm(tt)
}

#---save the data:
write.csv(df_VIs,paste0("./data/MODIS/VIs/df_VIs.csv"))

#test:
library(ggplot2)
df_VIs%>%
  filter(sitename=="TLG")%>%
  ggplot()+
    geom_point(aes(x=date,y=NDVI,col="NDVI"))+
    geom_point(aes(x=date,y=EVI,col="EVI"))

