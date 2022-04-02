##############################
#Aim: remove the outliers of VIs and gap-fill the VIs
##############################
library(dplyr)
library(tidyr)
library(ggplot2)
library(readxl)
#---------------
#1. load the data
#---------------
#
df.VIs<-read.csv(paste0("./data/MODIS/VIs/df_VIs.csv"))
df.VIs<-df.VIs[,-1]
df.VIs$date<-as.Date(df.VIs$date)
#load the site infomation:
site.info<-read_xlsx(paste0("./data/data_from_Wenqi/site_general_info.xlsx"))
site.info<-site.info %>%
  select(Code,`Longitude (°E)`,`Latitude (°N)`,`Altitude (m)`,Anatomy,Prec,Tem)
names(site.info)<-c("sitename","Long.","Lat.","Alt.","Is_Anatomy","Prec","Ta")
#merge data:
df.VIs<-left_join(df.VIs,site.info,id="sitename")

#---------------
#2.remove the outliers of VIs
#---------------
#write a function
rm_outliers<-function(df,VI.name) {
  # df<-df.VIs
  # VI.name<-"EVI"
  #
  df_sel<-df %>%
    select(sitename,date,NDVI,EVI,Prec,Ta)
  sites<-unique(df_sel$sitename)
  #
  df.all<-c()
  for (i in 1:length(sites)) {
    df_t<-df_sel %>%
      filter(sitename==sites[i])
    #------------------
    #filter the data
    #------------------
    if(VI.name=="NDVI"){
      df_t$NDVI.filtered<-df_t$NDVI
      #a. remove the NDVI outlier below 0.1(absolute values)
      df_t$NDVI.filtered[df_t$NDVI.filtered<0.1]<-NA
      #b. then remove the NDVI outlier below perc1
      NDVI_q0510<-quantile(df_t$NDVI.filtered,probs=c(0.01,0.025,0.05),na.rm = T)
      df_t$NDVI.filtered[df_t$NDVI.filtered<NDVI_q0510[1]]<-NA
      #c. remove the outlier according to annual Prec and Temp
      Prec<-unique(df_t$Prec)
      Ta<-unique(df_t$Ta)
      if(Prec<410 & Ta>6){
       df_t$NDVI.filtered[df_t$NDVI.filtered<=0.12]<-NA
      }
      if(c(Prec> 300& Prec<410) & Ta>3 & Ta<6){
        df_t$NDVI.filtered[df_t$NDVI.filtered<=0.15]<-NA
      }
      if(c(Prec<410 & Ta<3)|Prec>410){
        df_t$NDVI.filtered[df_t$NDVI.filtered<=0.2]<-NA
      }
      ###
      # plot(df_t$date,df_t$NDVI)
      # points(df_t$date,df_t$NDVI.filtered,col="green3")
      # abline(h=c(0.12,0.18,0.2),col="orange")
      df.out<-df_t %>%
        select(sitename,date,NDVI.filtered)
      df.all<-rbind(df.all,df.out)
      rm(df.out)
    }
    if(VI.name=="EVI"){
      df_t$EVI.filtered<-df_t$EVI
      #a. remove the NDVI outlier below 0.05(absolute values) and higher than 0.8
      df_t$EVI.filtered[df_t$EVI.filtered<0.05 | df_t$EVI.filtered>0.8]<-NA
      #b.remove the outlier according to annual Prec and Temp
      Prec<-unique(df_t$Prec)
      Ta<-unique(df_t$Ta)
      if(Prec<410 & Ta>6){
        df_t$EVI.filtered[df_t$EVI.filtered<=0.05]<-NA
      }
      if(c(Prec> 300& Prec<410) & Ta>3 & Ta<6){
        df_t$EVI.filtered[df_t$EVI.filtered<=0.08]<-NA
      }
      if(c(Prec<410 & Ta<3)|Prec>410){
        df_t$EVI.filtered[df_t$EVI.filtered<=0.1]<-NA
      }
      #
      # plot(df_t$date,df_t$EVI)
      # points(df_t$date,df_t$EVI.filtered,col="green3")
      # abline(h=c(0.05,0.08,0.1),col="orange")
      df.out<-df_t %>%
        select(sitename,date,EVI.filtered)
      df.all<-rbind(df.all,df.out)
      rm(df.out)
    }
    #------------------
    #plotting
    #------------------
    # if(VI.name=="NDVI"){
    #   p_display<-df_t%>%
    #     ggplot()+
    #     geom_point(aes(x=date,y=NDVI,col="NDVI"))+
    #     geom_point(aes(x=date,y=NDVI.filtered,col="filtered"))+
    #     geom_hline(yintercept = NDVI_q0510,col="blue",lty=2)+
    #     geom_hline(yintercept = c(0.12,0.15,0.2),col="orange")+
    #     scale_color_manual(values = c("NDVI"="tomato","filtered"="green3"))+
    #     annotate(geom = "text",x=as.Date("2019-01-01"),y=0.95,col="blue",label=sites[i])
    # }
    # if(VI.name=="EVI"){
    #   p_display<-df_t%>%
    #     ggplot()+
    #     geom_point(aes(x=date,y=EVI,col="EVI"))+
    #     geom_point(aes(x=date,y=EVI.filtered,col="filtered"))+
    #     geom_hline(yintercept = c(0.05,0.08,0.1),col="orange")+
    #     scale_color_manual(values = c("EVI"="tomato","filtered"="green3"))+
    #     annotate(geom = "text",x=as.Date("2019-01-01"),y=0.6,col="blue",label=sites[i])
    # }

    # print(p_display)
    #
  }
  return(df.all)
}

##
NDVI.filtered<-rm_outliers(df.VIs,VI.name = "NDVI")
EVI.filtered<-rm_outliers(df.VIs,VI.name = "EVI")
#merge
df.VIs_final<-data.frame(df.VIs,"NDVI.filtered"=NDVI.filtered$NDVI.filtered)
df.VIs_final<-data.frame(df.VIs_final,"EVI.filtered"=EVI.filtered$EVI.filtered)
#save the filtered data:
write.csv(df.VIs_final,paste0("./data/MODIS/VIs/df_VIs.filtered.csv"))

#---------------
#3.plotting
#---------------
#test#
df.VIs_final %>%
  filter(sitename=="QS")%>%
  ggplot()+
  # geom_point(aes(x=date,y=NDVI,col="NDVI"))+
  # geom_point(aes(x=date,y=NDVI.filtered,col="filtered"))+
  geom_point(aes(x=date,y=EVI,col="EVI"))+
  geom_point(aes(x=date,y=EVI.filtered,col="filtered"))


df.VIs_final %>%
  # filter(sitename=="QS")%>%
  group_by(sitename)%>%
  ggplot()+
  geom_point(aes(x=date,y=NDVI,col="NDVI"))+
  geom_point(aes(x=date,y=NDVI.filtered,col="filtered"))+
  facet_wrap(~sitename)

df.VIs_final %>%
  group_by(sitename)%>%
  ggplot()+
  geom_point(aes(x=date,y=EVI,col="EVI"))+
  geom_point(aes(x=date,y=EVI.filtered,col="filtered"))+
  facet_wrap(~sitename)
