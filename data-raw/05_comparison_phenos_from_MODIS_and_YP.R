#------------------------------
#Aim:comparison between SOS from by MODIS and sos extracted by YP:
#------------------------------
library(dplyr)
library(tidyverse)
#-----------
#(1)load the data:
#-----------
MODIS.phenos<-read.csv(paste0("./data/MODIS/Phenology/Pheno_DoY.csv"))
MODIS.phenos<-MODIS.phenos[,-1]
YP.phenos<-read.csv(paste0("./data/MODIS/Phenology/Pheno_DoY_updated_YP.csv"))
YP.phenos<-YP.phenos[,-1]
#------
#(2)plotting
#-------
t1<-MODIS.phenos %>%
  select(sitename,Year,Greenup)
names(t1)<-c("sitename","year","greenup")
t2<-YP.phenos %>%
  select(sitename,year,trs_sos25,trs_sos50,trs_eos75)
#
df.merge<-left_join(t1,t2,by=c("sitename","year"))

#for sos25-greenup
df.merge %>%
  group_by(sitename)%>%
  ggplot(aes(x=greenup,y=trs_sos25))+
  geom_point()+
  geom_smooth(method = "lm")+
  geom_abline(slope = 1,intercept = 0,lty=2)+
  facet_wrap(~sitename)
lm_coefs<-round(coef(lm(trs_sos25~greenup,data = df.merge)),2)
ggplot(data=df.merge,aes(x=greenup,y=trs_sos25,col=sitename))+
  geom_point()+
  geom_abline(intercept =lm_coefs[1],slope = lm_coefs[2],col="blue")+
  geom_abline(slope = 1,intercept = 0,lty=2)

#for sos50-greenup
ggplot(data=df.merge,aes(x=greenup,y=trs_sos50,col=sitename))+
  geom_point()+
  geom_smooth(method = "lm")+
  geom_abline(slope = 1,intercept = 0,lty=2)
lm_coefs<-round(coef(lm(trs_sos50~greenup,data = df.merge)),2)
ggplot(data=df.merge,aes(x=greenup,y=trs_sos50,col=sitename))+
  geom_point()+
  geom_abline(intercept =lm_coefs[1],slope = lm_coefs[2],col="blue")+
  geom_abline(slope = 1,intercept = 0,lty=2)

