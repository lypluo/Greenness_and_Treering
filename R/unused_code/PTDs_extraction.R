#--------------------------------
#Aim:extract the PTDs from the time series of NDVI
#--------------------------------
library(readxl)
library(tidyr)
library(lubridate)
library(ggplot2)
library(reshape2)
#---------------------------
#(1)load the data from Wenqi
#---------------------------
data.path<-"./data/"
df<-read_excel(paste0(data.path,"NDVI_SOS.xlsx"),sheet = "MODIS")

#---------------------------
#(2)extract the PTDs
#---------------------------
fun.path<-"./R/"
source(paste0(fun.path,"pheno_extraction_fun2.R"))

#
names(df)
#re-tidy the form of data.frame:
df<-reshape2::melt(df,id="date")
names(df)<-c("Date","sitename","NDVI")
df$Date<-as.Date(df$Date)

#check the time series:
#an example: for site "BS"
sites<-unique(df$sitename)
ori_ts<-c()
for (i in 1:length(sites)) {
plot_temp<-df %>%
    filter(sitename==sites[i])%>%
    # filter(Date>=as.Date("2005-01-01") & Date<=as.Date("2006-12-31")) %>%
    ggplot(aes(x=Date,y=NDVI))+
    geom_point()+
    geom_line()+
    annotate(geom = "text",x=as.Date("2020-01-01"),y=0.1,label=sites[i],col="blue")
ori_ts[[i]]<-plot_temp
}
names(ori_ts)<-sites


#---(2a)test for one site:smoothed the time series and extract the phenophases-------
df %>%
  filter(sitename=="BS")%>%
  mutate(year=year(Date),
         doy=yday(Date),
         )%>%
  group_by(year)%>%
  ggplot(aes(x=doy,y=NDVI))+
  geom_point()+
  geom_line()+
  facet_grid(~year)
