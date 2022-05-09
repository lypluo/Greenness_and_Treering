#####################################
#First tidy the GIMMS data download by the Wenqi:
#####################################
library(readxl)
library(dplyr)
library(ggplot2)
#-----------------
#(1)load the data:
#---------------
load.path<-"data-raw/data_from_GIMMS/"
ori.VIs<-read_xlsx(paste0(load.path,"GIMMS_NDVI3g_v1.xlsx"))
ori.VIs<-ori.VIs[,-1]
names(ori.VIs)<-c("Site","lat","lon","Year","Month","NDVI","Source")

#
proc.VIs<-ori.VIs %>%
  mutate(month=as.integer(substr(Month,1,2)),
         date_flag=substr(Month,3,3),
         day=ifelse(date_flag=="a",8,21),
         date=as.Date(paste0(Year,"-",month,"-",day)))
final.VIs<-proc.VIs %>%
    select(Site,lat,lon,date,Year,month,NDVI)
##
final.VIs %>%
  # group_by(Site)%>%
  filter(Site=="LPS" & Year<=1990)%>%
  ggplot()+
  geom_point(aes(x=date,y=NDVI))+
  facet_wrap(.~Site)

#------------------
#(2)filter the data-->e.g. remove the contamination of the snow
#------------------



#------------------
#(3)gapfilling the data
#------------------








#save the data:
save.path<-"./data/GIMMS/VIs/"
write.csv(final.VIs,file = paste0(save.path,"GIMMS_NDVI3g.csv"))
