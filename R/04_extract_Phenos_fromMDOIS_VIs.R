#-----------------------
#Aim: to extract phenology from VIs
#here: I used EVI to extract the phenos since EVI has much less variation
#-----------------------
library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)
#----------------
#(1)load the data
#----------------
df.VIs<-read.csv(paste0("./data/MODIS/VIs/df_VIs.filtered.csv"))
df.VIs<-df.VIs[,-1]
df.VIs$date<-as.Date(df.VIs$date)
#--------------------
#(2)start to extract the phenology
#--------------------
#------------------------------
#a. check the time series of each site
#------------------------------
view_ts<-function(df,site){
  # df<-df.VIs
  # site<-"LS"

  df.sel<-df %>%
    filter(sitename==site)%>%
    select(sitename,date,EVI,EVI.filtered)%>%
    mutate(year=year(date),
           doy=yday(date))
  #
  df.sel.annual<-df.sel %>%
    dplyr::group_by(year)%>%
    dplyr::summarise(nday=length(EVI.filtered[!is.na(EVI.filtered)]),
              mindoy=min(doy,na.rm = T))%>%
    mutate(year_flag=ifelse(mindoy<60 & nday>c(365/2),"for-pheno","nofor-pheno"))

  #flag to remove the year data
  df.merge<-left_join(df.sel,df.sel.annual)
  df_plot<-df.merge %>%
    filter(year_flag=="for-pheno")
  #plotting
  y.range<-range(df.merge$EVI.filtered,na.rm=T)
  p_plot<-df_plot %>%
    group_by(year)%>%
    ggplot(aes(x=doy,y=EVI.filtered,col="year"))+
    # geom_line()+
    geom_point()+
    ylab("EVI")+
    annotate(geom = "text",x=250,y.range[1]+0.1,
             label=paste0(site))+
    facet_wrap(~year)
   print(p_plot)
  ##
  return(df.sel.annual)
}
#
sites<-unique(df.VIs$sitename)
i=20
view_ts(df.VIs,sites[i])
##after checking all the sites, that all the sites
#has the same extraction period: 2000-2021(22 years)-->except the site LS

#------------------------------
#b. start to extract the phenology:
#------------------------------
#source the extraction function:
source(paste0("./R/pheno_extraction_fun2.R"))
extract_phenos<-function(df,site){
  # df<-df.VIs
  # site<-"LS"

  df.sel<-df %>%
    filter(sitename==site)%>%
    select(sitename,date,EVI,EVI.filtered)%>%
    mutate(year=year(date),
           doy=yday(date))
  #pay attention:summarise must add dplyr::,otherwise it does not work
  df.sel.annual<-df.sel %>%
    dplyr::group_by(year)%>%
    dplyr::summarise(nday=length(EVI.filtered[!is.na(EVI.filtered)]),
              mindoy=min(doy,na.rm = T))%>%
    mutate(year_flag=ifelse(mindoy<60 & nday>c(365/2),"for-pheno","nofor-pheno"))

  #flag to remove the year data
  df.merge<-left_join(df.sel,df.sel.annual)
  df.proc<-df.merge %>%
    filter(year_flag=="for-pheno")
  #extraction of phenology:
  df_phenos<-c()
  years<-unique(df.proc$year)
  for (i in 1:length(years)) {
    df.ts<-df.proc %>%
      filter(year==years[i])
    pheno_results<-SplinePheno_extraction(df.ts,site,"EVI",FALSE,years[i])
    #merge phenos
    phenos_temp<-pheno_results$Pheno_sum$pheno
    phenos_temp$year<-years[i]
    phenos_temp$sitename<-site
    df_phenos<-rbind(df_phenos,phenos_temp)
  }
  return(df_phenos)
}
#------------
#merge all the years
#------------
Phenos.final<-c()
for (i in 1:length(sites)) {
  phenos<-extract_phenos(df.VIs,sites[i])
  Phenos.final<-rbind(Phenos.final,phenos)
}
#----------------------
#save the data
#----------------------
write.csv(Phenos.final,file=paste0("./data/MODIS/Phenology/Pheno_DoY_updated_YP.csv"))
