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
sites<-unique(df.VIs$Site)
#At present, only extract the Phenos from sites before NS:
sites<-sites[1:23]
#----------------------
#(2)To extract the phenology:
#trying to using phenofit R package: refer Kong et al., 2022:
#https://besjournals.onlinelibrary.wiley.com/doi/10.1111/2041-210X.13870
#----------------------
#ask wenqi to QC file..-->do not obtained at the end..
Phenos_doy<-c()
Phenos_date<-c()

for (i in 1:length(sites)) {
sitename<-sites[i]
#------------------
#a.using check_input--> is used to pre-process the input VI time series;
#------------------
VI.temp<-df.VIs %>%
  filter(Site==sitename,Year>1981)
VI_input<-check_input(t=VI.temp$date,y=VI.temp$NDVI,wmin = 0.2)
plot_input(VI_input)

#------------------
#b.using season_mov--> 
#conducts the rough fitting and performs growing season division
#------------------
VI_mov<-season_mov(VI_input,list(rFUN="smooth_wWHIT",wFUN="wTSM",
        iters=1,maxExtendMonth=12,r_max=0.1,r_min=0.05))
plot_season(VI_input,VI_mov)

#------------------
#c.using curvefits--> 
# implements the fine fitting and reconstructs the daily VI 
#series in every growing season;
#------------------
#facing some probelm here...
VI_fit<-curvefits(VI_input,VI_mov,
      list(maxExtendMonth=3,minExtendMonth=1,minPercValid = 0.2,
            methods = c("AG", "Zhang", "Beck", "Elmore")))

#---------------------
#d.get_pheno--> extracts the vegetation phenological metrics 
#from the reconstructed daily VI time series
#---------------------
VI_pheno<-get_pheno(VI_fit,"Elmore",TRS=c(0.25,0.5))

## check the extracted phenology
# pheno <- get_pheno(VI_fit[1:6], "Elmore", IsPlot = T,TRS=c(0.25,0.5))

#-----------------------
#e.tidy the extract Phenos
#-----------------------
#only using the Elmore as the fitting:
temp_doy<-VI_pheno$doy$Elmore
temp_doy$sitename<-rep(sitename,nrow(temp_doy))
Phenos_doy<-rbind(Phenos_doy,temp_doy)

temp_date<-VI_pheno$date$Elmore
temp_date$sitename<-rep(sitename,nrow(temp_date))
Phenos_date<-rbind(Phenos_date,temp_date)

rm(temp_doy,temp_date)
print(paste0("The",i,"site>>>>>>>"))
}


#further tidy:
Phenos_date_final<-Phenos_date %>%
  mutate(year=substr(flag,1,4),
    flag=NULL,origin=NULL)
Phenos_doy_final<-Phenos_doy %>%
  mutate(year=substr(flag,1,4),
         flag=NULL,origin=NULL)

#-----------------------
#f.save the data:
#-----------------------
save.path<-"./data/GIMMS/Phenology/"
write.csv(Phenos_date_final,paste0(save.path,"Pheno_Date.csv"))
write.csv(Phenos_doy_final,paste0(save.path,"Pheno_DoY.csv"))
