##########################
#function used to filter the data
##########################
library(zoo)
library(phenopix)
SplineFilter_ts<-function(df,VI.name){
  # ts_ori<-df.proc.VIs[,2]
  ##do primary gapfilling in order to enable SplineFit function afterwards
  ##do not use SSA method as sometimes it extrapolates too much
  df<-df_t
  VI.name<-"NDVI"
  ts_ori<-zoo(df[,VI.name],order.by = c(1:length(df[,VI.name])))
  #for Vegetation index, set lower limit <0
  hard_lower_limit<-0
  #hard removing
  ts_ori[ts_ori<hard_lower_limit]<-NA
  #remove based on the mean and sd of the times series(based on 2*sd):
  mean_ts<-mean(ts_ori,na.rm=T)
  sd_ts<-sd(ts_ori,na.rm = T)
  ts_ori[ts_ori<c(mean_ts-2*sd_ts)]<-NA
  ts_ori[ts_ori>c(mean_ts+2*sd_ts)]<-NA
  pos_NA_ori<-which(is.na(ts_ori))
  #
  ##----------------------
  #continue to remove the outliers
  ##----------------------
  # #in case some NA at the beginning of the time series,use na.fill for the beginning of gapfilling
  # temp1<-na.fill(ts_ori[1:150],fill='extend') ##here temporily set the 150 day as the maximum flling gap
  # temp2<-na.spline(ts_ori[151:length(ts_ori)])
  # ts_new<-c(temp1,temp2)
  ts_new<-ts_ori
  pos_noise<-c()
  for(i in 1:20){
    ts_new<-zoo(ts_new,order.by = index(1:length(ts_new)))
    ####delete the data which bigger than (SplineFit-sd*2,SplineFit+sd*4)
    fitResult<-rollapply(ts_new,5,function(x){mean(x)},fill = NA)
    residuals<-as.numeric(ts_new)-as.numeric(fitResult)
    sd.res<-sd(residuals,na.rm=TRUE)
    selectcriterion_above<-mean(residuals)+5*sd.res
    selectcriterion_below<-mean(residuals)-5*sd.res
    ##
    plot(df_t$date,residuals)
    abline(h=c(selectcriterion_below,selectcriterion_above),col="red")
    noise<-residuals[residuals>selectcriterion_above|residuals<selectcriterion_below]
    matchNum<-match(noise,residuals)
    pos_noise<-c(pos_noise,matchNum)
    if(length(matchNum)==0){
      break
    }
    ts_new[matchNum]<-NA
    #
    plot(df_t$date,ts_new)
    ts_NA1<-as.numeric(ts_new)
    ###gapfilling with na.fill function
    temp1<-na.fill(ts_NA1,fill='extend')
    ts_new<-temp1
  }
  #set all the outliers(NA_ori) to NA
  ts_ori[c(pos_NA_ori,pos_noise)]<-NA



  return(ts_ori)
}

