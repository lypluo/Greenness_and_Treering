simpleWAI <- function(preci, pet, theta=0.05, awc=100){ 
  library(zoo)
  # preci<-df$Rain
  # pet<-df$PET
  # theta=0.5
  # awc = 100
  
  n <- length(preci) 
  
  WAI <- rep(NA, n) 
  WAI[1] <- awc 
  
  ##if is.na(preci) or preci<0 ,set preci==s0
  for(i in 1:n){
    if(is.na(preci[i])|preci[i]<0){
      preci[i]=0
    }
  }
  #keep the NA if ET is at the start or end of time series,extrapolate the NA within the time series
  pet<-na.fill(pet,c(NA,'extend',NA))
  
  ETmodWAI <- rep(NA, n) 
  input <- rep(NA, n) 
  
  for (i in 1:(n-1)) { 
    input[i] <- min(preci[i], awc - WAI[i]) #Recharge = min (preci, water deficit previous state) 
    ##after read the refered literature (Tramontana et al.,2016),I updated the the formula of ETsupply by adding input[i] behind WAI[i]
    ETsupply <- theta * (WAI[i]+input[i]) #Evapotranspiration supply, theta is a parameter from literature 
    ETmodWAI[i] <- min(pet[i], ETsupply) 
    WAI[i+1] <- WAI[i] + input[i] - ETmodWAI[i] 
    
    # if (is.na(WAI[i+1]))
    # {
    #   WAI[i+1] <- WAI[i]
    # }
    
  } 
  
  data.frame(WAI=WAI, ETmodWAI=ETmodWAI, input=input) 
} 
