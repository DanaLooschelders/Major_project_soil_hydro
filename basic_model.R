library(tidyverse)
library(readxl)


# set constant parameters
max_swc_H <- 90 # water holding capacity for Hyytiala
max_swc_N <- 90  # water holding capacity for Norunda
k <- 0.7

# function to check for negative or more than max soil water capacity
check_swc <- function(check_swc, max_swc) {
  if (check_swc < 0) {
    check_swc <- 0
  } else if (check_swc > max_swc) {
    check_swc <- max_swc
  }
  return(check_swc)
}
  
# basic model for calculating soil water content
calc_swc <- function(P, ET, max_swc, k) {
  swc <- P[1] - ET[1]
  swc <- check_swc(swc, max_swc)
  Q <- 0
  for (i in 2:length(P)) {
    Q[i] <- swc[i-1]*k*(swc[i-1]/max_swc)^2
    if (max_swc < P[i] - Q[i] - ET[i]) {
      swc[i] <- max_swc - swc[i-1]
      swc[i] <- check_swc(swc[i], max_swc)
  } else {
      swc[i] <- swc[i-1] + P[i] - ET[i] - Q[i]
      swc[i] <- check_swc(swc[i], max_swc)
    }
  }
  return(swc)
}

swc_H_1999day <- calc_swc(H_1999_day$Prec, H_1999_day$Evapotr, max_swc_H, k)
swc_N_1997day <- calc_swc(N_1997_day$Prec, N_1997_day$Evapotr, max_swc_N, k)
swc_N_1999day <- calc_swc(N_1999_day$Prec, N_1999_day$Evapotr, max_swc_N, k)

#basic model from Dana
# groundwater recharge with soil water 𝜃 as state variable
# ∆𝜃 =𝑃−𝐸−𝑄−𝑅
# Q = 𝜃k(𝜃/𝜃max)^2
calc_swc<-function(P, ET, init_swc, max_swc, k) {
  swc<-data.frame("change"=rep(NA, length(P))) #output dataframe for the changing soil water
  swc$sum<-rep(NA, length(P)) #output for total soil water content
  swc$sum[1]<-init_swc #initial soil water content
  swc$change[1]<-0 #initial change is zero
  swc$runoff<-rep(NA, length(P)) #output for excess soil water (runoff)
  for(i in 2:length(swc$sum)){ #soil water content from day 2
    temp_Q<-swc$sum[i-1]*k*(swc$sum[i-1]/max_swc)^2 #calculate value for Q
    if(max_swc > P[i] - temp_Q - ET[i]) # water available for swc is less than max_swc
    {
     swc$change[i]<- P[i] - temp_Q - ET[i] #calculate change in soil water
     swc$sum[i]<-swc$sum[i-1]+swc$change[i] #add change to sum
    }else{ #if water available for swc is more or equal to than max swc
      swc$change[i]<-0
      swc$sum[i]<-swc$sum[i-1]
      swc$runoff<-P[i] - temp_Q - ET[i] - max_swc
    }
  
  }
return(swc)}

#test model
swc<-calc_swc(P=Hyytiala1999day$Prec, ET=Hyytiala1999day$Evapotr, 
               init_swc=0, max_swc=33.5, k=0.5)
#plot results
plot(swc$change, type="l") 
plot(swc$sum, type="l")
abline(h = 33.5, col="red")
