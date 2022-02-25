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
function(P, ET, init_swc, max_swc, k) {
  swc_change<-data.frame("swc"=rep(NA, length(P))) #create an output dataframe for the changing soil water
  swc_change[1]<-init_swc #initial soil water content
  swc_sum<-data.frame("swc"=rep(NA, length(P))) #output for total soil water content
  for(i in 2:length(swc_change)){ #soil water content from day 2
    temp_Q<-swc_change[i-1]*k*(swc_change[i-1]/max_swc)^2 #calculate value for Q
    if(max_swc < P[i] - temp_Q - ET[i]) # water available for swc is less than max_swc
    {
     swc_change[i]<- P[i] - temp_Q - ET[i] #calculate change in soil water
     swc_sum[i]<-swc_sum[i-1]+swc_change[i] #add change to sum
    }else{ #if water available for swc is more or equal to than max swc
      
    }
  
  }
}
