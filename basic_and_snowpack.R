snow_accumulation <- function(T_u, T_lf, T_lm, k_m, T, P) {
  snowpack<-data.frame("accumulation"=rep(NA, length(T)), 
                  "size"=rep(NA, length(T)),
                  "theo_melt"=rep(NA, length(T)),
                  "actual_melt"=rep(NA, length(T)),
                  "rain"=rep(NA,length(T))) #output dataframe for the changing snowpack
  
  for (i in 1:length(T)) {
    # calculate snowpack accumulation and size
    if (T_lm >= T[i]) {
      snowpack$theo_melt[i] <- 0 
    } else {
      snowpack$theo_melt[i] <- k_m*(T[i]-T_lm)
    }
    if (T[i] >= T_u) {
      snowpack$accumulation[i] <- 0
      snowpack$rain[i] <- P[i]
    } else if (T[i] > T_lf & T[i] < T_u) {
      snowpack$accumulation[i] <- P[i]*((T_u-T[i])/(T_u-T_lf))
      snowpack$rain[i] <- P[i] - snowpack$accumulation[i]
    } else {
      snowpack$accumulation[i] <- P[i]
      snowpack$rain[i] <- 0
    }
  }
  snowpack$size[1]<- 50 #initial snowpack size to 0 mm
  for (i in 2:length(T)) {
    snowpack$size[i] <- snowpack$size[i-1] + snowpack$accumulation[i]
    if (snowpack$theo_melt[i] > snowpack$size[i]) {
      snowpack$actual_melt[i] <- snowpack$size[i]
      snowpack$size[i] <- 0
    } else {
      snowpack$actual_melt[i] <- snowpack$theo_melt[i]
      snowpack$size[i] <- snowpack$size[i-1] + snowpack$accumulation[i] - snowpack$actual_melt[i]
    }
    if (snowpack$size[i] < 0) {
      snowpack$size[i] <- 0
    }
  }
  return(snowpack)
}

snowpack <- snow_accumulation(T_u=5, T_lf=-7, T_lm=-14, k_m=4, T=Hyytiala_all_day$AirT, P=Hyytiala_all_day$Prec)

calc_swc_M<-function(P, M, ET, max_swc, k) {
  swc<-data.frame("change"=rep(NA, length(P)), 
                  "sum"=rep(NA, length(P)),
                  "runoff"=rep(NA, length(P))) #output dataframe for the changing soil water
  swc$sum[1]<- 0.4*200 #initial soil water content
  if (swc$sum[1] > max_swc) {
    swc$sum[1] = max_swc
  }
  swc$change[1]<-0 #initial change is zero
  for(i in 2:length(swc$sum)){ #soil water content from day 2
    temp_Q<-swc$sum[i-1]*k*(swc$sum[i-1]/max_swc)^2 #calculate value for Q
    if(max_swc > P[i] + M[i] - temp_Q - ET[i]) # water available for swc is less than max_swc
    {
      swc$change[i]<- P[i] + M[i] - temp_Q - ET[i] #calculate change in soil water
      swc$sum[i]<-swc$sum[i-1]+swc$change[i] #add change to sum
      if(swc$sum[i]<0){ #if the sums turns negative
        swc$sum[i]=0 #set sum to zero
      }else if(swc$sum[i]>max_swc){ #if the sum exceeds max_swc 
        swc$sum[i]=max_swc #set sum to swc max
      } else{}#if sum positive and less than swc max, do nothing
    }else{ #if water available for swc is more than or equal to max swc
      #calculate change until max_swc is reached
      swc$change[i]<-max_swc-swc$sum[i-1] #calculate the maximum possible change 
      swc$sum[i]<-max_swc #sum is then max_swc
      theoretical_change <- P[i] + M[i] - temp_Q - ET[i] #calculate the theoretical change
      excess_water<-theoretical_change-swc$change[i] #calculate the excess water
      swc$runoff<-P[i] + M[i] - temp_Q - ET[i] - max_swc + excess_water #excess water goes into runoff
    }
  }
  return(swc)}

# define constants
h = 200 # set soil depth in mm
max_swc_H = 0.5 # max swc in % for Hyytiala

#test model
swc<-calc_swc_M(P=snowpack$rain, M=snowpack$actual_melt, ET=Hyytiala_all_day$Evapotr,
              max_swc=max_swc_H*h, k=0.02)
#add column with date
swc$date<-Hyytiala_all_day$date
swc$date<-as.POSIXct(swc$date)
swc$obs<-Hyytiala_all_day$SWC20 #add observations to sim data

