
calc_swc_M<-function(M, ET, max_swc, min_swc, k, T_u, T_lf, T_lm, k_m, T, Precip, init_snowsize) {
  #calculate snowpack
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
      snowpack$rain[i] <- Precip[i]
    } else if (T[i] > T_lf & T[i] < T_u) {
      snowpack$accumulation[i] <- Precip[i]*((T_u-T[i])/(T_u-T_lf))
      snowpack$rain[i] <- Precip[i] - snowpack$accumulation[i]
    } else {
      snowpack$accumulation[i] <- Precip[i]
      snowpack$rain[i] <- 0
    }
  }
  snowpack$size[1]<- init_snowsize #initial snowpack size 
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
  #calculate soil water
  swc<-data.frame("change"=rep(NA, length(snowpack$rain)), 
                  "sum"=rep(NA, length(snowpack$rain)),
                  "Q"=rep(NA, length(snowpack$rain)),
                  "runoff"=rep(NA, length(snowpack$rain)),
                  "snowsize"=snowpack$size,
                  "snowmelt"=snowpack$actual_melt,
                  "snowaccumulation"=snowpack$accumulation) #output dataframe for the changing soil water
  swc$sum[1]<- 400 #initial soil water content
  if (swc$sum[1] > max_swc) {
    swc$sum[1] = max_swc
  }
  swc$change[1]<-0 #initial change is zero
  for(i in 2:length(swc$sum)){ #soil water content from day 2
    temp_Q<-swc$sum[i-1]*k*(swc$sum[i-1]/max_swc)^2 #calculate value for Q
    swc$Q[i]<-temp_Q
    if(max_swc > snowpack$rain[i] + snowpack$actual_melt[i] - temp_Q - ET[i]) # water available for swc is less than max_swc
    {
      swc$change[i]<- snowpack$rain[i] + snowpack$actual_melt[i] - temp_Q - ET[i] #calculate change in soil water
      swc$sum[i]<-swc$sum[i-1]+swc$change[i] #add change to sum
      if(swc$sum[i]<min_swc){ #if the sums get below the minimum
        swc$sum[i]=min_swc #set sum to minimum observed value
      }else if(swc$sum[i]>max_swc){ #if the sum exceeds max_swc 
        swc$sum[i]=max_swc #set sum to swc max
      } else{}#if sum positive and less than swc max, do nothing
    }else{ #if water available for swc is more than or equal to max swc
      #calculate change until max_swc is reached
      swc$change[i]<-max_swc-swc$sum[i-1] #calculate the maximum possible change 
      swc$sum[i]<-max_swc #sum is then max_swc
      theoretical_change <- snowpack$rain[i] + snowpack$actual_melt[i] - temp_Q - ET[i] #calculate the theoretical change
      excess_water<-theoretical_change-swc$change[i] #calculate the excess water
      swc$runoff<-snowpack$rain[i] + snowpack$actual_melt[i] - temp_Q - ET[i] - max_swc + excess_water #excess water goes into runoff
    }
  }
  return(swc)}

# define constants
max_swc_H = max(Hyytiala_all_day$SWC20) # max swc in % for Hyytiala
min_swc_H =min(Hyytiala_all_day$SWC20)
#test model
swc<-calc_swc_M(Precip=Hyytiala_all_day$Prec, ET=Hyytiala_all_day$Evapotr,
              max_swc=max_swc_H*1000, k=0.005, min_swc=min_swc_H*1000, T_u=5, 
              T_lf=-7, T_lm=-5, k_m=0.6, T=Hyytiala_all_day$AirT, init_snowsize=0)

#add column with date
swc$date<-Hyytiala_all_day$date
swc$date<-as.POSIXct(swc$date)
swc$obs<-Hyytiala_all_day$SWC20 #add observations to sim data

plot(swc$date, swc$snowsize, type="l")
