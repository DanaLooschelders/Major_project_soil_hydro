
calc_swc_M<-function(M, ET, max_swc_layer1, max_swc_layer2, max_swc_layer3, min_swc, k, T_u, T_lf, T_lm, k_m, T, Precip, init_snowsize, ET_change, swc_m, swc_fc,k_s1, k_sl1, 
                     k_sl2, k_sl3, k_s21, k_s22, k_s23, k_u1, k_u2, k_u3,
                     k_u21, k_u22, k_u23) {
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
  #change ET based on snow cover
  ET[which(snowpack$size==0)]<-ET[which(snowpack$size==0)]+ET_change
  #calculate soil water
  swc<-data.frame("change_layer1"=rep(NA, length(snowpack$rain)),
                  "change_layer2"=rep(NA, length(snowpack$rain)),
                  "change_layer3"=rep(NA, length(snowpack$rain)),
                  "sum_layer1"=rep(NA, length(snowpack$rain)),
                  "sum_layer2"=rep(NA, length(snowpack$rain)),
                  "sum_layer3"=rep(NA, length(snowpack$rain)),
                  "Q"=rep(NA, length(snowpack$rain)),
                  "runoff"=rep(NA, length(snowpack$rain)),
                  "infiltration"=rep(NA, length(snowpack$rain)),
                  "snowsize"=snowpack$size,
                  "snowmelt"=snowpack$actual_melt,
                  "snowaccumulation"=snowpack$accumulation,
                  "rain"=snowpack$rain) #output dataframe for the changing soil water
  swc$sum_layer1[1]<- 270 #initial soil water content
  swc$sum_layer2[1]<- 400 #initial soil water content
  swc$sum_layer3[1]<- 440 #initial soil water content
  if (swc$sum_layer1[1] > max_swc_layer1) {
    swc$sum_layer1[1] = max_swc_layer1
  }
  if (swc$sum_layer2[1] > max_swc_layer2) {
    swc$sum_layer2[1] = max_swc_layer2
  }
  if (swc$sum_layer3[1] > max_swc_layer3) {
    swc$sum_layer3[1] = max_swc_layer3
  }
  swc$change_layer1[1] <-0 #initial change is zero
  swc$change_layer2[1] <-0 
  swc$change_layer3[1] <-0 
  for(i in 2:length(swc$sum)){ #soil water content from day 2
    swc$runoff[i] <- swc$snowmelt[i] + swc$rain[i] * ((swc$sum[i-1]-swc_m)/(swc_s-swc_m))^k_s1
    swc$infiltration[i] <- swc$snowmelt[i] + swc$rain[i] - swc$runoff[i]
    swc$sum_layer1[i] <- swc$sum_layer1[i-1] + swc$infiltration[i]
    
    if (swc$sum_layer1[i] > swc_fc) {
      q_sat <- (swc$sum_layer1[i] - swc_fc) * k_s1 * ((swc$sum_layer1-swc_fc)/(max_swc_layer1-swc_fc))^k_s2i
    }
    }
  return(swc)
}

# define constants
max_swc_H_layer1 = max(Hyytiala_all_day$SWC10) # max swc in % for Hyytiala
max_swc_H_layer2 = max(Hyytiala_all_day$SWC20) # max swc in % for Hyytiala
max_swc_H_layer3 = max(Hyytiala_all_day$SWC50) # max swc in % for Hyytiala
min_swc_H =min(Hyytiala_all_day$SWC20)

#test model
swc<-calc_swc_M(Precip=Hyytiala_all_day$Prec, ET=Hyytiala_all_day$Evapotr,
                max_swc_layer1=max_swc_H_layer1*1000, max_swc_layer2=max_swc_H_layer2*1000,
                max_swc_layer3=max_swc_H_layer3*1000, k=0.005, min_swc=min_swc_H*1000, T_u=4, 
                T_lf=2, T_lm=-4, k_m=6, T=Hyytiala_all_day$AirT, init_snowsize=0,
                ET_change=0.7, swc_m=0.5*1000, swc_fc=0.363*1000, k_s1=1.7, k_sl1=0.5, 
                k_sl2=0.8, k_sl3=0.8, k_s21=1, k_s22=2.5, k_s23=10, k_u1=0.8, k_u2=0.5, k_u3=0.2,
                k_u21=2.5, k_u22=3, k_u23=10)


#add column with date
swc$date<-Hyytiala_all_day$date
swc$date<-as.POSIXct(swc$date)
swc$obs<-Hyytiala_all_day$SWC20 #add observations to sim data


