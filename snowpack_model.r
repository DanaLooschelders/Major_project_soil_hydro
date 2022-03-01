snow_accumulation <- function(T_u, T_lf, T_lm, k_m, T, Precip) {
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
  snowpack$size[1]<- 5 #initial snowpack size to 5 mm
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

snowpack <- snow_accumulation(T_u=5, T_lf=-7, T_lm=-5, k_m=4, T=Hyytiala_all_day$AirT, P=Hyytiala_all_day$Prec)
