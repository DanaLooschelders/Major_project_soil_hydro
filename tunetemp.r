#tune for temperature
#tune snowpack model based on rmse and r2
tempvalues<-seq(0, 100, by=1)
othervalues<-seq(-0.2, 5, by=0.1 )
optimaltemp_snow<-data.frame("tempvalue"=rep(tempvalues, length(othervalues)),
                             "othervalue"=rep(othervalues, each=length(tempvalues)),
                             "RMSE"=NA, "rsquared"=NA)

for(i in tempvalues){
  for(x in othervalue){
  #model once with every tempvalue
  swc_temp<-calc_swc_M(Precip=Hyytiala_all_day$Prec, M=snowpack$actual_melt, ET=Hyytiala_all_day$Evapotr,
                  max_swc=max_swc_H*1000, k=0.005, min_swc=min_swc_H*1000, T_u=4, 
                  T_lf=-6, T_lm=-5, k_m=x, T=Hyytiala_all_day$AirT, init_snowsize=0) 
  swc_temp$sum<-swc_temp$sum/1000
  swc_temp$obs<-Hyytiala_all_day$SWC20 #add observations to sim data
  #calculate RMSE for model
  optimaltemp_snow$RMSE[optimaltemp_snow$tempvalue==i&optimaltemp_snow$othervalue==x]<-sqrt(mean((swc_temp$obs - swc_temp$sum)^2))
  #calculate linear fit
  optimaltemp_snow$rsquared[optimaltemp_snow$tempvalue==i&optimaltemp_snow$othervalue==x]<-summary(lm(swc_temp$sum~swc_temp$obs))[["adj.r.squared"]]
}
}

#get optimal temp value
optimaltemp_snow$tempvalue[which.min(optimaltemp_snow$RMSE)] #for rmse
optimaltemp_snow$othervalue[which.min(optimaltemp_snow$RMSE)]
optimaltemp_snow$tempvalue[which.max(optimaltemp_snow$rsquared)] #for rsquared

#plot
setwd("C:/00_Dana/Uni/3. Mastersemester (Erasmus)/Ecosystem Modelling/Major_Project/Database")

ggplot(data=optimaltemp_snow, aes(x=tempvalue, y=RMSE))+
  geom_line()+
  theme_bw()+
  #ggtitle(label="RMSE tuned for k_m", 
  #        subtitle = "melt in mm per degrees celcius")+
  theme(text=element_text(size=8))+
  xlab(label="Temperature [°C]")
ggsave(filename="RMSE_k_m.jpg", width = 20, height=12, units = "cm")

range(Hyytiala_all_day$AirT)

#####tune for all variables#####
#tuvalues<-seq(-2, 7, by=1) #max temp above which is precipitation
#tlfvalues<-seq(-9, 2, by=1) #lower limit below preci will be snow
#tlmvalues<-seq(-15, 2, by=1) #temp limit above which snow can melt
kmvalue<-seq(0.5, 5, by=0.5) #rate snow melts
k_winter<-seq(0, 0.01, by=0.001 ) #k value for winter
k_summer<-seq(0, 0.15, by=0.01 ) #k value for summer
#e_winter<-seq(-1, 0, by=0.1) #ET decrease for winter 
#e_summer<-seq(0, 1, by=0.1) #ET increase for summer

#optimal_snow<-expand.grid(tuvalues, tlfvalues, tlmvalues, kmvalue, kvalues)
optimal_snow<-expand.grid(kmvalue, k_winter, k_summer)
#colnames(optimal_snow)<-c("tuvalues", "tlfvalues", "tlmvalues", "kmvalue", "kvalues")
colnames(optimal_snow)<-c("kmvalue", "k_winter", "k_summer")
optimal_snow$RMSE<-NA

for(i in 1:dim(optimal_snow)[1]){
    #model once with every tempvalue
    swc_temp<-calc_swc_M(Precip=Hyytiala_calibration$Prec, ET=Hyytiala_calibration$Evapotr,
                         max_swc=max_swc_H*1000, k_winter=optimal_snow$k_winter[i], 
                         k_summer=optimal_snow$k_summer[i], min_swc=min_swc_H*1000, T_u=4, 
                         T_lf=-7, T_lm=-5, k_m=optimal_snow$kmvalue[i], T=Hyytiala_calibration$AirT, init_snowsize=0,
                         ET_summer=0, ET_winter=0, 
                         month=Hyytiala_calibration$Month) 
    
    swc_temp$sum<-swc_temp$sum/1000
    swc_temp$obs<-Hyytiala_calibration$SWC20 #add observations to sim data
    #calculate RMSE for model
    optimal_snow$RMSE[i]<-sqrt(mean((swc_temp$obs - swc_temp$sum)^2))
  }

#dim(optimal_snow)[1]/10*0.69/60/60 was 2.45

optimal_values<-optimal_snow[which.min(optimal_snow$RMSE),]
optimal_values_order<-optimal_snow[order(optimal_snow$RMSE),]
#optimal_values_nonzero<-optimal_values_order[optimal_values_order$kmvalue>0&optimal_values_order$kvalues>0,]
head(optimal_values_order, n = 15)

####tune for km value####
tempvalue<-seq(0, 5, by=1) #rate snow melts
optimaltemp_snow<-data.frame("tempvalue"=tempvalue,"RMSE"=NA)

for(i in tempvalue){
    #model once with every tempvalue
    swc_temp<-calc_swc_M(Precip=Hyytiala_calibration$Prec, ET=Hyytiala_calibration$Evapotr,
                         max_swc=max_swc_H*1000, k_winter=0.004, 
                         k_summer=0.05, min_swc=min_swc_H*1000, T_u=4, 
                         T_lf=-7, T_lm=-5, k_m=i, T=Hyytiala_calibration$AirT, 
                         init_snowsize=0,
                         ET_summer=0, ET_winter=0, 
                         month=Hyytiala_calibration$Month) 
    swc_temp$sum<-swc_temp$sum/1000
    swc_temp$obs<-Hyytiala_calibration$SWC20 #add observations to sim data
    #calculate RMSE for model
    optimaltemp_snow$RMSE[optimaltemp_snow$tempvalue==i]<-sqrt(mean((swc_temp$obs - swc_temp$sum)^2))
    #calculate linear fit
    #optimaltemp_snow$rsquared[optimaltemp_snow$tempvalue==i&optimaltemp_snow$othervalue==x]<-summary(lm(swc_temp$sum~swc_temp$obs))[["adj.r.squared"]]
  }


#get optimal temp value
optimaltemp_snow$tempvalue[which.min(optimaltemp_snow$RMSE)] #for rmse
#plot
ggplot(data=optimaltemp_snow, aes(x=tempvalue, y=RMSE))+
  geom_line()+
  theme_bw()+
  #ggtitle(label="RMSE tuned for k_m", 
  #        subtitle = "melt in mm per degrees celcius")+
  theme(text=element_text(size=8))+
  xlab(label="km value")
#ggsave(filename="RMSE_k_m.jpg", width = 20, height=12, units = "cm")


####tune for k_winter#### 
tempvalue<-seq(0, 0.01, by=0.001 ) #k value for winter
optimaltemp_snow<-data.frame("tempvalue"=tempvalue,"RMSE"=NA)

for(i in tempvalue){
  #model once with every tempvalue
  swc_temp<-calc_swc_M(Precip=Hyytiala_calibration$Prec, ET=Hyytiala_calibration$Evapotr,
                       max_swc=max_swc_H*1000, k_winter=i, 
                       k_summer=0.05, min_swc=min_swc_H*1000, T_u=4, 
                       T_lf=-7, T_lm=-5, k_m=0, T=Hyytiala_calibration$AirT, 
                       init_snowsize=0,
                       ET_summer=0, ET_winter=-1, 
                       month=Hyytiala_calibration$Month) 
  swc_temp$sum<-swc_temp$sum/1000
  swc_temp$obs<-Hyytiala_calibration$SWC20 #add observations to sim data
  #calculate RMSE for model
  optimaltemp_snow$RMSE[optimaltemp_snow$tempvalue==i]<-sqrt(mean((swc_temp$obs - swc_temp$sum)^2))
  #calculate linear fit
  #optimaltemp_snow$rsquared[optimaltemp_snow$tempvalue==i&optimaltemp_snow$othervalue==x]<-summary(lm(swc_temp$sum~swc_temp$obs))[["adj.r.squared"]]
}

#get optimal temp value
optimaltemp_snow$tempvalue[which.min(optimaltemp_snow$RMSE)] #for rmse
#plot
ggplot(data=optimaltemp_snow, aes(x=tempvalue, y=RMSE))+
  geom_line()+
  theme_bw()+
  #ggtitle(label="RMSE tuned for k_m", 
  #        subtitle = "melt in mm per degrees celcius")+
  theme(text=element_text(size=8))+
  xlab(label="k winter")
#ggsave(filename="RMSE_k_m.jpg", width = 20, height=12, units = "cm")

#####tune for k_summer####
tempvalue<-seq(0, 0.15, by=0.01 ) #k value for summer#k value for winter
optimaltemp_snow<-data.frame("tempvalue"=tempvalue,"RMSE"=NA)

for(i in tempvalue){
  #model once with every tempvalue
  swc_temp<-calc_swc_M(Precip=Hyytiala_calibration$Prec, ET=Hyytiala_calibration$Evapotr,
                       max_swc=max_swc_H*1000, k_winter=0.004, 
                       k_summer=i, min_swc=min_swc_H*1000, T_u=4, 
                       T_lf=-7, T_lm=-5, k_m=0, T=Hyytiala_calibration$AirT, 
                       init_snowsize=0,
                       ET_summer=0, ET_winter=-1, 
                       month=Hyytiala_calibration$Month) 
  swc_temp$sum<-swc_temp$sum/1000
  swc_temp$obs<-Hyytiala_calibration$SWC20 #add observations to sim data
  #calculate RMSE for model
  optimaltemp_snow$RMSE[optimaltemp_snow$tempvalue==i]<-sqrt(mean((swc_temp$obs - swc_temp$sum)^2))
  #calculate linear fit
  #optimaltemp_snow$rsquared[optimaltemp_snow$tempvalue==i&optimaltemp_snow$othervalue==x]<-summary(lm(swc_temp$sum~swc_temp$obs))[["adj.r.squared"]]
}

#get optimal temp value
optimaltemp_snow$tempvalue[which.min(optimaltemp_snow$RMSE)] #for rmse
#plot
ggplot(data=optimaltemp_snow, aes(x=tempvalue, y=RMSE))+
  geom_line()+
  theme_bw()+
  #ggtitle(label="RMSE tuned for k_m", 
  #        subtitle = "melt in mm per degrees celcius")+
  theme(text=element_text(size=8))+
  xlab(label="k summer")
#ggsave(filename="RMSE_k_m.jpg", width = 20, height=12, units = "cm")

#####tune for e_winter####
tempvalue<-seq(-1, 0, by=0.1) #ET decrease for winter
optimaltemp_snow<-data.frame("tempvalue"=tempvalue,"RMSE"=NA)

for(i in tempvalue){
  #model once with every tempvalue
  swc_temp<-calc_swc_M(Precip=Hyytiala_calibration$Prec, ET=Hyytiala_calibration$Evapotr,
                       max_swc=max_swc_H*1000, k_winter=0.004, 
                       k_summer=0.05, min_swc=min_swc_H*1000, T_u=4, 
                       T_lf=-7, T_lm=-5, k_m=0, T=Hyytiala_calibration$AirT, 
                       init_snowsize=0,
                       ET_summer=0, ET_winter=i, 
                       month=Hyytiala_calibration$Month) 
  swc_temp$sum<-swc_temp$sum/1000
  swc_temp$obs<-Hyytiala_calibration$SWC20 #add observations to sim data
  #calculate RMSE for model
  optimaltemp_snow$RMSE[optimaltemp_snow$tempvalue==i]<-sqrt(mean((swc_temp$obs - swc_temp$sum)^2))
  #calculate linear fit
  #optimaltemp_snow$rsquared[optimaltemp_snow$tempvalue==i&optimaltemp_snow$othervalue==x]<-summary(lm(swc_temp$sum~swc_temp$obs))[["adj.r.squared"]]
}

#get optimal temp value
optimaltemp_snow$tempvalue[which.min(optimaltemp_snow$RMSE)] #for rmse
#plot
ggplot(data=optimaltemp_snow, aes(x=tempvalue, y=RMSE))+
  geom_line()+
  theme_bw()+
  #ggtitle(label="RMSE tuned for k_m", 
  #        subtitle = "melt in mm per degrees celcius")+
  theme(text=element_text(size=8))+
  xlab(label="ET winter")
#ggsave(filename="RMSE_k_m.jpg", width = 20, height=12, units = "cm")

#####tune for e_summer####
tempvalue<-seq(-5, 1, by=0.1) #ET increase for summer
optimaltemp_snow<-data.frame("tempvalue"=tempvalue,"RMSE"=NA)

for(i in tempvalue){
  #model once with every tempvalue
  swc_temp<-calc_swc_M(Precip=Hyytiala_calibration$Prec, ET=Hyytiala_calibration$Evapotr,
                       max_swc=max_swc_H*1000, k_winter=0.004, 
                       k_summer=0.05, min_swc=min_swc_H*1000, T_u=4, 
                       T_lf=-7, T_lm=-5, k_m=1, T=Hyytiala_calibration$AirT, 
                       init_snowsize=0,
                       ET_summer=i, ET_winter=0, 
                       month=Hyytiala_calibration$Month) 
  swc_temp$sum<-swc_temp$sum/1000
  swc_temp$obs<-Hyytiala_calibration$SWC20 #add observations to sim data
  #calculate RMSE for model
  optimaltemp_snow$RMSE[optimaltemp_snow$tempvalue==i]<-sqrt(mean((swc_temp$obs - swc_temp$sum)^2))
  #calculate linear fit
  #optimaltemp_snow$rsquared[optimaltemp_snow$tempvalue==i&optimaltemp_snow$othervalue==x]<-summary(lm(swc_temp$sum~swc_temp$obs))[["adj.r.squared"]]
}

#get optimal temp value
optimaltemp_snow$tempvalue[which.min(optimaltemp_snow$RMSE)] #for rmse
#plot
ggplot(data=optimaltemp_snow, aes(x=tempvalue, y=RMSE))+
  geom_line()+
  theme_bw()+
  #ggtitle(label="RMSE tuned for k_m", 
  #        subtitle = "melt in mm per degrees celcius")+
  theme(text=element_text(size=8))+
  xlab(label="E summer factor")
#ggsave(filename="RMSE_k_m.jpg", width = 20, height=12, units = "cm")

begin<-Hyytiala_validation$date[1]
end<-Hyytiala_validation$date[length(Hyytiala_validation$date)]
T_min<-min(Hyytiala_validation$AirT)
T_max<-max(Hyytiala_validation$AirT)
ggplot()+
  geom_line(data=Hyytiala_validation, aes(x=date, y=AirT), color="darkblue")+
  #geom_hline(aes(yintercept=335, col="max water content"))+
  labs(color="")+
  xlab(label="Date")+
  ylab(label="Temperature [°C]")+
  ggtitle(label="Temperature in Hyytiala", subtitle="1999 to 2001")+
  theme_bw()+
  theme(text=element_text(size=10), legend.position = "bottom")+
  annotate("rect", xmin=begin, xmax= end, ymin = T_min, ymax = -7, alpha = 0.2, fill = "green") +
  annotate("rect", xmin=begin, xmax= end, ymin = -7, ymax = -5, alpha = 0.2, fill = "orange") +
  annotate("rect", xmin=begin, xmax= end, ymin = -5, ymax = 4, alpha = 0.2, fill = "red")+
  annotate("rect", xmin=begin, xmax= end, ymin = 4, ymax = T_max, alpha = 0.2, fill = "darkred")
  scale_fill_manual(values = c("green", "orange", "red", "darkred"), 
                    labels=c("A", "B", "C", "D"),
                    breaks = c(T_min, -7, -5, 4))
  
  ####Norunda####
  Norunda_validation$date<-as.POSIXct(Norunda_validation$date)
  begin<-as.POSIXct(Norunda_validation$date[1])
  end<-as.POSIXct(Norunda_validation$date[length(Norunda_validation$date)])
  T_min<-min(Norunda_validation$AirT)
  T_max<-max(Norunda_validation$AirT)
  ggplot()+
    geom_line(data=Norunda_validation, aes(x=date, y=AirT), color="darkblue")+
    #geom_hline(aes(yintercept=335, col="max water content"))+
    labs(color="")+
    xlab(label="Date")+
    ylab(label="Temperature [°C]")+
    ggtitle(label="Temperature in Norunda", subtitle="1999 to 2001")+
    theme_bw()+
    theme(text=element_text(size=10), legend.position = "bottom")+
    annotate("rect", xmin=begin, xmax= end, ymin = T_min, ymax = -7, alpha = 0.2, fill = "green") +
    annotate("rect", xmin=begin, xmax= end, ymin = -7, ymax = -5, alpha = 0.2, fill = "orange") +
    annotate("rect", xmin=begin, xmax= end, ymin = -5, ymax = 4, alpha = 0.2, fill = "red")+
    annotate("rect", xmin=begin, xmax= end, ymin = 4, ymax = T_max, alpha = 0.2, fill = "darkred")
  scale_fill_manual(values = c("green", "orange", "red", "darkred"), 
                    labels=c("A", "B", "C", "D"),
                    breaks = c(T_min, -7, -5, 4))
  
