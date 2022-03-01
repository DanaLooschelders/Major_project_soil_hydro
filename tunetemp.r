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

#####
tuvalues<-seq(-2, 7, by=1) #max temp above which is precipitation
tlfvalues<-seq(-9, 2, by=1) #lower limit below preci will be snow
tlmvalues<-seq(-15, 2, by=1) #temp limit above which snow can melt
kmvalue<-seq(0, 5, by=1) #rate snow melts
kvalues<-seq(0, 2, by=0.2 ) #k value

optimal_snow<-expand.grid(tuvalues, tlfvalues, tlmvalues, kmvalue, kvalues)
colnames(optimal_snow)<-c("tuvalues", "tlfvalues", "tlmvalues", "kmvalue", "kvalues")
optimal_snow$RMSE<-NA

for(i in 1:dim(optimal_snow)[1]){
    #model once with every tempvalue
    swc_temp<-calc_swc_M(Precip=Hyytiala_all_day$Prec, ET=Hyytiala_all_day$Evapotr,
                         max_swc=max_swc_H*1000, k=optimal_snow$kvalues[i], min_swc=min_swc_H*1000, 
                         T_u=optimal_snow$tuvalues[i], 
                         T_lf=optimal_snow$tlfvalues[i], T_lm=optimal_snow$tlmvalues[i], 
                         k_m=optimal_snow$kmvalue[i], 
                         T=Hyytiala_all_day$AirT, init_snowsize=0) 
    swc_temp$sum<-swc_temp$sum/1000
    swc_temp$obs<-Hyytiala_all_day$SWC20 #add observations to sim data
    #calculate RMSE for model
    optimal_snow$RMSE[i]<-sqrt(mean((swc_temp$obs - swc_temp$sum)^2))
  }

optimal_values<-optimal_snow[which.min(optimal_snow$RMSE),]
optimal_values_order<-optimal_snow[order(optimal_snow$RMSE),]
optimal_values_nonzero<-optimal_values_order[optimal_values_order$kmvalue>0&optimal_values_order$kvalues>0,]
head(optimal_values_nonzero, n = 5)
