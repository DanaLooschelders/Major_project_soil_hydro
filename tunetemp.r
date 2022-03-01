#tune for temperature
#tune snowpack model based on rmse and r2
tempvalues<-seq(-2, 10, by=0.5)
optimaltemp_snow<-data.frame("tempvalue"=tempvalues, "RMSE"=NA, "rsquared"=NA)

for(i in tempvalues){
  #model once with every tempvalue
  swc_temp<-calc_swc_M(Precip=Hyytiala_all_day$Prec, M=snowpack$actual_melt, ET=Hyytiala_all_day$Evapotr,
                  max_swc=max_swc_H*1000, k=0.005, min_swc=min_swc_H*1000, T_u=4, 
                  T_lf=-6, T_lm=i, k_m=4, T=Hyytiala_all_day$AirT) 
  swc_temp$sum<-swc_temp$sum/1000
  swc_temp$obs<-Hyytiala_all_day$SWC20 #add observations to sim data
  #calculate RMSE for model
  optimaltemp_snow$RMSE[optimaltemp_snow$tempvalue==i]<-sqrt(mean((swc_temp$obs - swc_temp$sum)^2))
  #calculate linear fit
  optimaltemp_snow$rsquared[optimaltemp_snow$tempvalue==i]<-summary(lm(swc_temp$sum/1000~swc_temp$obs))[["adj.r.squared"]]
}

#get optimal temp value
optimaltemp_snow$tempvalue[which.min(optimaltemp_snow$RMSE)] #for rmse
optimaltemp_snow$tempvalue[which.max(optimaltemp_snow$rsquared)] #for rsquared

#plot
setwd("C:/00_Dana/Uni/3. Mastersemester (Erasmus)/Ecosystem Modelling/Major_Project/Database")

ggplot(data=optimaltemp_snow, aes(x=tempvalue, y=RMSE))+
  geom_line()+
  theme_bw()+
  ggtitle(label="RMSE tuned for k_m", 
          subtitle = "melt in mm per degrees celcius")+
  theme(text=element_text(size=8))+
  xlab(label="Temperature [°C]")
ggsave(filename="RMSE_k_m.jpg", width = 20, height=12, units = "cm")

range(Hyytiala_all_day$AirT)
