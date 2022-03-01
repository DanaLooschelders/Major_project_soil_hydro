#tune model for optimal rmse
kvalues<-seq(0.1, 2, by=0.1)
optimalk_basic<-data.frame("kvalue"=kvalues, "RMSE"=NA, "rsquared"=NA)

#tune basic model based on rmse and r2
for(i in kvalues){
  #model once with every kvalue
  swc_temp<-calc_swc(P=Hyytiala_all_day$Prec, ET=Hyytiala_all_day$Evapotr,
           max_swc=max_swc_H*h, k=i)
  swc_temp$obs<-Hyytiala_all_day$SWC20 #add observations to sim data
  #calculate RMSE for model
  optimalk_basic$RMSE[optimalk_basic$kvalue==i]<-  sqrt(mean((swc_temp$obs - swc_temp$sum/h)^2))
  #calculate linear fit
  optimalk_basic$rsquared[optimalk_basic$kvalue==i]<- summary(lm(swc_temp$sum/h~swc_temp$obs))[["adj.r.squared"]]
}

#get optimal k value
optimalk_basic$kvalue[which.min(optimalk_basic$RMSE)] #for rmse
optimalk_basic$kvalue[which.max(optimalk_basic$rsquared)] #for rsquared


#tune snowpack model based on rmse and r2
kvalues<-seq(-0.2, 2, by=0.005)
optimalk_snow<-data.frame("kvalue"=kvalues, "RMSE"=NA, "rsquared"=NA)

for(i in kvalues){
  #model once with every kvalue
  swc_temp<-calc_swc_M(P=snowpack$rain, M=snowpack$actual_melt, ET=Hyytiala_all_day$Evapotr,
             max_swc=max_swc_H*1000, k=i, min_swc=min_swc_H*1000, T_u=4, 
             T_lf=-6, T_lm=-5, k_m=4, T=Hyytiala_all_day$AirT)
  swc_temp$sum<-swc_temp$sum/1000
  swc_temp$obs<-Hyytiala_all_day$SWC20 #add observations to sim data
  #calculate RMSE for model
  optimalk_snow$RMSE[optimalk_snow$kvalue==i]<-  sqrt(mean((swc_temp$obs - swc_temp$sum/1000)^2))
  #calculate linear fit
  optimalk_snow$rsquared[optimalk_snow$kvalue==i]<- summary(lm(swc_temp$sum/1000~swc_temp$obs))[["adj.r.squared"]]
}

#get optimal k value
optimalk_snow$kvalue[which.min(optimalk_snow$RMSE)] #for rmse
optimalk_snow$kvalue[which.max(optimalk_snow$rsquared)] #for rsquared


#plot
setwd("C:/00_Dana/Uni/3. Mastersemester (Erasmus)/Ecosystem Modelling/Major_Project/Database")

ggplot(data=optimalk_snow, aes(x=kvalue, y=RMSE))+
  geom_line()+
  theme_bw()+
  ggtitle(label="RMSE tuned for k")+
  theme(text=element_text(size=8))+
  xlab(label="Temperature [°C]")
ggsave(filename="RMSE_k_m.jpg", width = 20, height=12, units = "cm")
