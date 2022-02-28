#tune model for optimal rmse
kvalues<-seq(0.1, 2, by=0.1)
optimalk<-data.frame("kvalue"=kvalues, "RMSE"=NA, "rsquared"=NA)

#tune model based on rmse and r2
for(i in kvalues){
  #model once with every kvalue
  swc_temp<-calc_swc(P=Hyytiala_all_day$Prec, ET=Hyytiala_all_day$Evapotr,
           max_swc=max_swc_H*h, k=i)
  swc_temp$obs<-Hyytiala_all_day$SWC20 #add observations to sim data
  #calculate RMSE for model
  optimalk$RMSE[optimalk$kvalue==i]<-  sqrt(mean((swc_temp$obs - swc_temp$sum/h)^2))
  #calculate linear fit
  optimalk$rsquared[optimalk$kvalue==i]<- summary(lm(swc_temp$sum/h~swc_temp$obs))[["adj.r.squared"]]
}

#get optimal k value
optimalk$kvalue[which.min(optimalk$RMSE)] #for rmse
optimalk$kvalue[which.max(optimalk$rsquared)] #for rsquared
