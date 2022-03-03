#sensitivity analysis
#simple:
swc$precipitation<-Hyytiala_all_day$Prec #add precipitation to output
swc$ET<-Hyytiala_all_day$Evapotr #add ET to output

#calculate simple stats
cols=c(2:8, 12, 13) #cols to use
sens<-data.frame("variable"=colnames(swc)[cols], "cor"=NA, "coef"= NA, "index" = NA)

for(i in cols){
  #calculate sample correlation coefficient
  sens$cor<-round(cor(swc$change, swc[,i]), 2) #round to two digits
  #sensitivity coefficient
  plot((swc$change-mean(swc$change, na.rm=T))/(swc[,i]-mean(swc[,i], na.rm=T)), type="l")
  #sensitivity index
  (max(swc$change[max(swc[,i]])-min(swc$change[max(swc[,i]]))/max(swc$change[max(swc[,i]])
}

#plot inputs against outputs
#soil water
ggplot(data=swc, aes(x=sum, y=change))+
  geom_smooth( method="lm", fill="blue", color="black")+
  geom_jitter(color="darkgrey", alpha=0.3)+
  xlab(label="Soil water [mm]")+
  ylab(label="Soil water change [mm]")+
  theme_bw()

#precipitation
ggplot(data=swc, aes(x=precipitation, y=change))+
  geom_smooth( method="lm", fill="blue", color="black")+
  geom_jitter(color="darkgrey", alpha=0.3)+
  xlab(label="Precipitation [mm]")+
  ylab(label="Soil water change [mm]")+
  theme_bw()

#rain
ggplot(data=swc, aes(x=rain, y=change))+
  geom_smooth( method="lm", fill="blue", color="black")+
  geom_jitter(color="darkgrey", alpha=0.3)+
  xlab(label="Rain [mm]")+
  ylab(label="Soil water change [mm]")+
  theme_bw()

#Evapotranspiration
ggplot(data=swc, aes(x=ET, y=change))+
  geom_smooth( method="lm", fill="blue", color="black")+
  geom_jitter(color="darkgrey", alpha=0.3)+
  xlab(label="Evapotranspiration")+
  ylab(label="Soil water change [mm]")+
  theme_bw()

#Snow melt
ggplot(data=swc, aes(x=snowmelt, y=change))+
  geom_smooth( method="lm", fill="blue", color="black")+
  geom_jitter(color="darkgrey", alpha=0.3)+
  xlab(label="Snow melt")+
  ylab(label="Soil water change [mm]")+
  theme_bw()

#Groundwater discharge
ggplot(data=swc, aes(x=Q, y=sum))+
  geom_smooth( method="lm", fill="blue", color="black")+
  geom_jitter(color="darkgrey", alpha=0.3)+
  xlab(label="Groundwater discharge")+
  ylab(label="Soil water [mm]")+
  theme_bw()

#Groundwater discharge
ggplot(data=swc, aes(x=Q, y=change))+
  geom_smooth( method="lm", fill="blue", color="black")+
  geom_jitter(color="darkgrey", alpha=0.3)+
  xlab(label="Groundwater discharge")+
  ylab(label="Soil water change [mm]")+
  theme_bw()

#srunoff
ggplot(data=swc, aes(x=runoff, y=change))+
  geom_smooth( method="lm", fill="blue", color="black")+
  geom_jitter(color="darkgrey", alpha=0.3)+
  xlab(label="runoff")+
  ylab(label="Soil water change [mm]")+
  theme_bw()
