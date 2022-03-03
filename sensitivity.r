#sensitivity analysis
#simple:
#plot inputs against outputs

#soil water
ggplot(data=swc, aes(x=sum, y=change))+
  geom_smooth( method="lm", fill="blue", color="black")+
  geom_jitter(color="darkgrey", alpha=0.3)+
  xlab(label="Soil water [mm]")+
  ylab(label="Soil water change [mm]")+
  theme_bw()


#precipitation
swc$precipitation<-Hyytiala_all_day$Prec
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
swc$ET<-Hyytiala_all_day$Evapotr
ggplot(data=swc, aes(x=ET, y=change))+
  geom_smooth( method="lm", fill="blue", color="black")+
  geom_jitter(color="darkgrey", alpha=0.3)+
  xlab(label="Evapotranspiration")+
  ylab(label="Soil water change [mm]")+
  theme_bw()

#Snow melt
swc$ET<-Hyytiala_all_day$Evapotr
ggplot(data=swc, aes(x=snowmelt, y=change))+
  geom_smooth( method="lm", fill="blue", color="black")+
  geom_jitter(color="darkgrey", alpha=0.3)+
  xlab(label="Snow melt")+
  ylab(label="Soil water change [mm]")+
  theme_bw()

#Groundwater discharge
ggplot(data=swc, aes(x=Q, y=change))+
  geom_smooth( method="lm", fill="blue", color="black")+
  geom_jitter(color="darkgrey", alpha=0.3)+
  xlab(label="Groundwater discharge")+
  ylab(label="Soil water change [mm]")+
  theme_bw()

#snow melt
ggplot(data=swc, aes(x=runoff, y=change))+
  geom_smooth( method="lm", fill="blue", color="black")+
  geom_jitter(color="darkgrey", alpha=0.3)+
  xlab(label="runoff")+
  ylab(label="Soil water change [mm]")+
  theme_bw()
