library(ggplot2)

####soil water change####
#plot
ggplot(data=swc_Hyytiala_validation)+
  geom_line(aes(x=date, y=change), color="darkblue")+
  xlab(label="Date")+
  ylab(label="Change in soil water content [mm]")+
  theme(text=element_text(size=10))+
  ggtitle(label = "Soil water change in Hyytiala", subtitle = "1999 to 2001")+
  theme_bw()
ggsave(filename="Hyytiala_sw_change.jpg", width = 20, height=12, units = "cm")

#####soil water####
ggplot(data=swc_Hyytiala_validation)+
  geom_line(aes(x=date, y=sum), color="darkblue")+
  geom_hline(aes(yintercept=335, col="max water content"))+
  labs(color="")+
  xlab(label="Date")+
  ylab(label="Soil water content [mm]")+
  ggtitle(label="Soil water content in Hyytiala", subtitle="1999 to 2001")+
  theme_bw()+
  theme(text=element_text(size=10), legend.position = "bottom")
ggsave(filename="Hyytiala_sw_content.jpg", width = 20, height=12, units = "cm")

####observed vs simulated swc####
plot(swc$sum/h, Hyytiala_validation$SWC20, xlim=c(0,0.55), ylim=c(0,0.55))

#HYYTIALÖA plot  modelled and observed fluxes as time series
ggplot(data=swc_Hyytiala_validation)+
  geom_line(aes(x=date, y=sum/200, color="Simulatated"), alpha=0.8)+
  geom_line(aes(x=date, y=obs, color="Observed"))+
  labs(color="")+
  scale_color_manual(values = c("black","#1b9e77"))+
  xlab(label="Date")+
  ylab(bquote('Soil water content ['*m^3*'/' *m^3*']'))+
  ggtitle(label="Soil water content in Hyytiala in 2001")+
  theme_bw()+
  theme(text=element_text(size=10), legend.position = "bottom")
ggsave(filename="Hyytiala_validation_sw_obs_sim.jpg",  width = 10, height=10, units = "cm")

#HYYTIALÖA plot with more Prec modelled and observed fluxes as time series
ggplot(data=swc_Hyytiala_validation_interc)+
  geom_line(aes(x=date, y=sum/200, color="Simulatated"), alpha=0.8)+
  geom_line(aes(x=date, y=obs, color="Observed"))+
  labs(color="")+
  scale_color_manual(values = c("black","#1b9e77"))+
  xlab(label="Date")+
  ylab(bquote('Soil water content ['*m^3*'/' *m^3*']'))+
  ggtitle(label="Soil water content in Hyytiala in 2001",
          subtitle = "Precipitation increased by 0.5 mm/d")+
  theme_bw()+
  theme(text=element_text(size=10), legend.position = "bottom")
ggsave(filename="Hyytiala_validation_interception_sw_obs_sim.jpg",  width = 10, height=10, units = "cm")

#NORUNDA plot  modelled and observed fluxes as time series
ggplot(data=swc_Norunda_validation)+
  geom_line(aes(x=date, y=sum/200, color="Simulatated"), alpha=0.8)+
  geom_line(aes(x=date, y=obs, color="Observed"))+
  labs(color="")+
  scale_color_manual(values = c("black","#1b9e77"))+
  xlab(label="Date")+
  ylab(bquote('Soil water content ['*m^3*'/' *m^3*']'))+
  ggtitle(label="Soil water content in Norunda in 1997")+
  theme_bw()+
  theme(text=element_text(size=10), legend.position = "bottom")
ggsave(filename="Norunda_validation_sw_obs_sim_.jpg",  width = 10, height=10, units = "cm")


#plot linear regression
ggplot(data=swc_Hyytiala_validation,aes(x=sum/h, y=obs))+
  geom_point()+
  geom_smooth(method="lm")+
  theme_bw()

snowpack$date<-Hyytiala_validation$date
ggplot(data=snowpack, aes(x=date, y=size))+
  geom_line()+
  theme_bw()

range(swc$runoff)
plot(swc$runoff, type="l")

soilwaterchange_obs<-diff(Hyytiala_validation$SWC20)
swc_Hyytiala_validation$obs_change<-NA
swc_Hyytiala_validation$obs_change[2:length(swc_Hyytiala_validation$obs_change)]<-diff(Hyytiala_validation$SWC20)
plot(soilwaterchange_obs, type="l")
mean(abs(soilwaterchange_obs))

max(swc$change/200)
#plot modelled and observed changes together with rain
ggplot()+
  geom_line(data=swc_Hyytiala_validation, aes(x=date, y=change/200, col="modelled"),show.legend = T)+
  geom_line(data=swc_Hyytiala_validation, aes(x=date, y=obs_change, col="observed"))+
  #geom_line(data=swc_Hyytiala_validation, aes(x=date, y=rain/100, col="rain"), alpha=0.4)+
  theme_bw()
#plot modelled and obs soil water together with rain
ggplot(data=swc_Hyytiala_validation)+
  geom_line(aes(x=date, y=sum/200, col="modelled"))+
  geom_line(aes(x=date, y=obs, col="obs"))+
  geom_line(aes(x=date, y=rain/100, col="rain"))+
  geom_hline(yintercept = max_swc_H)+
  theme_bw()

#calculate ratio max soil water and max soil water change
max(diff(Hyytiala_all_30$SWC20))/max(Hyytiala_validation$SWC20)*100
max(swc$change)/max(swc$sum)*100