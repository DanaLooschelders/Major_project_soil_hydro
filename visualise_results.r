library(ggplot2)

####soil water change####
#plot
ggplot(data=swc)+
  geom_line(aes(x=date, y=change), color="darkblue")+
  xlab(label="Date")+
  ylab(label="Change in soil water content [mm]")+
  theme(text=element_text(size=10))+
  ggtitle(label = "Soil water change in Hyytiala", subtitle = "1999 to 2001")+
  theme_bw()
ggsave(filename="Hyytiala_sw_change.jpg", width = 20, height=12, units = "cm")

#####soil water####
ggplot(data=swc)+
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
plot(swc$sum/h, Hyytiala_all_day$SWC20, xlim=c(0,0.55), ylim=c(0,0.55))

#plot  modelled and observed fluxes as time series
ggplot(data=swc)+
  geom_line(aes(x=date, y=sum/1000, color="Simulatated"), alpha=0.8)+
  geom_line(aes(x=date, y=obs, color="Observed"))+
    labs(color="")+
  scale_color_manual(values = c("black","darkblue"))+
  xlab(label="Date")+
  ylab(label="Soil water content [mm]")+
  ggtitle(label="Soil water content in Hyytiala", subtitle="1999 to 2001")+
  theme_bw()+
  theme(text=element_text(size=10), legend.position = "bottom")
ggsave(filename="Hyytiala_sw_obs_sim.jpg",  width = 20, height=12, units = "cm")

#plot linear regression
ggplot(data=swc,aes(x=sum/h, y=obs))+
  geom_point()+
  geom_smooth(method="lm")+
  theme_bw()

snowpack$date<-Hyytiala_all_day$date
ggplot(data=snowpack, aes(x=date, y=size))+
  geom_line()+
  theme_bw()

range(swc$runoff)
plot(swc$runoff, type="l")
