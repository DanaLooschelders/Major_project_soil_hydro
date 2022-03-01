#sensitivity analysis
#with ET
Hyytiala_all_day$date<-as.POSIXct(Hyytiala_all_day$date)

ggplot(data=swc)+
  geom_line(aes(x=date, y=sum/1000, color="Simulatated"), alpha=0.8)+
  geom_line(aes(x=date, y=obs, color="Observed"))+
  geom_line(data=Hyytiala_all_day, aes(x=date, y=Evapotr/10, color="ET"))+
  labs(color="")+
  scale_color_manual(values = c("red", "black", "darkblue"))+
  xlab(label="Date")+
  ylab(label="Soil water content [mm]")+
  ggtitle(label="Soil water content in Hyytiala", subtitle="1999 to 2001")+
  theme_bw()+
  theme(text=element_text(size=10), legend.position = "bottom")

#Q
ggplot(data=swc)+
  geom_line(aes(x=date, y=sum/1000, color="Simulatated"), alpha=0.8)+
  geom_line(aes(x=date, y=obs, color="Observed"))+
  geom_line(aes(x=date, y=Q/10, color="Q"))+
  labs(color="")+
  scale_color_manual(values = c("black", "red", "darkblue"))+
  xlab(label="Date")+
  ylab(label="Soil water content [mm]")+
  ggtitle(label="Soil water content in Hyytiala", subtitle="1999 to 2001")+
  theme_bw()+
  theme(text=element_text(size=10), legend.position = "bottom")
range(swc$Q, na.rm=T)

plot(swc$snowsize, type="l")
plot(Hyytiala_all_day$AirT, type="l")

#
snowpack$date<-Hyytiala_all_day$date
ggplot(data=swc)+
  geom_line(aes(x=date, y=sum/1000, color="Simulatated"), alpha=0.8)+
  geom_line(aes(x=date, y=obs, color="Observed"))+
  geom_line(data=snowpack, aes(x=date, y=actual_melt/10, color="Melt"))+
  labs(color="")+
  scale_color_manual(values = c("black", "red", "darkblue"))+
  xlab(label="Date")+
  ylab(label="Soil water content [mm]")+
  ggtitle(label="Soil water content in Hyytiala", subtitle="1999 to 2001")+
  theme_bw()+
  theme(text=element_text(size=10), legend.position = "bottom")
