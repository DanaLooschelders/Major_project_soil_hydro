library(ggplot2)

#add column with date
swc$date<-Hyytiala_all_day$date
swc$date<-as.POSIXct(swc$date)

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
  geom_hline(aes(yintercept=33.5, col="max water content"))+
  labs(color="")+
  xlab(label="Date")+
  ylab(label="Soil water content [mm]")+
  ggtitle(label="Soil water content in Hyytiala", subtitle="1999 to 2001")+
  theme_bw()+
  theme(text=element_text(size=10), legend.position = "bottom")
ggsave(filename="Hyytiala_sw_content.jpg", width = 20, height=12, units = "cm")
