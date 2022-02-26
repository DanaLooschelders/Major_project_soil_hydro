library(ggplot2)

#add column with date
swc$date<-Hyytiala_all_day$date
swc$date<-as.POSIXct(swc$date)

####soil water change####
#plot
ggplot(data=swc)+
  geom_line(aes(x=date, y=change))+
  xlab(label="Date")+
  ylab(label="Change in soil water content [mm]")+
  theme(text=element_text(size=10))+
  theme_bw()

#####soil water####
ggplot(data=swc)+
  geom_line(aes(x=date, y=sum))+
  geom_hline(aes(yintercept=33.5, col="max water content"))+
  labs(color="")+
  xlab(label="Date")+
  ylab(label="Soil water content [mm]")+
  theme_bw()+
  theme(text=element_text(size=10), legend.position = "bottom")
