library(ggplot2)

#add column with date
swc$date<-paste(rep(1999, length(Hyytiala1999day$Year)), Hyytiala1999day$Month, Hyytiala1999day$Day)
swc$date<-strptime(swc$date, format="%Y %m %d")
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
