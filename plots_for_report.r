#Plot air Temperature with thresholds

ggplot(data=Hyytiala_all_day, aes(x=date, y=AirT))+
  geom_line()+
  geom_abline(aes(h=))
  xlab(label="Date")+
  ylab(label="Air Temperature [°C]")+
  theme_bw()

  T_u=4, 
  T_lf=-6, T_lm=i, k_m=i