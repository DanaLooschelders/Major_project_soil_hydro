library(tidyverse)
library(reshape2)
#climate sensitivty

####Temperature####
#vary temperature in between -5 and + 5 degrees and check how output changes
temp_var<-seq(-5, 5, by=1)
temp_output<-data.frame(matrix(nrow = length(Hyytiala_all_day$Year), 
                                  ncol = length(temp_var))) #create empty dataframe
colnames(temp_output)<-as.character(temp_var) #set colnames
#dataframe for input temp
temp_input<-data.frame(matrix(nrow = length(Hyytiala_all_day$Year), 
                               ncol = length(temp_var))) #create empty dataframe
colnames(temp_input)<-as.character(temp_var) #set colnames

for(i in 1:length(temp_var)){
  #call function
  temp_swc<-calc_swc_M(Precip=Hyytiala_all_day$Prec, ET=Hyytiala_all_day$Evapotr,
             max_swc=max_swc_H*1000, k=0.005, min_swc=min_swc_H*1000, T_u=4, 
             T_lf=2, T_lm=-4, k_m=6, 
             T=Hyytiala_all_day$AirT+i, #change airtemp with scaling factor
             init_snowsize=0,
             ET_change=0.7)
  temp_output[,i]<-temp_swc$sum #write in output
  temp_input[,i]<-Hyytiala_all_day$AirT+i
  rm(temp_swc) #remove temp output
}
temp_output$date<-Hyytiala_all_day$date #add date column
temp_output_long<-melt(temp_output, id.vars=c("date")) #get data in long format
temp_input_long<-melt(temp_input)
temp_whole<-data.frame("input"=temp_input_long$value, 
                       "output"=temp_output_long$value,
                       "scenario"=temp_output_long$variable)
#plot
ggplot(data=temp_output_long, aes(y=value, x=date, col=as.factor(variable)))+
  geom_line()+
  theme_bw()

ggplot(data=temp_output_long, aes(y=value, x=as.factor(variable)))+
  geom_boxplot()+
  xlab(label="Change in Temperature [°C]")+
  ylab(label="Soil water Content [mm]")+
  theme_bw()

#calculate coefficient of linear regression for every pair
coefs_temp<-data.frame("scenario"=temp_var, "coef"=NA, "corr"=NA)
for(i in 1:length(temp_var)){
  temp<-lm(temp_input[,i]~temp_output[,i]) #calculate lm
  coefs_temp$coef[i]<-round(temp$coefficients[2],2)#extract coef
  coefs_temp$corr[i]<-round(cor(temp_input[,i],temp_output[,i]),2) #calculate corr
}
coefs_temp$coef_label<-paste("lm coef:", coefs_temp$coef) #create annotation for coef
coefs_temp$corr_label<-paste("cor:", coefs_temp$corr) #create annotation for corr
label_temp_scenario<-paste("Temp. change:", coefs_temp$scenario, "°C")#create label for facet
names(label_temp_scenario)<-as.character(coefs_temp$scenario)
#plot regression for every scenario
ggplot(data=temp_whole, aes(x=input, y=output))+
  geom_jitter()+
  stat_smooth(method="lm")+
  geom_text(data = coefs_temp, aes(x = -15,  y = 160, label = coef_label), size=3) +
  geom_text(data = coefs_temp, aes(x = -15,  y = 190, label = corr_label), size=3) +
  xlab(label="Temperature [°C]")+
  ggtitle(label="Change in soil water content with changing temperature")+
  ylab(label="Soil water Content [mm]")+
  theme(strip.text.x = element_text(size = 5))+
  facet_wrap(~scenario, 
             labeller=labeller(scenario=label_temp_scenario))+
  theme_bw()
ggsave(filename="Temperature_sensitivity.jpg", width = 20, height=12, units = "cm")

####Precipitation####
#vary precipitation in between -50% and + 50% 
prec_var<-round(seq(-0.7, 0.7, by=0.1), 1)
prec_output<-data.frame(matrix(nrow = length(Hyytiala_all_day$Year), 
                               ncol = length(prec_var))) #create empty dataframe
colnames(prec_output)<-as.character(prec_var) #set colnames

#create input dataframe
prec_input<-data.frame(matrix(nrow = length(Hyytiala_all_day$Year), 
                               ncol = length(prec_var))) #create empty dataframe
colnames(prec_input)<-as.character(prec_var) #set colnames

for(i in 1:length(prec_var)){
  #call function
  prec_swc<-calc_swc_M(Precip=Hyytiala_all_day$Prec*i, #change prec with scaling factor
                       ET=Hyytiala_all_day$Evapotr,
                       max_swc=max_swc_H*1000, k=0.005, min_swc=min_swc_H*1000, T_u=4, 
                       T_lf=2, T_lm=-4, k_m=6, 
                       T=Hyytiala_all_day$AirT, 
                       init_snowsize=0,
                       ET_change=0.7)
  prec_output[,i]<-prec_swc$sum #write in output
  prec_input[,i]<-Hyytiala_all_day$Prec*i
  rm(prec_swc) #remove temp output
}
prec_output$date<-Hyytiala_all_day$date #add date column
prec_output_long<-melt(prec_output, id.vars=c("date")) #get data in long format
prec_input_long<-melt(prec_input)
prec_whole<-data.frame("input"=prec_input_long$value, 
                       "output"=prec_output_long$value,
                       "scenario"=prec_output_long$variable)

#plot
ggplot(data=prec_output_long, aes(y=value, x=date, col=as.factor(variable)))+
  geom_line()+
  theme_bw()

ggplot(data=prec_output_long, aes(y=value, x=as.factor(variable)))+
  geom_boxplot()+
  xlab(label="Change in Precipitation [%]")+
  ylab(label="Soil water Content [mm]")+
  scale_x_discrete(limits=as.factor(unique(prec_output_long$variable)), 
                   labels=c(as.factor(seq(-70, 70, 10))))+
  theme_bw()

#calculate coefficient of linear regression for every pair
coefs_prec<-data.frame("scenario"=prec_var, "coef"=NA, "corr"=NA)
for(i in 1:length(prec_var)){
  temp<-lm(prec_input[,i]~prec_output[,i]) #calculate lm
  coefs_prec$coef[i]<-round(temp$coefficients[2], 2) #extract coef
  coefs_prec$corr[i]<-round(cor(prec_input[,i],prec_output[,i]),2) #calculate corr
}
coefs_prec$coef_label<-paste("lm coef:", coefs_prec$coef)
coefs_prec$corr_label<-paste("cor:", coefs_prec$corr)
label_prec_scenario<-paste("Prec. change:", coefs_prec$scenario*100, "%")#create label for facet
names(label_prec_scenario)<-as.character(coefs_prec$scenario)
#plot regression for every scenario
ggplot(data=prec_whole, aes(x=input, y=output))+
  geom_jitter()+
  stat_smooth(method="lm")+
  geom_text(data = coefs_prec, aes(x = 550,  y = 200, label = coef_label), size=3) +
  geom_text(data = coefs_prec, aes(x = 550,  y = 250, label = corr_label), size=3) +
  xlab(label="Precipitation [mm]")+
  ylab(label="Soil water Content [mm]")+
  theme(strip.text.x = element_text(size = 5))+
  facet_wrap(~scenario, 
             labeller=labeller(scenario=label_prec_scenario))+
  theme_bw()
ggsave(filename="Precipitation_sensitivity.jpg", width = 20, height=12, units = "cm")
