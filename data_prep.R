# Script that should be run after load_data.R to ensure that all data has the
# right units. Note: run only once to avoid updating the same data entries
# multiple times!

library(tidyverse)
#tidy up environment
#write the three years into one dataframe for 30mins
Norunda_all_30<-rbind(Norunda1997, Norunda1999, Norunda2001) #for Norunda
rm(Norunda1997, Norunda1999, Norunda2001) #remove
Hyytiala_all_30<-rbind(Hyytiala1999, Hyytiala2000, Hyytiala2001)#for Hyytiala
rm(Hyytiala1999, Hyytiala2000, Hyytiala2001)#remove

#write the three years into one dataframe for daily data
Norunda_all_day<-rbind(Norunda1997day, Norunda1999day, Norunda2001day) #for Norunda
rm(Norunda1997day, Norunda1999day, Norunda2001day) #remove
#the dates seems to be wrong for Hyytiala -> correct it
Hyytiala1999day$Year<-1999 #change from 2000 to 1999
Hyytiala2001day$Year<-2001 #change from 2000 to 2001
Hyytiala_all_day<-rbind(Hyytiala1999day, Hyytiala2000day, Hyytiala2001day) #for Hyytiala
rm(Hyytiala1999day, Hyytiala2000day, Hyytiala2001day) #remove
#create proper date column for Hyytiala
Hyytiala_all_day$date<-paste(Hyytiala_all_day$Year, Hyytiala_all_day$Month, Hyytiala_all_day$Day) #combine columns for date
Hyytiala_all_day$date<-strptime(Hyytiala_all_day$date, format="%Y %m %d") #convert date to proper form
#create proper date column for Norunds
Norunda_all_day$date<-paste(Norunda_all_day$Year, Norunda_all_day$Month, Norunda_all_day$Day) #combine columns for date
Norunda_all_day$date<-strptime(Norunda_all_day$date, format="%Y %m %d") #convert date to proper form
Norunda_all_30$date<-paste(Norunda_all_30$Year, Norunda_all_30$Month, Norunda_all_30$Day) #combine columns for date
Norunda_all_30$date<-strptime(Norunda_all_30$date, format="%Y %m %d") #convert date to proper form

#select certain dataframes in environment
dfs<-mget(ls(pattern = "30"), .GlobalEnv)
# convert ET from mmol/m2/s1 to mm/30 min. for 30 min. data 
for(i in 1:length(dfs)){ dfs[[i]]$Evapotr<-(dfs[[i]]$Evapotr* 10^-3 * 18.02)/1000*30*60} #convert
list2env(dfs ,.GlobalEnv) #unlist again in environment
rm(dfs) #remove temp list

#sum the half hourly SWC20 data
daily_SWC<-Norunda_all_30 %>%
  group_by(date) %>%
  summarize(mean = mean(SWC20))
Norunda_all_day$SWC20<-daily_SWC

