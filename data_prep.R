# Script that should be run after load_data.R to ensure that all data has the
# right units. Note: run only once to avoid updating the same data entries
# multiple times!

library(tidyverse)
#tidy up environment
#write the three years into one dataframe for 30mins
Norunda_all_30<-rbind(Norunda1997, Norunda1999, Norunda2001)
rm(Norunda1997, Norunda1999, Norunda2001) #remove
Hyytiala_all_30<-rbind(Hyytiala1999, Hyytiala2000, Hyytiala2001)
rm(Hyytiala1999, Hyytiala2000, Hyytiala2001)#remove

#write the three years into one dataframe for daily data
Norunda_all_day<-rbind(Norunda1997day, Norunda1999day, Norunda2001day)
rm(Norunda1997day, Norunda1999day, Norunda2001day) #remove
Hyytiala_all_day<-rbind(Hyytiala1999day, Hyytiala2000day, Hyytiala2001day)
rm(Hyytiala1999day, Hyytiala2000day, Hyytiala2001day) #remove

#select certain dataframes in environment
dfs<-mget(ls(pattern = "30"), .GlobalEnv)
# convert ET from mmol/m2/s1 to mm/30 min. for 30 min. data 
for(i in 1:length(dfs)){ dfs[[i]]$Evapotr<-(dfs[[i]]$Evapotr* 10^-3 * 18.02)/1000*30*60} #convert
list2env(dfs ,.GlobalEnv) #unlist again in environment
rm(dfs) #remove temp list

# convert ET from mmol/m2/s1 to mm/30 min. for 30 min. data
H_1999_30$Evapotr <- (H_1999_30$Evapotr * 10^-3 * 18.02)/1000*30*60
H_2000_30$Evapotr <- (H_2000_30$Evapotr * 10^-3 * 18.02)/1000*30*60
H_2001_30$Evapotr <- (H_2001_30$Evapotr * 10^-3 * 18.02)/1000*30*60

N_1997_30$Evapotr <- (N_1997_30$Evapotr * 10^-3 * 18.02)/1000*30*60
N_1999_30$Evapotr <- (N_1999_30$Evapotr * 10^-3 * 18.02)/1000*30*60
N_2001_30$Evapotr <- (N_2001_30$Evapotr * 10^-3 * 18.02)/1000*30*60

