# Script that should be run after load_data.R to ensure that all data has the
# right units. Note: run only once to avoid updating the same data entries
# multiple times!

library(tidyverse)


# convert ET from mmol/m2/s1 to mm/30 min. for 30 min. data
H_1999_30$Evapotr <- (H_1999_30$Evapotr * 10^-3 * 18.02)/1000*30*60
H_2000_30$Evapotr <- (H_2000_30$Evapotr * 10^-3 * 18.02)/1000*30*60
H_2001_30$Evapotr <- (H_2001_30$Evapotr * 10^-3 * 18.02)/1000*30*60

N_1997_30$Evapotr <- (N_1997_30$Evapotr * 10^-3 * 18.02)/1000*30*60
N_1999_30$Evapotr <- (N_1999_30$Evapotr * 10^-3 * 18.02)/1000*30*60
N_2001_30$Evapotr <- (N_2001_30$Evapotr * 10^-3 * 18.02)/1000*30*60
