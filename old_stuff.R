# Script that loads all data used in the other scripts for the project.
# Note that the Hyytiala dataset is loaded as an Excel file whereas Norunda data
# is in csv file format. This implies that the first row for the Norunda data
# contains the headers for the columns.
# Also note that the name for Hyytiala file path has been changed such that it
# does not contain the Swedish ?.

library(tidyverse)
library(readxl)


# set working directory
setwd("C:/Users/margo/Documents/Universiteit/Lund University/Ecosystem modelling (NGEN02)/Project/R")

# import .xlsx files
H_1999_day <- read_xlsx("Hyytiala/Hyytiala.xlsx", sheet="1999day",cell_cols(1:21))
H_2000_day <- read_xlsx("Hyytiala/Hyytiala.xlsx", sheet="2000day",cell_cols(1:21))
H_2001_day <- read_xlsx("Hyytiala/Hyytiala.xlsx", sheet="2001day",cell_cols(1:21))
H_1999_30 <- read_xlsx("Hyytiala/Hyytiala.xlsx", sheet="1999",cell_cols(1:40))
H_2000_30 <- read_xlsx("Hyytiala/Hyytiala.xlsx", sheet="2000",cell_cols(1:40))
H_2001_30 <- read_xlsx("Hyytiala/Hyytiala.xlsx", sheet="2001",cell_cols(1:40))

# import .csv files
N_1997_day <- read_csv("Norunda/Daily_data/Norunda1997day.csv", cell_cols(1:15), skip=1)
N_1999_day <- read_csv("Norunda/Daily_data/Norunda1999day.csv", cell_cols(1:15), skip=1)
N_2001_day <- read_csv("Norunda/Daily_data/Norunda2001day.csv", cell_cols(1:15), skip=1)
N_1997_30 <- read_csv("Norunda/30min_data/Norunda1997.csv", cell_cols(1:30), skip=1)
N_1999_30 <- read_csv("Norunda/30min_data/Norunda1999.csv", cell_cols(1:30), skip=1)
N_2001_30 <- read_csv("Norunda/30min_data/Norunda2001.csv", cell_cols(1:30), skip=1)

# import headers for csv files
N_headers_day <- read_csv("Norunda/Daily_data/Norunda1997day.csv", cell_cols(1:15), n_max=1) 
N_headers_30 <- read_csv("Norunda/30min_data/Norunda1997.csv", cell_cols(1:30), n_max=1) 

# assign headers to Norunda data
names(N_1997_30) <- N_headers_30[1, ]
names(N_1999_30) <- N_headers_30[1, ]
names(N_2001_30) <- N_headers_30[1, ]
names(N_1997_day) <- N_headers_day[1, ]
names(N_1999_day) <- N_headers_day[1, ]
names(N_2001_day) <- N_headers_day[1, ]

library(tidyverse)
library(readxl)


# set constant parameters
max_swc_H <- 90 # water holding capacity for Hyytiala
max_swc_N <- 90  # water holding capacity for Norunda
k <- 0.7

# function to check for negative or more than max soil water capacity
check_swc <- function(check_swc, max_swc) {
  if (check_swc < 0) {
    check_swc <- 0
  } else if (check_swc > max_swc) {
    check_swc <- max_swc
  }
  return(check_swc)
}

# basic model for calculating soil water content
calc_swc <- function(P, ET, max_swc, k) {
  swc <- P[1] - ET[1]
  swc <- check_swc(swc, max_swc)
  Q <- 0
  for (i in 2:length(P)) {
    Q[i] <- swc[i-1]*k*(swc[i-1]/max_swc)^2
    if (max_swc < P[i] - Q[i] - ET[i]) {
      swc[i] <- max_swc - swc[i-1]
      swc[i] <- check_swc(swc[i], max_swc)
    } else {
      swc[i] <- swc[i-1] + P[i] - ET[i] - Q[i]
      swc[i] <- check_swc(swc[i], max_swc)
    }
  }
  return(swc)
}

swc_H_1999day <- calc_swc(H_1999_day$Prec, H_1999_day$Evapotr, max_swc_H, k)
swc_N_1997day <- calc_swc(N_1997_day$Prec, N_1997_day$Evapotr, max_swc_N, k)
swc_N_1999day <- calc_swc(N_1999_day$Prec, N_1999_day$Evapotr, max_swc_N, k)

#test model
swc<-calc_swc_M(Precip=Hyytiala_all_day$Prec, ET=Hyytiala_all_day$Evapotr,
                max_swc=max_swc_H*1000, k_winter=0.004, k_summer=0.05, 
                min_swc=min_swc_H*1000, T_u=4, 
                T_lf=-7, T_lm=-5, k_m=0, T=Hyytiala_all_day$AirT, init_snowsize=0,
                ET_summer=0, ET_winter=-1, month=Hyytiala_all_day$Month)
#add column with date
swc$date<-Hyytiala_all_day$date
swc$date<-as.POSIXct(swc$date)
swc$obs<-Hyytiala_all_day$SWC20 #add observations to sim data

#plot  modelled and observed fluxes as time series
ggplot(data=swc)+
  geom_line(aes(x=date, y=sum, color="Simulated"), alpha=0.8)+
  geom_line(aes(x=date, y=obs*1000, color="Observed"))+
  labs(color="")+
  scale_color_manual(values = c("black","darkblue"))+
  xlab(label="Date")+
  ylab(label="Soil water content [mm]")+
  ggtitle(label="Soil water content in Hyytiala", subtitle="1999 to 2001")+
  theme_bw()+
  theme(text=element_text(size=10), legend.position = "bottom")
ggsave(filename="Hyytiala_old_sw_obs_sim.jpg",  width = 20, height=12, units = "cm")

#Hyytiala - calibration
swc_Hyytiala_calib<-calc_swc_M(Precip=Hyytiala_calibration$Prec, ET=Hyytiala_calibration$Evapotr,
                               max_swc=max_swc_H*1000, k_winter=0.002, k_summer=0.05, min_swc=min_swc_H*1000, T_u=4, 
                               T_lf=-7, T_lm=-5, k_m=3, T=Hyytiala_calibration$AirT, init_snowsize=0,
                               ET_change=0, month=Hyytiala_calibration$Month)
#Hyytiala validation
swc_Hyytiala_valid<-calc_swc_M(Precip=Hyytiala_validation$Prec, ET=Hyytiala_validation$Evapotr,
                               max_swc=max_swc_H*1000, k_winter=0.002, k_summer=0.05, min_swc=min_swc_H*1000, T_u=4, 
                               T_lf=-7, T_lm=-5, k_m=3, T=Hyytiala_validation$AirT, init_snowsize=0,
                               ET_change=0, month=Hyytiala_validation$Month)
#Norunda validation
swc_Norunds_valid<-calc_swc_M(Precip=Norunda_validation$Prec, ET=Norunda_validation$Evapotr,
                              max_swc=max_swc_H*1000, k_winter=0.002, k_summer=0.05, min_swc=min_swc_H*1000, T_u=4, 
                              T_lf=-7, T_lm=-5, k_m=3, T=Norunda_validation$AirT, init_snowsize=0,
                              ET_change=0, month=Norunda_validation$Month)
