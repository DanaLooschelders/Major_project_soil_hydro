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
