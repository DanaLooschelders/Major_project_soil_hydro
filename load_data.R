# Script that loads all data used in the other scripts for the project.
# Note that the Hyytiala dataset is loaded as an Excel file whereas Norunda data
# is in csv file format. This implies that the first row for the Norunda data
# contains the headers for the columns.
# Also note that the name for Hyytiala file path has been changed such that it
# does not contain the Swedish ä.

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
