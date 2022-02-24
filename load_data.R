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

#short way load data 

#by using the encoding UTF8 you can also include the swedish ä --> even though R does not particulary like it
#you can save a file with UTF8 encoding and also open a file by: file --> reopen with encoding
library(readxl)
path="C:/00_Dana/Uni/3. Mastersemester (Erasmus)/Ecosystem Modelling/Major_Project/Database"
setwd(path) #set wd to path 
#read in all csv data 
files_list=list.files(pattern ="\\.csv", recursive = T) #first get all files ending in csv in the working directory and all subfolders
#loop through all the files in the folders and assign name
for (i in 1:length(files_list)) assign(files_list[i], 
         read.csv(files_list[i], header = T, na.strings = c("-9999", "NA", "NAN"), #read csv file with header
                  stringsAsFactors = F))
#read in all xlsx file
files_list=list.files(pattern ="\\.xlsx", recursive = T)
#loop through all the files in the folders and assign name 
for (i in 1:length(files_list)) assign(files_list[i], 
         read_xlsx(path=files_list[i], trim_ws = T)) #read files
