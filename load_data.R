#short way load data 

#by using the encoding UTF8 you can also include the swedish ä --> even though R does not particulary like it
#you can save a file with UTF8 encoding and also open a file by: file --> reopen with encoding
library(readxl)
 path="C:/00_Dana/Uni/3. Mastersemester (Erasmus)/Ecosystem Modelling/Major_Project/Database"
#path="C:/Users/margo/Documents/Universiteit/Lund University/Ecosystem modelling (NGEN02)/Project/R"
setwd(path) #set wd to path 
#read in all csv data 
files_list=list.files(pattern ="\\.csv", recursive = T) #first get all files ending in csv in the working directory and all subfolders
#function edit names of files list
TidyNames<-function(files){
  files<-sub(".*/", "", files) #remove the first part with the file path
  files<-sub(pattern = "älä", "ala", files) ##sub the special characters
  files<-tools::file_path_sans_ext(files) #remove file extension
}

#loop through all the files in the folders and assign name 
for (i in 1:length(files_list)) assign(TidyNames(files_list[i]), #assign tidy names
         read.csv(files_list[i], header = T, na.strings = c("-9999", "NA", "NAN"), #read csv file with header
                  stringsAsFactors = F))
#read in all xlsx file
files_list=list.files(pattern ="\\.xlsx", recursive = T)
#loop through all the files in the folders and assign name 
for (i in 1:length(files_list)) assign(TidyNames(files_list[i]), #assign tidy name
         read_xlsx(path=files_list[i], trim_ws = T)) #read files

