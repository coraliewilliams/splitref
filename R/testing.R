# Testing script
# Random select and split reference list


##################### 
#### For testing ------------------------------------------
#####################

#**
### IMPORTANT: Before running tests make sure you have the functions loaded and 
#**



# Set path where you
funcDir <- "C:/Users/z5394590/OneDrive - UNSW/Documents/R/simstudies/splitRef/"
source(file.path(funcDir, "functions_splitref_oct2022.R"))



# Set root directory and paths where to save files
datdir <- rprojroot::find_rstudio_root_file("splitRef")

# Obtain path of csv file exported from Rayyan with full reference list (make sure to include name)
filePath <- file.path(datdir, "articles.csv")


#### getpilotref function 
getpilotref(filePath, n=15, fileName="pilot_test")


#### splitref function 
# Test 1: simple equal split
splitref(filePath, n=2, prop=0.5, seed=123, fileName="split_test1")

# Test 2: split into three unequal splits
splitref(filePath, n=3, prop=c(0.2,0.3,0.5), seed=123, fileName="split_test2")


