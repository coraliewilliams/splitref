# Testing script for functions


##################### 
#### For testing ------------------------------------------
#####################


# Load functions from source file
source("R/functions_splitref.R")



# quick tests -----------------------------------------

# List of references as data frame 
articles <- read_csv("data/articles.csv", show_col_types=F)

# pilot number
n <- -2
n <- "not_an_integer"
n <- 10^10

# test getpilotref function
getpilotref(articles, n=asd)








# \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

#### Load functions -------------
# Set path where the function script is saved
funcDir <- "C:/Users/z5394590/OneDrive - UNSW/Documents/Projects/splitref/R"


#### Get file path ----------------

# Set root directory and paths where to save files
datdir <- rprojroot::find_rstudio_root_file("data")

# Obtain path of csv file exported from Rayyan with full reference list (make sure to include name)
filePath <- file.path(datdir, "articles.csv")

#### getpilotref function test --------------
getpilotref(filePath, n=15, fileName="pilot_test")


#### splitref function test -----------------

# Test 1: simple equal split
splitref(filePath, n=2, prop=0.5, seed=123, fileName="split_test1")

# Test 2: split into three unequal splits
splitref(filePath, n=3, prop=c(0.2,0.3,0.5), seed=123, fileName="split_test2")


