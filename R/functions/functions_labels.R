# author: Coralie Williams 
# last updated: 18/01/2023

# Load libraries
library(readr)
library(dplyr)
library(purrr)
library(splitstackshape)


##################### 
#### Description ----------------------------------------------------------------------------------
#####################


# Develop function to retrieve exclusion reason label from exported Rayyan table. Additional functionality is to add a label
# describing which level in the PICO/PECO framework the exclusion refers to.



##################### 
#### Testing ------------------------------------------------------------------------------------
#####################

# load test data
dat_lab <- read_csv("data/articles_withlabelsdecisions.csv")
dat_ocp <- read_csv("data/articles_ocp_kyle.csv")


####### TEST 1

# get exclusion reason
dat_ocp$exclusion_reason <- NA
dat_ocp$exclusion_reason[grep("full", dat_ocp$notes)] <- "full_text_not_available"
dat_ocp$exclusion_reason[grep("ocp|organochlor|pesticide", dat_ocp$notes)] <- "wrong_exposure_type"
dat_ocp$exclusion_reason[grep("meta", dat_ocp$notes)] <- "wrong_study_type"
dat_ocp$exclusion_reason[grep("study|model", dat_ocp$notes)] <- "not_english_language"



###### TEST 2

# split label vector 
notes <- strsplit(dat_k$notes, split="|", fixed=TRUE)

# Extracting Second Sub Element from List Using sapply Function
# Only works if second sub element is not NA
sapply(notes, "[[", 2)



####### TEST 3 (long)

# split label vector 
notes <- strsplit(dat_lab$notes, split="| RAYYAN-LABELS: ", fixed=TRUE)

# get length of data frame
res <- NA
result <- NA
n <- length(notes)

# make a loop through each list
for (i in 1:n) {
  res[i] <- ifelse(length(notes[[i]])==1, NA,notes[[i]][[2]])
}

# clean up
strsplit(res, split=" |", fixed=TRUE)

# store results as a new variable
dat <- as.data.frame(cbind(dat_lab, res))





##################### 
#### Functions ------------------------------------------------------------------------------------
#####################


# -----------------------------------
# getlabel function 
# -----------------------------------
## Description: 
#     Randomly subset a pilot set of references
#
# Arguments
# - x: data frame with reference list from Rayyan
# - n: number of papers needed for pilot set
# - fileName: name of file with list of articles for pilot study
# - write: logical argument whether to save the pilot list as csv in current working directory

getlabel <- function(x, n=10, write=FALSE, fileName="pilot"){
  
  
}




