# author: Coralie Williams 

# Load libraries
library(readr)
library(dplyr)
library(purrr)
library(splitstackshape)


# Script description ------------------

# Functions to randomly select and split a list of references from an exported Rayyan project for a pilot assessment or
# for a collaborative Rayyan project. 

######
# getpilotref : line 22 to 62
# 
#
#


##################### 
#### Functions ------------------------------------------
#####################


# -----------------------------------
# getpilotref function 
# -----------------------------------
## Description: 
#     Randomly subset a pilot set of references
#
# Arguments
# - x: data frame with reference list from Rayyan
# - n: number of papers needed for pilot set
# - fileName: name of file with list of articles for pilot study
# - write: logical argument whether to save the pilot list as csv in current working directory

getpilotref <- function(x, n=10, fileName="pilot", write=FALSE){
  
  if (length(n) == 1L && is.integer(n) && n>0 && n<=nrow(x)) { 
  
    # get list of indexes for each reference
    x$ids <- 1:nrow(x)
    
    # sample randomly a list n of indexes
    pdat <- x[which(x$ids %in% sample(x$ids, n)),]
    
    # remove ids column generated for split
    pdat <<- pdat[,-which(colnames(pdat)=="ids")]
    
    } else {

    # error message if provided n value is not valid 
    stop("Incompatible value n supplied, please check. n must be a positive integer no higher than the total number of references provided.") 
      
      }
  
  if (write==T){
    
    # save generated pilot list in working directory using the name provided
    write_csv(pilot, paste(fileName, ".csv", sep=""), na="")
    
    # print out
    cat(paste("Pilot random sample set of ", n, " articles is saved as: ", fileName, ".csv", sep=""))
    
    }
}





# -----------------------------------
# splitref_two function 
# -----------------------------------
## Description: 
#     Randomly split reference list for collaborative work between k=2 collaborators
#
## Arguments: 
# - x: data frame with reference list from Rayyan
# - k: number of collaborators/splits needed (default is 2, maximum possible is 5)
# - prop: proportion required of each split, must be an integer between 0 and 1. Default is 0.5 (equal proportions for each split)
# - fileName: provided desired suffix of split files






# -----------------------------------
# splitref_k function 
# -----------------------------------
## Description: 
#     Randomly split reference list for collaborative work with multiple people (k>=2)
#
## Arguments: 
# - x: data frame with reference list from Rayyan
# - n: number of collaborators/splits needed (default is 2, maximum possible is 5)
# - prop: proportion required of each split, must be an integer between 0 and 1. Default is 0.5 (equal proportions for each split)
# - fileName: provided desired suffix of split files






# -----------------------------------
# splitref_kprop function 
# -----------------------------------
## Description: 
#     Randomly split reference list for collaborative work with multiple people (k>=2)
#
## Arguments: 
# - x: data frame with reference list from Rayyan
# - n: number of collaborators/splits needed (default is 2, maximum possible is 5)
# - prop: proportion required of each split, must be an integer between 0 and 1. Default is 0.5 (equal proportions for each split)
# - fileName: provided desired suffix of split files





# -----------------------------------
# splitref function 
# -----------------------------------
## Description: 
#     Randomly split reference list for collaborative work
#
## Arguments: 
# - x: data frame with reference list from Rayyan
# - k: number of collaborators/splits needed (default is 2, maximum possible is 5)
# - prop: proportion required of each split, must be an integer between 0 and 1. Default is 0.5 (equal proportions for each split)
# - fileName: provided desired suffix of split files

splitref <- function(x, k=2, prop=0.5, fileName="split"){
  
  
  # read full list of articles
  x <- read_csv(filePath, show_col_types = FALSE)
  
  # get random list of indexes for each reference
  rids <- sample(1:nrow(x))
  
  # obtain cut off indexes to split reference list based on proportion given
  if (length(prop)==1){
    # get equal split proportions based on number of splits
    p <- (2:k-1)*(1/k)
    split_ids <- floor(p * nrow(x))
  } else {
    # error message if prop values are not valid for number of splits given
    if (!length(prop)==k) { 
      stop("Incompatible proportion values supplied. prop argument must be a vector with size equal to n.") 
    } else {
      # input proportions for split
      split_ids <- floor(prop * nrow(x))
    }
  }
  
  
  if(k==2){
    # for k=2 collaborators
    dat1_indexes <- rids[1:split_ids]
    dat2_indexes <- rids[(split_ids+1):nrow(x)]
    
    # get separate datasets
    dat1 <- x[dat1_indexes,] 
    dat2 <- x[dat2_indexes,]
    
    # save files
    write_csv(dat1, paste(fileName, "_set1", ".csv", sep=""), na="")
    write_csv(dat2, paste(fileName, "_set2",".csv", sep=""), na="")
    
  } else if(k==3){
    # for k=3 collaborators
    dat1_indexes <- rids[1:split_ids[1]]
    dat2_indexes <- rids[(split_ids[1]+1):split_ids[2]]
    dat3_indexes <- rids[(split_ids[2]+1):nrow(x)]
    
    # get separate datasets
    dat1 <- x[dat1_indexes,] 
    dat2 <- x[dat2_indexes,]
    dat3 <- x[dat3_indexes,]
    
    # save files
    write_csv(dat1, paste(fileName, "_set1", ".csv", sep=""), na="")
    write_csv(dat2, paste(fileName, "_set2",".csv", sep=""), na="")
    write_csv(dat3, paste(fileName, "_set3",".csv", sep=""), na="")
    
  } else if (k==4){
    # for k=4 collaborators
    dat1_indexes <- rids[1:split_ids[1]]
    dat2_indexes <- rids[(split_ids[1]+1):split_ids[2]]
    dat3_indexes <- rids[(split_ids[2]+1):split_ids[3]]
    dat4_indexes <- rids[(split_ids[3]+1):nrow(x)]
    
    # get separate datasets
    dat1 <- x[dat1_indexes,] 
    dat2 <- x[dat2_indexes,]
    dat3 <- x[dat3_indexes,]
    dat4 <- x[dat4_indexes,]
    
    # save files
    write_csv(dat1, paste(fileName, "_set1", ".csv", sep=""), na="")
    write_csv(dat2, paste(fileName, "_set2",".csv", sep=""), na="")
    write_csv(dat3, paste(fileName, "_set3",".csv", sep=""), na="")
    write_csv(dat4, paste(fileName, "_set4",".csv", sep=""), na="")
    
  } else if (k==5){
    # for k=5 collaborators
    dat1_indexes <- rids[1:split_ids[1]]
    dat2_indexes <- rids[(split_ids[1]+1):split_ids[2]]
    dat3_indexes <- rids[(split_ids[2]+1):split_ids[3]]
    dat4_indexes <- rids[(split_ids[3]+1):split_ids[4]]
    dat5_indexes <- rids[(split_ids[4]+1):nrow(x)]
    
    # get separate datasets
    dat1 <- x[dat1_indexes,] 
    dat2 <- x[dat2_indexes,]
    dat3 <- x[dat3_indexes,]
    dat4 <- x[dat4_indexes,]
    dat5 <- x[dat5_indexes,]
    
    # save files
    write_csv(dat1, paste(fileName, "_set1", ".csv", sep=""), na="")
    write_csv(dat2, paste(fileName, "_set2",".csv", sep=""), na="")
    write_csv(dat3, paste(fileName, "_set3",".csv", sep=""), na="")
    write_csv(dat4, paste(fileName, "_set4",".csv", sep=""), na="")
    write_csv(dat5, paste(fileName, "_set5",".csv", sep=""), na="")
    
  } else if (!k%in%(2:5)){
    # error message if number of splits provided is not valid 
    stop("Incompatible number of splits supplied. n must be an interger between 2 and 5.\n
         A maximum of 5 splits is possible.")
  }
  
  
  # print out summary of splitting and data files saved
  p <- ifelse(length(prop)==1, "equal", "unequal")
  cat(paste(c("Reference list was randomly split into", k, "based on", p, "proportions", "(", prop, ")")))
  cat(paste(c("\n", k, "files were saved:\n", 
              paste(getwd(),"/",fileName, "_set1", ".csv", sep=""))))
  
}





# -----------------------------------
# dedup_ref function 
# -----------------------------------
## Description: 
#     Deduplicate 
#