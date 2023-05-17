# author: Coralie Williams 
# last updated: 09/02/2022

# Load libraries
library(readr)
library(dplyr)
library(purrr)
library(splitstackshape)


# For ris export to be used in Rayyan need to reorder files


##################### 
#### Description ----------------------------------------------------------------------------------
#####################

# Functions to randomly select and split a list of references from an exported Rayyan project for a pilot assessment or
# for a collaborative Rayyan project. 
# 
# 1. getpilotref function (line 27)
# 2. randomsplit function (line 27)


##################### 
#### Functions ------------------------------------------------------------------------------------
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
#
# Output: 

getpilotref <- function(x, n=10, write=FALSE, fileName="pilot"){
  
  # check if input is valid 
  if (!is.data.frame(x)) stop("x must be a data frame")
  if (n <= 0) stop("n must be a positive number")
  if (n > nrow(x)) stop("n must be a smaller number than the total number of rows in the data frame")
  if (!(is.numeric(n))) stop("n must be a integer (whole number)")
  if (!(n %% 1 == 0)) stop("n must be a integer (whole number)")

  # sample randomly the vector n of row indexes and remove id column in the final dataset
  x$ids <- 1:nrow(x)
  pdat <- x[which(x$ids %in% sample(x$ids, n)),]
  pilotdat <<- pdat[,-which(colnames(pdat)=="ids")]
  
  if (write==T){
    # save generated pilot list in working directory using the name provided
    write_csv(pilotdat, paste(fileName, ".csv", sep=""), na="", row.names=F)
    # print out summary of saved file name
    cat(paste("Pilot random sample set of ", n, " articles is saved as: ", fileName, ".csv", sep=""))
  }
}



# -----------------------------------
# random_split function 
# -----------------------------------
## Description: 
#     Randomly split reference list for collaborative work between k=2 collaborators and set proportions for each split (p)
#
## Arguments: 
# - x: data frame with reference list from Rayyan
# - k: number of splits (collaborators in screening)
# - equal: logical argument to set whether the proportion of splits is equal (default is FALSE)
# - p: vector of proportions of split, it must have two positive numerical values that sum to 1. 
# - write: logical argument whether to save the pilot list as csv in current working directory (default is FALSE)
#
# Output: list of k dataframes


random_split <- function(x, k, eq=FALSE, p=NULL, write=FALSE) {
  
  # check if input is valid
  if (!is.data.frame(x)) stop("x must be a data frame")
  if (k <= 0) stop("k must be a positive number")
  if (!k%%1==0) stop("k must be a whole number")
  if (k > nrow(x)) stop("k must be a number smaller than the total number of entries in the dataframe")

  # if equal is set to true get equal proportions based on k
  if (eq==T){ p <- rep(1/k,k) }
  
  # check if vector p is valid
  if (length(p) != k) stop("p must be a vector of length k")
  if (abs(sum(p) - 1) > 1e-10) stop("p must sum to 1")
  
  # shuffle pseudo-randomly rows of the input data frame
  x <- x[sample(nrow(x)), ]
  
  # initialize list where results will be stored
  result <- vector("list", k)
  
  # split the dataframe into k parts according to the proportions in p
  start_index <- 1
  
  for (i in 1:k) {

    # compute the number of rows for this split
    n <- round(nrow(x) * p[i])
    
    # extract the rows for this split
    result[[i]] <- x[start_index:(start_index + n - 1), ]
    
    # remove empty row (in the case length of the input dataframe is not an even number)
    result[[i]] <- result[[i]][!apply(is.na(result[[i]]), 1, all),]
    
    # update the start index for the next split
    start_index <- start_index + n
    
    if (write==T){
      # save as dataframe current split
      split <- data.frame(result[[i]])
      # save dataframe in working directory as csv
      write_csv(split, paste("split", i, ".csv", sep=""), na="")
    }
  }
  
  # return the result
  return(result)

}





#############################################################################

# format for RIS export
deduplicated_refs<- mutate(deduplicated_refs, source_type=ifelse(is.na(source_type)=="TRUE", "JOUR", source_type)) # Add source type where missing
deduplicated_refs <- dplyr::select(deduplicated_refs, source_type, title, author, source, abstract, year, doi, volume, issue, start_page, end_page, language, keywords) 



####################################################################################################################
####################################################################################################################


##----------------------------------------
# Old versions (prior to December 2022)
## ---------------------------------------


# splitref_prop function was presented on the I-DEEL blog:  http://www.i-deel.org/blog/split-reference-list-helper-for-pilot-and-collaborative-screening-rounds
# Works for two collaborators
#
# -----------------------------------
# splitref_prop function 
# -----------------------------------
## Description: 
#     Randomly split reference list for collaborative work between k=2 collaborators and set proportions for each split (p)
#
## Arguments: 
# - x: data frame with reference list from Rayyan
# - p: vector of proportions of split, it must have two positive numerical values that sum to 1. 
# - fileName: provided desired suffix of split files
# - write: logical argument whether to save the pilot list as csv in current working directory


splitref_prop <- function(x, p=c(0.5, 0.5), fileName = "split", write = F) {
  
  if (length(p) == 2L && is.numeric(p) && sum(p) == 1 && all(p > 0)) {
    # get random list of indexes for each reference
    rids <- sample(1:nrow(x))
    
    # get index of row to split on using the first value of proportion of vector
    spl <- floor(p[-length(p)] * nrow(x))
    
    # get separate split data frame based on split ids indexes
    indx1 <- rids[1:spl]
    indx2 <- rids[(spl + 1):nrow(x)]
    
    # get separate datasets
    split1 <<- x[indx1,]
    split2 <<- x[indx2,]
    
    # print out message
    cat(paste(c("Reference list was randomly split into",length(p), "proportions of", p[1]*100, "% and", p[2]*100, "%")))
    
    if (write == T) {
      # save files
      write_csv(split1, paste(fileName, "_set1", ".csv", sep = ""), na ="")
      write_csv(split2, paste(fileName, "_set2", ".csv", sep = ""), na ="")
      
      # print out summary of splitting and data files saved
      p <- ifelse(length(prop) == 1, "equal", "unequal")
      cat(paste(c("\n",k,"files were saved:\n",paste(getwd(), "/", fileName, "_set1", ".csv", sep = ""))))
      
    }
    
  } else {
    # error message if provided n value is not valid
    stop("Incompatible values for p (proportions) supplied, please check.
           Proportion values must be positive integers less than 1, and the total sum of all proportions should equal to 1.")
  }
}


# Initial version of splitref function 
# ---> runs but it is missing some error checks and it is not efficient 

# # -----------------------------------
# # splitref function 
# # -----------------------------------
# ## Description: 
# #     Randomly split reference list for collaborative work
# #
# ## Arguments: 
# # - x: data frame with reference list from Rayyan
# # - k: number of collaborators/splits needed (default is 2, maximum possible is 5)
# # - prop: proportion required of each split, must be an integer between 0 and 1. Default is 0.5 (equal proportions for each split)
# # - fileName: provided desired suffix of split files
# # - write: logical argument whether to save the pilot list as csv in current working directory
# 
# splitref <- function(x, k=2, prop=0.5, fileName="split", write=F){
#   
#   # get random list of indexes for each reference
#   rids <- sample(1:nrow(x))
#   
#   # obtain cut off indexes to split reference list based on proportion given
#   if (length(prop)==1){
#     # get equal split proportions based on number of splits
#     p <- (2:k-1)*(1/k)
#     split_ids <- floor(p * nrow(x))
#   } else {
#     # error message if prop values are not valid for number of splits given
#     if (!length(prop)==k) { 
#       stop("Incompatible proportion values supplied. prop argument must be a vector with size equal to n.") 
#     } else {
#       # input proportions for split
#       split_ids <- floor(prop * nrow(x))
#     }
#   }
#   
#   
#   if(k==2){
#     # for k=2 collaborators
#     dat1_indexes <- rids[1:split_ids]
#     dat2_indexes <- rids[(split_ids+1):nrow(x)]
#     
#     # get separate datasets
#     dat1 <- x[dat1_indexes,] 
#     dat2 <- x[dat2_indexes,]
#     
#     if (write==T){
#       # save files
#       write_csv(dat1, paste(fileName, "_set1", ".csv", sep=""), na="")
#       write_csv(dat2, paste(fileName, "_set2",".csv", sep=""), na="")
#     }
#     
#   } else if(k==3){
#     # for k=3 collaborators
#     dat1_indexes <- rids[1:split_ids[1]]
#     dat2_indexes <- rids[(split_ids[1]+1):split_ids[2]]
#     dat3_indexes <- rids[(split_ids[2]+1):nrow(x)]
#     
#     # get separate datasets
#     dat1 <- x[dat1_indexes,] 
#     dat2 <- x[dat2_indexes,]
#     dat3 <- x[dat3_indexes,]
#     
#     if (write==T){
#       # save files
#       write_csv(dat1, paste(fileName, "_set1", ".csv", sep=""), na="")
#       write_csv(dat2, paste(fileName, "_set2",".csv", sep=""), na="")
#       write_csv(dat3, paste(fileName, "_set3",".csv", sep=""), na="")
#     }
#     
#   } else if (k==4){
#     # for k=4 collaborators
#     dat1_indexes <- rids[1:split_ids[1]]
#     dat2_indexes <- rids[(split_ids[1]+1):split_ids[2]]
#     dat3_indexes <- rids[(split_ids[2]+1):split_ids[3]]
#     dat4_indexes <- rids[(split_ids[3]+1):nrow(x)]
#     
#     # get separate datasets
#     dat1 <- x[dat1_indexes,] 
#     dat2 <- x[dat2_indexes,]
#     dat3 <- x[dat3_indexes,]
#     dat4 <- x[dat4_indexes,]
#     
#     if (write==T){
#       # save files
#       write_csv(dat1, paste(fileName, "_set1", ".csv", sep=""), na="")
#       write_csv(dat2, paste(fileName, "_set2",".csv", sep=""), na="")
#       write_csv(dat3, paste(fileName, "_set3",".csv", sep=""), na="")
#       write_csv(dat4, paste(fileName, "_set4",".csv", sep=""), na="")
#     }
#     
#   } else if (k==5){
#     # for k=5 collaborators
#     dat1_indexes <- rids[1:split_ids[1]]
#     dat2_indexes <- rids[(split_ids[1]+1):split_ids[2]]
#     dat3_indexes <- rids[(split_ids[2]+1):split_ids[3]]
#     dat4_indexes <- rids[(split_ids[3]+1):split_ids[4]]
#     dat5_indexes <- rids[(split_ids[4]+1):nrow(x)]
#     
#     # get separate datasets
#     dat1 <- x[dat1_indexes,] 
#     dat2 <- x[dat2_indexes,]
#     dat3 <- x[dat3_indexes,]
#     dat4 <- x[dat4_indexes,]
#     dat5 <- x[dat5_indexes,]
#     
#     if (write==T){
#       # save files
#       write_csv(dat1, paste(fileName, "_set1", ".csv", sep=""), na="")
#       write_csv(dat2, paste(fileName, "_set2",".csv", sep=""), na="")
#       write_csv(dat3, paste(fileName, "_set3",".csv", sep=""), na="")
#       write_csv(dat4, paste(fileName, "_set4",".csv", sep=""), na="")
#       write_csv(dat5, paste(fileName, "_set5",".csv", sep=""), na="")
#     }
#     
#   } else if (!k%in%(2:5)){
#     # error message if number of splits provided is not valid 
#     stop("Incompatible number of splits supplied. n must be an interger between 2 and 5.\n
#          A maximum of 5 splits is possible.")
#   }
#   
#   
#   # print out summary of splitting and data files saved
#   p <- ifelse(length(prop)==1, "equal", "unequal")
#   cat(paste(c("Reference list was randomly split into", k, "based on", p, "proportions", "(", prop, ")")))
#   cat(paste(c("\n", k, "files were saved:\n", 
#               paste(getwd(),"/",fileName, "_set1", ".csv", sep=""))))
#   
# }
