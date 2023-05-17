# Load libraries
library(readr)
library(dplyr)
library(purrr)
library(splitstackshape) 


-----------------------------------
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



# save directly into csv files in working directory
rand_split_list <- random_split(articles, 11, p=rep(1/11,11), write=T)

# save manually
split1 <- data.frame(rand_split_list[1])
split2 <- data.frame(rand_split_list[2])
split3 <- data.frame(rand_split_list[3])
split4 <- data.frame(rand_split_list[4])
split5 <- data.frame(rand_split_list[5])
split6 <- data.frame(rand_split_list[6])
# .....