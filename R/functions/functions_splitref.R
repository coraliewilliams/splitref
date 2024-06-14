# author: Coralie Williams 
# last updated: 17/05/2023

# Check if pacman package is installed and if not, install it
if (!requireNamespace("pacman", quietly = TRUE)) { install.packages("pacman")}

# Load pacman package
library(pacman)

# Install and load packages if they are not already installed
pacman::p_load(readr, dplyr, purrr, splitstackshape)



#### Description -----------------------------------------------------

# Functions to randomly select and split a list of references from an 
# exported Rayyan project for a pilot assessment or for a collaborative
# Rayyan project. 
# 
# 1. getpilotref function (line 28)
# 2. randomsplit function (line 66)

# for testing
#x <- articles
#n <- 10


#### Functions -----------------------------------------------------------

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
  if (!(is.numeric(n))) stop("n must be a integer (whole number)")
  if (n > nrow(x)) stop("n must be a smaller number than the total number of rows in the data frame")
  if (!(n %% 1 == 0)) stop("n must be a integer (whole number)")

  # sample randomly the vector n of row indexes and remove id column in the final dataset
  x$ids <- 1:nrow(x)
  pdat <- x[which(x$ids %in% sample(x$ids, n)),]
  pilotdat <<- pdat[,-which(colnames(pdat)=="ids")]
  
  if (write==T){
    # save generated pilot list in working directory using the name provided
    write_csv(pilotdat, paste(fileName, ".csv", sep=""), na="")
    # print out summary of saved file name
    cat(paste("Pilot random sample set of ", n, " articles is saved as: ", fileName, ".csv", sep=""))
  }
}



# -----------------------------------
# random_split function 
# -----------------------------------
## Description: 
#     Randomly split reference list for collaborative work between k collaborators and set proportions for each split (p)
#
## Arguments: 
# - x: data frame with reference list from Rayyan
# - k: number of splits (collaborators in screening), default is set to 2.
# - eq: logical argument to set whether the proportion of splits is equal (default is FALSE)
# - p: vector of proportions of split, it must have two positive numerical values that sum to 1. 
# - write_csv: logical argument whether to save the pilot list as csv in current working directory (default is FALSE)
# - write_csv: logical argument whether to save the pilot list as csv in current working directory (default is FALSE)
#
# Output: list of k dataframes

### Need to add in specifications for floor or ceiling depending
# then we can make the statement that the last split will be always the biggest or smallest
# due to rounding this will change => provide default setting interpretation


random_split <- function(x, k=2, eq=TRUE, p=NULL, write_csv=FALSE, write_ris=FALSE) {
  
  # check if input is valid
  if (!is.data.frame(x)) stop("x must be a data frame")
  if (k <= 0) stop("k must be a positive number")
  if (!is.numeric(k)) stop("k must be a whole number")
  if (!k%%1==0) stop("k must be a whole number")
  if (k > nrow(x)) stop("k must be a number smaller than the total number of entries in the dataframe")

  # if equal is set to true get equal proportions based on k
  if (eq==T){ 
    if (!is.null(p)){ stop("no p required, equal proportion splits is set by default") } 
    p <- rep(1/k,k) 
  }
  
  # if equal is set to false p must be provided
  if (eq==F & is.null(p)){ stop("vector of proportion splits p must be provided") } 

  # check if vector p is valid
  if (length(p) != k) stop("p must be a vector of length k")
  if (abs(sum(p) - 1) > 1e-10) stop("p must sum to 1")
  
  # shuffle pseudo-randomly rows of the input data frame
  x <- x[sample(nrow(x)), ]
  
  # initialise list where results will be stored
  result <- vector("list", k)
  
  # split the dataframe into k parts according to the proportions in p
  start_index <- 1
  
  for (i in 1:k) {
    
    if(i < k){
      # compute the number of rows for this split
      n <- round(nrow(x) * p[i])
    } else {
      # For the last split, include all remaining rows
      n <- nrow(x) - (start_index - 1)
    }
    
    # extract the rows for the current split
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
