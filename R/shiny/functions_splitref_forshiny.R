# Install required packages
#install.packages(c("tm", "wordcloud","SnowballC"))

# Load libraries
library(readr)
library(dplyr)
library(purrr)
library(splitstackshape)


## Split References Helper Functions (for Shiny) ------------------------------------------------------------------------------------------


#### for testing
datdir <- rprojroot::find_rstudio_root_file("splitRef")
filePath <- file.path(datdir, "articles.csv")
fileName <- "pilot"
nPapers <- 15
# read full list of articles
articles <- read_csv(filePath, show_col_types=F)


# -----------------------------------
# getpilotref function 
# -----------------------------------
#   Randomly subset a pilot set of references
#
# Parameters
# - filePath: path to csv file with full list of articles
# - n: number of papers needed for pilot set
# - fileName: name of file with list of articles for pilot study

getpilotref <- function(data, n=10){
  
  # get dataframe of articles
  articles <- tibble(data)
  # split
  pilot <- articles[which(sample(articles$key, nPapers) %in% articles$key),]
  # print out
  cat(paste(c("Pilot random sample set of", nPapers)))
  # save output
  return(pilot)
}


p <- getpilotref(data=articles, df)


#### for testing
n <- 3
prop <- c(0.25,0.25,0.5)

# -----------------------------------
# splitref function 
# -----------------------------------
#   Randomly split reference list for collaborative work
#
# Parameters
# - filePath: path to csv file with full list of articles
# - n: number of collaborators/splits needed (default is 2, maximum possible is 5)
# - prop: proportion required of each split, must be an integer between 0 and 1. Default is 0.5 (equal proportions for each split)
# - seed: set seed for reproducibility of random splitting
# - fileName: provided desired suffix of split files

splitref <- function(filePath, n=2, prop=0.5, seed=123, fileName="split"){
  
  # read full list of articles
  df <- read_csv(filePath,show_col_types = FALSE)
  
  # get random list of indexes for each reference
  rids <- sample(1:nrow(df))
  
  # obtain cut off indexes to split reference list based on proportion given
  if (length(prop)==1){
    # get equal split proportions based on number of splits
    p <- (2:n-1)*(1/n)
    split_ids <- floor(p * nrow(df))
  } else {
    # error message if prop values are not valid for number of splits given
    if (!length(prop)==n) { 
      stop("Incompatible proportion values supplied. prop argument must be a vector with size equal to n.") 
      } else {
        # input proportions for split
        ratio <- prop
        split_ids <- floor(ratio * nrow(df))
    }
  }

  
  if(n==2){
    # for n=2 collaborators
    dat1_indexes <- rids[1:split_ids]
    dat2_indexes <- rids[(split_ids+1):nrow(df)]
    
    # get separate datasets
    dat1 <- df[dat1_indexes,] 
    dat2 <- df[dat2_indexes,]
    
    # save files
    write_csv(dat1, paste(fileName, "_set1", ".csv", sep=""), na="")
    write_csv(dat2, paste(fileName, "_set2",".csv", sep=""), na="")
    
  } else if(n==3){
    # for n=3 collaborators
    dat1_indexes <- rids[1:split_ids[1]]
    dat2_indexes <- rids[(split_ids[1]+1):split_ids[2]]
    dat3_indexes <- rids[(split_ids[2]+1):nrow(df)]
    
    # get separate datasets
    dat1 <- df[dat1_indexes,] 
    dat2 <- df[dat2_indexes,]
    dat3 <- df[dat3_indexes,]
    
    # save files
    write_csv(dat1, paste(fileName, "_set1", ".csv", sep=""), na="")
    write_csv(dat2, paste(fileName, "_set2",".csv", sep=""), na="")
    write_csv(dat3, paste(fileName, "_set3",".csv", sep=""), na="")
    
  } else if (n==4){
    # for n=4 collaborators
    dat1_indexes <- rids[1:split_ids[1]]
    dat2_indexes <- rids[(split_ids[1]+1):split_ids[2]]
    dat3_indexes <- rids[(split_ids[2]+1):split_ids[3]]
    dat4_indexes <- rids[(split_ids[3]+1):nrow(df)]
    
    # get separate datasets
    dat1 <- df[dat1_indexes,] 
    dat2 <- df[dat2_indexes,]
    dat3 <- df[dat3_indexes,]
    dat4 <- df[dat4_indexes,]
    
    # save files
    write_csv(dat1, paste(fileName, "_set1", ".csv", sep=""), na="")
    write_csv(dat2, paste(fileName, "_set2",".csv", sep=""), na="")
    write_csv(dat3, paste(fileName, "_set3",".csv", sep=""), na="")
    write_csv(dat4, paste(fileName, "_set4",".csv", sep=""), na="")
    
  } else if (n==5){
    # for n=4 collaborators
    dat1_indexes <- rids[1:split_ids[1]]
    dat2_indexes <- rids[(split_ids[1]+1):split_ids[2]]
    dat3_indexes <- rids[(split_ids[2]+1):split_ids[3]]
    dat4_indexes <- rids[(split_ids[3]+1):split_ids[4]]
    dat5_indexes <- rids[(split_ids[4]+1):nrow(df)]
    
    # get separate datasets
    dat1 <- df[dat1_indexes,] 
    dat2 <- df[dat2_indexes,]
    dat3 <- df[dat3_indexes,]
    dat4 <- df[dat4_indexes,]
    dat5 <- df[dat5_indexes,]
    
    # save files
    write_csv(dat1, paste(fileName, "_set1", ".csv", sep=""), na="")
    write_csv(dat2, paste(fileName, "_set2",".csv", sep=""), na="")
    write_csv(dat3, paste(fileName, "_set3",".csv", sep=""), na="")
    write_csv(dat4, paste(fileName, "_set4",".csv", sep=""), na="")
    write_csv(dat5, paste(fileName, "_set5",".csv", sep=""), na="")
    
  } else if (!n%in%(2:5)){
    # error message if number of splits provided is not valid 
    stop("Incompatible number of splits supplied. n must be an interger between 2 and 5.\n
         A maximum of 5 splits is possible.")
  }
  

  # print out summary of splitting and data files saved
  p <- ifelse(length(prop)==1, "equal", "unequal")
  cat(paste(c("Reference list was randomly split into", n, "based on", p, "proportions", "(", prop, ")")))
  cat(paste(c(n, "files were saved:\n", 
              paste(getwd(),"/",fileName, "set1", ".csv", sep=""))))
  
}




##### Word cloud -----------------------------------------------

# Load libraries
library(tm);library(wordcloud);library(SnowballC)

## select variable to show on word cloud
v <- df$title

## format and clean the text
# Strip unnecessary whitespace
v <- tm_map(v, stripWhitespace)
# Convert to lowercase
v <- tm_map(v, tolower)
# Remove conjunctions etc.
v <- tm_map(v, removeWords, stopwords("english"))
# Remove suffixes to the common 'stem'
v <- tm_map(v, stemDocument)
# Remove commas etc.
v <- tm_map(v, removePunctuation)

## test word cloud
wordcloud(v
          , scale=c(5,0.5)     # Set min and max scale
          , max.words=100      # Set top n words
          , random.order=FALSE # Words in decreasing freq
          , rot.per=0.35       # % of vertical words
          , use.r.layout=FALSE # Use C++ collision detection
          , colors=brewer.pal(8, "PRGn"))


# -----------------------------------
# titlewordcloud function 
# -----------------------------------
#   Check whether two files contain duplicate reference entries
#
# Parameters
# - filePath: path to csv file with full list of articles
# - nCollab: number of collaborators/splits needed (default is 2)
# - nProp: proportion in % of each split (default is equal proportions to each split)

checkDupl <- function(filePath, nCollab=2, nProp=0.5, fileName="pilot"){
  
  # read full list of articles
  fullSet <- read_csv(filePath)
  # get a random sample of 
  pilot <- articles[which(sample(articles$key, nPapers) %in% articles$key),]
  # save list of articles for pilot study
  fullFileName <- paste(fileName, ".csv", sep="")
  write_csv(pilot, fullFileName, na="")
  
  # print out
  cat(paste(c("Pilot random sample set of", nPapers, "articles is saved in:", fullFileName)))
}








# -----------------------------------
# checkdup function 
# -----------------------------------
#   Check whether two files contain duplicate reference entries
#
# Parameters
# - filePath: path to csv file with full list of articles
# - nCollab: number of collaborators/splits needed (default is 2)
# - nProp: proportion in % of each split (default is equal proportions to each split)

checkDupl <- function(filePath, nCollab=2, nProp=0.5, fileName="pilot"){
  
  # read full list of articles
  fullSet <- read_csv(filePath)
  # get a random sample of 
  pilot <- articles[which(sample(articles$key, nPapers) %in% articles$key),]
  # save list of articles for pilot study
  fullFileName <- paste(fileName, ".csv", sep="")
  write_csv(pilot, fullFileName, na="")
  
  # print out
  cat(paste(c("Pilot random sample set of", nPapers, "articles is saved in:", fullFileName)))
}


