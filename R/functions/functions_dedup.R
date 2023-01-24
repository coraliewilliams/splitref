# author: Coralie Williams 
# last updated: 23/01/2023

# Load libraries
library(readr)
library(dplyr)
library(tidyverse) # https://www.tidyverse.org/
library(synthesisr) # https://CRAN.R-project.org/package=synthesisr
library(revtools) # https://revtools.net/


##################### 
#### Description ----------------------------------------------------------------------------------
#####################

# Functions to deduplicate bibliometric records exported from Rayyan.

##################### 
#### Testing ------------------------------------------------------------------------------------
#####################

# Code from blog article by Malgorzata Lagisz: https://doi.org/10.53962/rmj5-tra3 


# Load exported file from Rayyan into R ------------
dat <- read.csv("FILENAME.csv") #load the file
dim(dat) #see the initial number of uploaded references


# Prepare data for deduplication ----------
dat$title2 <- stringr::str_replace_all(dat$title,"[:punct:]","") %>% str_replace_all(.,"[ ]+", " ") %>% tolower() # Removing all punctuation and extra white spaces


# Remove title matches (removes exact duplicates) ----------
dat2 <- distinct(dat, title2, .keep_all = TRUE) #reduce to records with unique titles
dim(dat2) #see the new number of records
#View(arrange(dat2, title2)$title2) #an optional visual check - sorted titles



# Deduplicate by fuzzy matching remaining titles ----------
duplicates_string <- synthesisr::find_duplicates(dat2$title2, method = "string_osa", to_lower
                                                 = TRUE, rm_punctuation = TRUE, threshold = 7)

dim(manual_checks) #number of duplicated records found

# optional visual check of the list of duplicates detected. 
View(review_duplicates(dat2$title2, duplicates_string))

# If needed, you can manually mark some records as unique (not duplicates) by providing
# their new record number from duplicates_string (duplicates have the same record number), e.g.
new_duplicates <- synthesisr::override_duplicates(duplicates_string, 34)

#extract unique references (i.e.remove fuzzy duplicates)
dat3 <- extract_unique_references(dat2, duplicates_string) 
dim(dat3) #new number of unique records


# Prepare data for exporting -----------------------------------

#select the key columns
dat3 %>% select(key, title, authors, journal, issn, volume, issue, pages, day, month, year,
                publisher, pmc_id, pubmed_id, url, abstract, language) -> dat4 

#save into a bib file
write_refs(dat4, format = "bib", file = "FILENAME_deduplicated.bib") 

#fix the record labels and save again as a .bib file
readLines("FILENAME_deduplicated.bib") %>%
  stringr::str_replace(
    pattern = "@ARTICLE",
    replace = "@article") %>%
  writeLines(con = " FILENAME_deduplicated.bib") 





##################### 
#### Functions ------------------------------------------------------------------------------------
#####################




# -----------------------------------
# dedup function 
# -----------------------------------
## Description: 
#     Deduplicate 
#
# Arguments
# - x: data frame with reference list from Rayyan
# - n: number of papers needed for pilot set
# - fileName: name of file with list of articles for pilot study
# - write: logical argument whether to save the pilot list as csv in current working directory








# -----------------------------------
# dedupfuzzy function 
# -----------------------------------
## Description: 
#     Deduplicate 
#
# Arguments
# - x: data frame with reference list from Rayyan
# - n: number of papers needed for pilot set
# - fileName: name of file with list of articles for pilot study
# - write: logical argument whether to save the pilot list as csv in current working directory





