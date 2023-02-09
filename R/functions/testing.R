# Testing script for functions


##################### 
#### For testing ------------------------------------------
#####################

# Library
library(tidyr)
library(readr)

# Load functions from source file ---------------------
source("R/functions/functions_splitref.R")


#### getpilotref tests ----

# Load list of references to tests
articles <- read_csv("data/articles.csv", show_col_types=F)

# test getpilotref function with incompatible n value
getpilotref(articles, n=-1)
getpilotref(articles, n=0)
getpilotref(articles, n=1.8)
getpilotref(articles, n=abc)
getpilotref(articles, n="100")
getpilotref(articles, n="coralie")
getpilotref(articles, n=1000)

# test write argument 
getpilotref(articles, n=2, write=T)



#### splitref_prop tests ----

# quick test
splitref_prop(articles)

# test getpilotref function with incompatible p value
splitref_prop(articles, p=c(0.1,0.8))
splitref_prop(articles, p=c(0.8))
splitref_prop(articles, p=c("ajkdnafk"))




#### random_ tests ----

# save directly into csv files in working directory
rand_split_list <- random_split(articles, 3, p=c(1/3,1/3,1/3), write=T)

# manually 
rand_split_list <- random_split(articles, 3, p=c(1/3,1/3,1/3))
split1 <- data.frame(rand_split_list[1])
split2 <- data.frame(rand_split_list[2])
split3 <- data.frame(rand_split_list[3])
# save files
write_csv(split1, "~/PhD/1_Sim/2_SysReview/screening/simstudies_split1.csv", na="")
write_csv(split2, "~/PhD/1_Sim/2_SysReview/screening/simstudies_split2.csv", na="")
write_csv(split3, "~/PhD/1_Sim/2_SysReview/screening/simstudies_split3.csv", na="")



# ---------------------------------------------------------
# create a sample dataframe
df <- data.frame(x = 1:10, y = 11:20)

# split the dataframe into 3 subsets with proportions 0.2, 0.5, and 0.3
subsets <- random_split(df, 3, c(0.5, 0.3, 0.2))
list2env(subsets)
