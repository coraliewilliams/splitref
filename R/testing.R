# Testing script for functions


##################### 
#### For testing ------------------------------------------
#####################


# Load functions from source file ---------------------
source("R/functions_splitref.R")



#### getpilotref tests ----

# Load list of references to tests
articles <- read_csv("data/articles.csv", show_col_types=F)

# test getpilotref function with incompatible n value
getpilotref(articles, n=-1)
getpilotref(articles, n=0)
getpilotref(articles, n=1.5)
getpilotref(articles, n=abc)
getpilotref(articles, n="100")
getpilotref(articles, n="abc")
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

