# load required libraries
library(shiny)
library(tidyverse)

# define the random_split() function
random_split <- function(df, k, p) {
  # check if input is valid
  if (k <= 0) stop("k must be a positive number")
  if (length(p) != k) stop("p must be a vector of length k")
  if (abs(sum(p) - 1) > 1e-10) stop("p must sum to 1")
  
  # shuffle the rows of the dataframe
  df <- df[sample(nrow(df)), ]
  
  # initialize the result list
  result <- vector("list", k)
  
  # split the dataframe into k parts according to the proportions in p
  start_index <- 1
  
  for (i in 1:k) {
    # compute the number of rows for this split
    n <- round(nrow(df) * p[i])
    
    # extract the rows for this split
    result[[i]] <- df[start_index:(start_index + n - 1), ]
    
    # update the start index for the next split
    start_index <- start_index + n
  }
  
  # return the result
  return(result)
}

# create a Shiny app
shinyApp(
  # define the user interface
  ui = fluidPage(
    # add a file input control
    fileInput("file", "Choose CSV file"),
    
    # add a numeric input control for k
    numericInput("k", "Number of splits", value = 2),
    
    # add an output for the proportions
    uiOutput("proportions"),
    
    # add an action button to submit the form
    actionButton("submit", "Split data"),
    
    # add an output for the data subsets
    uiOutput("subsets"),
    
    # add an action button to download the subsets
    downloadButton("download", "Download subsets")
  ),
  
  # define the server logic
  server = function(input, output) {
    # define a reactive value to store the dataframe
    df <- reactiveVal()
    
    # when the file is uploaded, store the dataframe in the reactive value
    observeEvent(input$file, {
      df(read_csv(input$file$datapath))
    })
    
    # create the proportions input controls dynamically based on the value of k
    output$proportions <- renderUI({
      tagList(
        lapply(1:input$k, function(i) {
          numericInput(
            paste0("p", i),
            paste0("Proportion for split ", i),
            value = 1 / input$k
          )
        })
      )
    })
    
    # create the data subsets output
    output$subsets <- renderUI({
      # check if the dataframe is available
      if (is.null(df())) return()
      
      # split the dataframe into k subsets
      subsets <- random_split(df(), input$k, sapply(1:input))
      
    })
    
  })
                                                    