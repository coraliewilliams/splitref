# 06/10/2022
# Rayyan helper app 

# Load libraries 
library(shiny)
library(readr)
library(dplyr)
library(purrr)
library(splitstackshape)
library(wordcloud)

####
# Define UI application -----------------------------------------------------------------------------
####

ui <- fluidPage(
    
    #shinythemes::themeSelector(),

    # Set navigation page ---------------------
    
    # Title
    navbarPage(("Rayyan helper app"),
      
      #### 1: Pilot -------------------------
      tabPanel("Random Pilot",
               
               # Sidebar panel with input and output definitions
               sidebarPanel(
                 
                 # Input: Upload full reference list file  
                 fileInput("filePilot", "Choose CSV File",
                           accept = c('csv', 'comma-separated-values','.csv')),
                 
                 ),
        
        # Show a data of the generated distribution
        mainPanel(
          
          # Input: Specify number of references to randomly sample for pilot study
          numericInput("n_pilot", "Select number of references for pilot:", 10),

          
          # Output: overview of pilot set with requested number of random articles
          h4("Data table"),
          tableOutput("table"),
          
          # # Output: code verbatism
          # h5("Verbatim text output"),
          # verbatimTextOutput("txtout"),
          
          # Download button
          downloadButton("downloadData", "Download")
        )
    ),
    
    
    #### 2: Split references  -----------------------------------------
    tabPanel("Split",
             
             # Sidebar panel with input and output definitions -------
             sidebarPanel(
               
               # Input: Upload full reference list file  
               fileInput("filePilot", "Choose CSV File", accept = c(".csv")),
               
               # Copy the chunk below to make a group of checkboxes
               checkboxInput("proptype", "Proportions type"),
               
               # Input: Specify number of splits of the references list to perform
               numericInput("n_splits", "Proportions of split", 0.5, min=0, max=1),
               
               # Input: Specify number of splits of the references list to perform
               numericInput("n_splits", "", 0.5, min=0, max=1),
             ),
             
             # Show a data of the generated distribution
             mainPanel(
               
             )       
    ),

    
    
    #### 3: Word cloud ------------------------------------------------
    tabPanel("Word Cloud",
             
             # Sidebar panel with input and output definitions
             sidebarPanel(

             ),
             
             # Show a data of the generated distribution
             mainPanel(
               
               # Button
               downloadButton("downloadData", "Download")
             )       
    ),
    

             
    )
)



####
# Define logic server ----------------------------------------------------------------
####

server <- function(input, output) {
  
  # Load functions from source file ------------------------------------
  source("R/functions/functions_splitref.R")
  
  
  #### 1. Pilot functions
  
  # read csv file
  dataframe<-reactive({
    if (is.null(input$filePilot))
      return(NULL)                
    data<-read.csv(input$filePilot$datapath)
    data
  })
  
  # output csv file table
  output$table<- renderTable({
    dataframe()
  })
  

  
  
  #### 2. Split functions
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
  
}

# Run the application 
shinyApp(ui = ui, server = server)
