# 06/10/2022
# Split Reference List

# Load libraries 
library(shiny)
library(readr)
library(dplyr)
library(purrr)
library(splitstackshape)

####
# Define UI application -----------------------------------------------------------------------------
####

ui <- fluidPage(
    
    #shinythemes::themeSelector(),

    # Set navigation page ---------------------
    
    # Title
    navbarPage(("Split reference list"),
      
      #### 1: Pilot -------------------------
      tabPanel("Random Pilot",
               
               # Sidebar panel with input and output definitions
               sidebarPanel(
                 
                 # Input: Upload full reference list file  
                 fileInput("filePilot", "Choose CSV File",
                           accept = c(".csv")),
                 
                 # Input: Specify number of references to randomly sample for pilot study
                 numericInput("n_pilot", "Select number of references for pilot:", 10),
                 
                 # Action button
                 actionButton("action2", "Action button", class = "btn-primary")
                 
                 ),
        
        # Show a data of the generated distribution
        mainPanel(
          
          # Output: overview of pilot set with requested number of random articles
          h4("Random Pilot Set"),
          tableOutput("pilot"),
          
          # Output: code verbatism
          h5("Verbatim text output"),
          verbatimTextOutput("txtout"),
          
          # Download button
          downloadButton("downloadData", "Download")
        )
    ),
    
    
    #### 2: Split references (k=2 collab) -----------------------------------------
    tabPanel("Split",
             
             # Sidebar panel with input and output definitions -------
             sidebarPanel(
               
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
  

  #### 1. Pilot functions
  
  # Random pilot set output
  output$pilot <- renderTable({
    
    # get dataframe of articles
    dat <- getpilotref(input$filePilot, n=output$n_pilot)
    
    if (is.null(dat))
        return(NULL)
    
    read_csv(dat$datapath, show_col_types=F)
  })
  
  
  
  #### 2. Split ref functions
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
