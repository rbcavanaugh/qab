#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(readxl)

source("global.R")

# Workaround for Chromium Issue 468227
downloadButton <- function(...) {
  tag <- shiny::downloadButton(...)
  tag$attribs$download <- NULL
  tag
}

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("QAB Macro Parser"),
    
    sidebarLayout(
      sidebarPanel(
        div(
          h4("Upload one or more QAB Macro files"), br(),
          p("- Do not make any changes to the files other than entering scores in the correct boxes"),
          p("- You must fill out the `Participant` field for each form completed, otherwise scores will be ignored"),
          p("- Forms not completed will have a single row in the data with NA values"),
          p("- The cleaned data will be in long format, with one row per question scored")
        ),br(),
        fileInput("file1", "Choose a file",
                  #accept = c(".xlsx", "xls"),
                  multiple = TRUE),
        uiOutput("downloadButtonUI")
        
        # downloadButton("downloadData", "Download Result")
        ),
      mainPanel(
        div(style = "overflow-y: auto;height: 50vh; max-height: 50vh; width: 100%;",
          uiOutput("results")
        )
        
      )
    )

    # file input for directory or excel file
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  qab_data <- reactiveValues(data = NULL)
  
  observeEvent(input$file1,{
    file <- input$file1
    req(file)
    files = file$datapath
    filenames = c(file$name)
    #print(files)
    #print(filenames)
   # print(file)
    

    if( length(files) > 1 ){
      df <- tryCatch(
        {
          #do.call("rbind", Map(clean_qab_macro, files, filenames))
          #do.call("rbind", Map(clean_qab_sheet, files, filenames))
          do.call("rbind", lapply(1:length(files), function(i) do.call("rbind", Map(clean_qab_sheet, files, filenames)[[i]])))
          
        },
        error = function(e) {
          # Handle the error
          message("An error occurred: ", e$message)
          # Return a default value or take appropriate action
          return(NULL)
        },
        warning = function(w) {
          # Handle warnings if needed
          showNotification(paste0("Missing data were found: ", w$message), type = "error")
          # Return the result or take appropriate action
          return(
            
            #do.call("rbind", lapply(c(files), clean_qab_macro))
            suppressWarnings(
              suppressMessages(
                do.call("rbind", lapply(1:length(files), function(i) do.call("rbind", Map(clean_qab_sheet, files, filenames)[[i]])))
              )
            )
            
            
            )
        },
        finally = {
          # Code to be executed regardless of success or failure
        }
      )  
      
    } else {

      df <- tryCatch(
        {
          do.call("rbind",clean_qab_sheet(files, filenames))
        },
        error = function(e) {
          # Handle the error
          message("An error occurred: ", e$message)
          # Return a default value or take appropriate action
          return(NULL)
        },
        warning = function(w) {
          # Handle warnings if needed
          showNotification(paste0("Missing data were found: ", w$message), type = "error")
          return(
            suppressWarnings(
              suppressMessages(
              #  clean_qab_sheet(files, filenames))
              do.call("rbind",clean_qab_sheet(files, filenames))
              )
            )
          )
          # Return the result or take appropriate action
        },
        finally = {
          # Code to be executed regardless of success or failure
        }
      )  
    }
    
    qab_data$data = df
  })
  
  output$contents <- renderTable({
    qab_data$data
  })
  
  
  output$results <- renderUI({
    if(is.null(qab_data$data)){
      h3("Upload QAB Macro file(s) to preview cleaned data.")
    } else {
      tableOutput("contents")
    }
  })
  
  output$downloadButtonUI <- renderUI({
    if (!is.null(qab_data$data)) {
      div(
        downloadButton("downloadData", "Download Processed Data"),
        actionButton("reset", "Reset")
      )
    } 
  })
  
  observeEvent(input$reset, {
    qab_data$data <- NULL
    #reset("file1")
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("QAB-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(qab_data$data, file)
    }
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)
