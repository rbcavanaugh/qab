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
library(bslib)

source("global.R")

# Workaround for Chromium Issue 468227
downloadButton <- function(...) {
  tag <- shiny::downloadButton(...)
  tag$attribs$download <- NULL
  tag
}

# Define UI for application that draws a histogram
ui <- page_navbar(title = "QAB Macro Parser",

      nav_panel_hidden(value = "t",
        layout_sidebar(
          fillable = TRUE,
          sidebar = sidebar(width = "30%",
            div(
              fileInput("file1", "Upload QAB Macro file(s)",
                        #accept = c(".xlsx", "xls"),
                        multiple = TRUE),
              p("- Do not make any changes to the files other than entering scores in the correct boxes"),
              p("- You must fill out the `Participant` field for each form completed, otherwise scores for that form will be ignored"),
              p("- Empty forms will have a single row in the data with NA values"),
              p("- The cleaned data will be in long format, with one row per question scored")
            ),
            uiOutput("downloadButtonUI")
          ),
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
      div(
        h3("Upload QAB Macro file(s) to preview cleaned data."), br(),
        p("This web app parses QAB Excel Macro files from https://aphasialab.org/qab/ and returns a cleaned data frame in long format."),
        p("It does not save or store any data: it is only available in your browser while the app is open."),
        p("If you have concerns about security/privacy, you can clone the repository and run the shiny app locally on your computer"),
        p("The source code is located at:", tags$link("https://github.com/rbcavanaugh/qab"))
      )
    } else {
      tableOutput("contents")
    }
  })
  
  output$downloadButtonUI <- renderUI({
    if (!is.null(qab_data$data)) {
      div(align = "center",
        downloadButton("downloadData", "Download"),br(),br(),
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
