
dataUploadUI <- function(id){
  
  ns <- NS(id)
  
  fluidPage(
    fluidRow(
      
      shinydashboard::box(
        width = 6,
        title = "Data input",
        tags$p("Click Browse to select E4 zip files to use in the application"),
        tags$p("The data will not be permanently stored on the server."),
        fileInput(ns("select_zip_files"),
                  label = "Choose ZIP file(s)", 
                  multiple = TRUE, 
                  accept = ".zip",
                  buttonLabel = "Browse..."),
        
        uiOutput(ns("msg_files_selected")),
        tags$br(),
        tags$br(),
        
        shinyjs::hidden(
          actionButton(ns("btn_read_data"), "Read data", icon = icon("chart-line"),
                       class = "btn btn-success")
        ),
        shinyjs::hidden(
          actionButton(ns("btn_reset"), "Start over", icon = icon("refresh"))
        ),
        htmlOutput(ns("msg_data_read"))
      )
    )
    
  )
  
}