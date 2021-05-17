


calendarUI <- function(id){
  
  ns <- NS(id)
  
  fluidPage(
    fluidRow(
      shinydashboard::box(width = 8,
                          title = "Calendar",
                          
                          tags$div(id = "calendar_in_block",
                                   tags$p("Optionally, select an Excel spreadsheet with Calendar data."),
                                   tags$p("Please consult the documentation for the format of the calendar."),
                                   
                                   fileInput(ns("select_calendar_file"),
                                             label = "Choose Calendar (XLS/XLSX) file", 
                                             multiple = FALSE, 
                                             accept = c(".xls",".xlsx"),
                                             buttonLabel = "Browse...")
                          ),
                          
                          tags$br(),
                          shinyjs::hidden(
                            tags$div(id = ns("calendar_block"),
                                     tags$h4("Calendar data"),
                                     withSpinner(
                                       dataTableOutput(ns("dt_calendar"))
                                     )           
                            )
                          )
                          
      )
    )
  )
  
  
}
