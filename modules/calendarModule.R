

read_calendar <- function(fn){
  
  ext <- tolower(tools::file_ext(fn))
  
  rectify_datetime <- function(date, time){
    ISOdatetime(year(date), month(date), day(date), 
                hour(time), minute(time), second(time))  
  }
  
 switch(ext,
       xls = read_excel(fn),
       xlsx = read_excel(fn),
       txt = read.csv2(fn)
       ) %>% 
    as_tibble() %>%
    mutate(Date = as.Date(Date),  ## ????
           Start = rectify_datetime(Date, Start),
           End = rectify_datetime(Date, End))
  
}




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
                                             label = "Choose Calendar (XLS/XLSX/TXT) file", 
                                             multiple = FALSE, 
                                             accept = c(".xls",".xlsx", ".txt"),
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



calendarModule <- function(input, output, session){
  
  calendar_out <- reactiveVal()
  
  observeEvent(input$select_calendar_file, {
    
    dfr <- input$select_calendar_file
    
    calendar_out(
      read_calendar(dfr$datapath)
    )
    
    shinyjs::hide("calendar_in_block")
    shinyjs::show("calendar_block")
  })
  
  # Make sure to use DT:: to use the right renderDataTable (shiny has an old version)
  output$dt_calendar <- DT::renderDataTable({
    
    req(calendar_out())
    
    calendar_out() %>%
      mutate(Date = format(Date, "%Y-%m-%d"),
             Start = format(Start, "%H:%M:%S"),
             End = format(End, "%H:%M:%S")
      ) %>% 
      datatable()
    
  })
  
  
return(calendar_out)
}




if(FALSE){
  
  
  library(shiny)
  
  ui <- fluidPage(
    useShinyjs(),
    calendarUI("cal"),
    tags$hr(),
    verbatimTextOutput("txt_out")
    
  )
  
  server <- function(input, output, session) {
    
    cal <- callModule(calendarModule, "cal")
    
    output$txt_out <- renderPrint({
      str(cal())
    })
  }
  
  shinyApp(ui, server)
  
  
}

