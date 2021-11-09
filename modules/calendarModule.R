

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


validate_calendar <- function(data){
  
  nms <- c("Date" ,"Start", "End" , "Text")
  
  all(nms %in% names(data))
  
}

calendar_add_color <- function(data){
  
  if(!"Color" %in% names(data)){
    
    data$Color <- .cc$visualisation$default_color
    
  }
  
  return(data)
}



calendarUI <- function(id){
  
  ns <- NS(id)
  
  fluidPage(
    fluidRow(
      shinydashboard::box(width = 8,
                          title = "Calendar",
                          
                          tags$div(style = "width: 100%;",
                                   tags$div(style = "float: right;",
                                            helpButtonUI(ns("help"))
                                   )
                          ),
                          
                          tags$div(id = "calendar_in_block",
                                   tags$p("Optionally, select an Excel spreadsheet or textfile with Calendar data."),
                                   tags$p("Please consult the documentation or Help button for the format of the calendar."),
                                   
                                   side_by_side(
                                     fileInput(ns("select_calendar_file"),
                                               label = "Choose Calendar (XLS/XLSX/TXT) file", 
                                               multiple = FALSE, 
                                               accept = c(".xls",".xlsx", ".txt"),
                                               width = 300,
                                               buttonLabel = "Browse..."),
                                     tags$div(style = "padding-left: 50px; padding-top: 25px;",
                                              actionButton(ns("btn_use_example_data_large"), "Use large example data", 
                                                           icon = icon("male"), class = "btn-info"),
                                              actionButton(ns("btn_use_example_data_small"), "Use small example data", 
                                                           icon = icon("child"), class = "btn-info")
                                     )
                                   )
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
  calendar_file <- reactiveVal()
  
  callModule(helpButton, "help", helptext = .help$calendar)
  
  observeEvent(input$btn_use_example_data_large, {
    calendar_file(
      data.frame(
        name = "Calendar_new format_Saskia.xlsx",
        size = NA,
        type = NA,
        datapath = "www/example_data/Calendar_new format_Saskia.xlsx"
      )
    )
  })
  
  observeEvent(input$btn_use_example_data_small, {
    calendar_file(
      data.frame(
        name = "Calendar_Peter.xlsx",
        size = NA,
        type = NA,
        datapath = "www/example_data/Calendar_Peter.xlsx"
      )
    )
  })
  
  observeEvent(input$select_calendar_file, {
    calendar_file(input$select_calendar_file)
  })
  
  observeEvent(calendar_file(), {
    
    data <- read_calendar(calendar_file()$datapath)
    
    if(!validate_calendar(data)){
      toastr_error("Calendar data must have columns Date, Start, End, Text, (Color), click Help!")
    } else {
      
      data <- calendar_add_color(data)
      
      calendar_out(
        data
      )
      
      shinyjs::hide("calendar_in_block")
      shinyjs::show("calendar_block")  
    }
    
    
  })
  
  # Make sure to use DT:: to use the right renderDataTable (shiny has an old version)
  output$dt_calendar <- DT::renderDataTable({
    
    req(calendar_out())
    
    calendar_out() %>%
      mutate(Date = format(Date, "%Y-%m-%d"),
             Start = format(Start, "%H:%M:%S"),
             End = format(End, "%H:%M:%S")
      ) %>% 
      datatable(selection = "none")
    
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

