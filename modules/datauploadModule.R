
dataUploadUI <- function(id){
  
  ns <- NS(id)
  
  fluidPage(
    fluidRow(
      
      
      shinydashboard::box(
        width = 7,
        title = "Data input",
        
        tags$div(style = "width: 100%;",
          tags$div(style = "float: right;",
                   helpButtonUI(ns("help"))
          )
        ),
        
        tags$p("Click Browse to select E4 zip files to use in the application."),
        tags$p("The data will not be permanently stored on the server."),
        fileInput(ns("select_zip_files"),
                  label = "Choose ZIP file(s)", 
                  multiple = TRUE, 
                  accept = ".zip",
                  buttonLabel = "Browse..."),
        
        uiOutput(ns("msg_files_selected")),
        tags$br(),
        htmlOutput(ns("msg_data_read"))
      )
    )
    
  )
  
}


dataUploadModule <- function(input, output, session){
  
  rv <- reactiveValues(
    zip_files = NULL,
    data = NULL,
    timeseries = NULL,
    data_agg = NULL
  )
  
  callModule(helpButton, "help", helptext = .help$dataupload)
  
  observeEvent(input$select_zip_files, {
    
    rv$zip_files <- input$select_zip_files
    
    # Read selected ZIP files
    fns <- rv$zip_files$datapath
    fn_names <- rv$zip_files$name
    
    # Read data into a list (Each element of the list contents from 1 zip file)
    data <- list()
    n <- length(fns) + 1
    withProgress(message = "Reading data...", value = 0, {
      
      for(i in seq_along(fns)){
        
        incProgress(1/n, detail = fn_names[i])
        data[[i]] <- e4tools::read_e4(fns[i])
        
      }
      
      # If more than 1 zip file selected, row-bind them using our custom function
      incProgress(1/n, detail = "Row-binding")
      if(length(fns) > 1){
        rv$data <- e4tools::rbind_e4(data)
      } else {
        rv$data <- data[[1]]
      }
      
      # Calculate aggregated version of the data for much quicker plotting
      rv$data_agg <- e4tools::aggregate_e4_data(rv$data)
      
    })
    
    # Precalc. timeseries (for viz.)
    rv$timeseries <- list(
      EDA = e4tools::as_timeseries(rv$data_agg$EDA, name_col = "EDA"),
      HR = e4tools::as_timeseries(rv$data_agg$HR, name_col = "HR"),
      TEMP = e4tools::as_timeseries(rv$data_agg$TEMP, name_col = "Temperature"),
      MOVE = e4tools::as_timeseries(rv$data_agg$ACC, index = 5, name_col = "Movement")
    )
    
    
    # Message: data read!
    toastr_success("Data read successfully.")
    
    output$msg_data_read <- renderUI({
      
        tags$body(
          
          p("Data was uploaded and read successfully. Go to the Calendar Tab.", style = "color: blue;"),
          p("To read in a new dataset, upload a new Zip file.")
          
        )
        
      
    })

    enable_link("tabCalendar")
    enable_link("tabVisualization")
    enable_link("tabAnalysis")

    
  })
  
  
  output$msg_files_selected <- renderUI({
    
    req(rv$zip_files)
    n <- nrow(rv$zip_files)
    if(n > 0){
      tags$p(glue("You have selected {n} ZIP files."))
    }
    
  })
  
  
  out <- reactive({
    
    list(
      data = rv$data,
      data_agg = rv$data_agg,
      timeseries = rv$timeseries
    )
    
  })


return(out)
}


if(FALSE){
  
  
  library(shiny)
  
  ui <- fluidPage(
    useShinyjs(),
    
    dataUploadUI("test"),
    tags$hr(),
    verbatimTextOutput("txt_out")
  )
  
  server <- function(input, output, session) {
    
    out <- callModule(dataUploadModule, "test")
    
    output$txt_out <- renderPrint({
      str(out())
    })
    
  }
  
  shinyApp(ui, server)
  
  
  
}

