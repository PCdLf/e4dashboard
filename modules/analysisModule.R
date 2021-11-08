
analysisModuleUI <- function(id){
  
  ns <- NS(id)
  
  fluidPage(
    fluidRow(
      shinydashboard::box(width = 12, title = tagList(icon("chart-line"), "Analysis"),
                          
                          tags$div(style = "width: 100%; height: 30px;",
                                   tags$div(style = "float: right;",
                                            helpButtonUI(ns("help"))
                                   )
                          ),
                          
                          
                          fluidRow(
                            column(6, 
                                   
                                   tags$p("Run the signal analysis for the time period shown on the visualisation tab, 
                                          or adjust the period with the menu on the right."),
                                   tags$p("When the analysis is complete, you can download the report in a box below."),
                                   
                                   actionButton(ns("btn_do_analysis"), "Run analysis", class = "btn-success btn-lg", 
                                                icon = icon("calculator"))       
                                   
                            ),
                            column(6,
                                   tags$h4("Select begin date / time"),
                                   
                                   side_by_side(
                                     dateInput(ns("date_analysis_start"), label = "Date",
                                               value = NULL, min = NULL, max = NULL,
                                               width = 200),
                                     numericInput(ns("time_hour_start"), "Hour", value = 0, width = 100),
                                     numericInput(ns("time_minute_start"), "Minutes", value = 0, width = 100),
                                     numericInput(ns("time_second_start"), "Seconds", value = 0, width = 100)
                                   ),
                                   tags$br(),
                                   
                                   tags$h4("Select end date / time"),
                                   side_by_side(
                                     dateInput(ns("date_analysis_end"), label = "Date",
                                               value = NULL, min = NULL, max = NULL,
                                               width = 200),
                                     numericInput(ns("time_hour_end"), "Hour", value = 0, width = 100),
                                     numericInput(ns("time_minute_end"), "Minutes", value = 0, width = 100),
                                     numericInput(ns("time_second_end"), "Seconds", value = 0, width = 100)
                                   )
                                   
                            )
                          )
                          
                          
      ),
      uiOutput(ns("ui_download_report"))
    )
  )
  
}




analysisModule <- function(input, output, session, 
                           data = reactive(NULL),
                           plots = reactive(NULL),
                           calendar = reactive(NULL)){
  
  # Help Button
  callModule(helpButton, "help", helptext = .help$analysis)
  
  # Fill analysis times
  observe({
    
    data <- data()$data
    
    req(nrow(data$EDA) >0)

    tms <- range(data$EDA$DateTime)
    updateDateInput(session, "date_analysis_start",
                    value = min(as.Date(tms)),
                    min = min(as.Date(tms)),
                    max = max(as.Date(tms))
    )
    updateDateInput(session, "date_analysis_end",
                    value = max(as.Date(tms)),
                    min = min(as.Date(tms)),
                    max = max(as.Date(tms))
    )
    
    updateNumericInput(session, "time_hour_start",
                       value = hour(min(tms)), min = 0, max = 23)
    updateNumericInput(session, "time_hour_end",
                       value = hour(max(tms)), min = 0, max = 23)
    
    updateNumericInput(session, "time_minute_start",
                       value = minute(min(tms)), min = 0, max = 59)
    updateNumericInput(session, "time_minute_end",
                       value = minute(max(tms)), min = 0, max = 59)
    
    updateNumericInput(session, "time_second_start",
                       value = second(min(tms)), min = 0, max = 59)
    updateNumericInput(session, "time_second_end",
                       value = second(max(tms)), min = 0, max = 59)
    
    
  })
  
  
  last_analysis <- reactiveVal()
  
  observeEvent(input$btn_do_analysis, {
    
    toastr_warning("Analysis started - please be patient, this can take a minute or longer", 
                   timeOut = 0, extendedTimeOut = 3000)
    
    start <- ISOdatetime(
      year = year(input$date_analysis_start),
      month = month(input$date_analysis_start),
      day = day(input$date_analysis_start),
      hour = input$time_hour_start,
      min = input$time_minute_start,
      sec = input$time_second_start,
      tz = "UTC"
    )
    
    end <- ISOdatetime(
      year = year(input$date_analysis_end),
      month = month(input$date_analysis_end),
      day = day(input$date_analysis_end),
      hour = input$time_hour_end,
      min = input$time_minute_end,
      sec = input$time_second_end,
      tz = "UTC"
    )
    
    data <- data()$data
    data <- wearables::filter_e4data_datetime(data, start, end)
    
    analysis_out <- wearables::process_e4(data)
    
    last_analysis(
      analysis_out
    )

  })
  
  

  output$ui_download_report <- renderUI({
    req(last_analysis())
    shinydashboard::box(width = 12, title = tagList(icon("file-medical-alt"), "Report"), 
                        
                        tags$p("Download a report of the current analysis."),
                        downloadButton(session$ns("btn_download_report"), "Download Report",
                                       icon = icon("file-download"),
                                       class = "btn btn-lg btn-success"),
                        
                        tags$div(style = "width: 100%;",
                                 tags$div(style = "float: right;",
                                          helpButtonUI(session$ns("help-report"))
                                 )
                        )
                        
    )
    
    
  })
  
  output$btn_download_report <- downloadHandler(
    
    filename = "e4_analysis.html",
    
    content = function(file){
      
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("www/report.Rmd", tempReport, overwrite = TRUE)
      
      analysis <- last_analysis()
      calendar <- calendar()
      plots <- plots()
      
      rmarkdown::render(tempReport, output_file = file)
      
    }
    
  )
  
  callModule(helpButton, "help-report", helptext = .help$report)
  
  
  
}




if(FALSE){
  
  library(shiny)
  
  ui <- fluidPage(
    
    dataUploadUI("test"),
    tags$hr(),
    analysisModuleUI("analysis")
  )
  
  server <- function(input, output, session) {
    
    data <- callModule(dataUploadModule, "test")
    
    callModule(analysisModule, "analysis", data = data)
  }
  
  shinyApp(ui, server)
  
}

