
analysisModuleUI <- function(id){
  
  ns <- NS(id)
  
  fluidPage(
    fluidRow(
      shinydashboard::box(width = 12, title = "Analysis", 
                          
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
                          ),
                          tags$br(),
                          
                          actionButton(ns("btn_do_analysis"), "Run analysis", class = "btn-success", 
                                       icon = icon("calculator")),
                          tags$hr(),
                          shinyjs::hidden(
                            actionButton(ns("btn_download_analysis"), "Download", icon = icon("download"),
                                         class = "btn-success")  
                          ),
                          verbatimTextOutput(ns("analysis_out"))
                          
                          
                          
      )
    )
  )
  
}




analysisModule <- function(input, output, session, data = reactive(NULL)){
  
  
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
    
    toastr_info("Analysis started - this can take a few seconds.")
    
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
    
    data <- filter_e4data_datetime(data, start, end)
    
    ibi_analysis <-  e4tools::ibi_analysis(data$IBI)
    
    eda_filt <- e4tools::process_eda(data$EDA)
    eda_feat <- compute_features2(eda_filt)
    
    last_analysis(
      list(
        ibi = ibi_analysis,
        eda = eda_feat
      )
    )

  })

  observe({
    
    last_analysis <- last_analysis()
    req(last_analysis)
    shinyjs::show("btn_download_analysis")
    enable_link("tabReport")
    
  })

  output$analysis_out <- renderPrint({
    last_analysis()
  })
  
  
return(last_analysis)
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

