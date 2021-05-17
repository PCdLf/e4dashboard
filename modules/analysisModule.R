
analysisModuleUI <- function(id){
  
  ns <- NS(id)
  
  fluidPage(
    fluidRow(
      shinydashboard::box(width = 12, title = "Analysis", 
                          
                          tags$h4("Select begin date"),
                          
                          dateInput(ns("date_analysis_start"), label = "Select Date",
                                    value = NULL, min = NULL, max = NULL,
                                    width = 200),
                          
                          side_by_side(
                            numericInput(ns("time_hour_start"), "Hour", value = 0, width = 200),
                            numericInput(ns("time_minute_start"), "Minutes", value = 0, width = 200),
                            numericInput(ns("time_second_start"), "Seconds", value = 0, width = 200)
                          ),tags$br(),
                          
                          tags$h4("Select end date"),
                          dateInput(ns("date_analysis_end"), label = "Select Date",
                                    value = NULL, min = NULL, max = NULL,
                                    width = 200),
                          
                          side_by_side(
                            numericInput(ns("time_hour_end"), "Hour", value = 0, width = 200),
                            numericInput(ns("time_minute_end"), "Minutes", value = 0, width = 200),
                            numericInput(ns("time_second_end"), "Seconds", value = 0, width = 200)
                          ),tags$br(),
                          
                          actionButton(ns("btn_do_analysis"), "Run analysis"),
                          tags$hr(),
                          dataTableOutput(ns("dt_analysis_output")),
                          actionButton(ns("btn_download_analysis"), "Download")
                          
      )
    )
  )
  
}