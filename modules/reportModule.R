reportModuleUI <- function(id){
  ns <- NS(id)
  
  fluidPage(
    fluidRow(
      shinydashboard::box(width = 8, title = "Report", 
                          
                          
                          tags$p("Download a report of the current analysis."),
                          downloadButton("btn_download_report", "Download Report",
                                         icon = icon("file-download"),
                                         class = "btn btn-larger btn-success")
                          
                          
      )
    )
  )
  
}

