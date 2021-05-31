reportModuleUI <- function(id){
  ns <- NS(id)
  
  fluidPage(
    fluidRow(
      shinydashboard::box(width = 8, title = "Report", 
                          
                          
                          tags$p("Download a report of the current analysis."),
                          downloadButton(ns("btn_download_report"), "Download Report",
                                         icon = icon("file-download"),
                                         class = "btn btn-larger btn-success")
                          
                          
      )
    )
  )
  
}


reportModule <- function(input, output, session, 
                         calendar = reactive(NULL),
                         analysis = reactive(NULL)){
  
  
  # See https://shiny.rstudio.com/articles/generating-reports.html
  output$btn_download_report <- downloadHandler(
    
    filename = "e4_analysis.html",
    
    content = function(file){
      
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("www/report.Rmd", tempReport, overwrite = TRUE)
      
      analysis <- analysis()
      calendar <- calendar()
      
      rmarkdown::render(tempReport, output_file = file)
      
    }
    
  )
  
}





