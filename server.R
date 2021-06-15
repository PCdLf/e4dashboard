function(input, output, session) {
  disable_link("tabCalendar")
  disable_link("tabVisualization")
  disable_link("tabAnalysis")
  disable_link("tabReport")


  data_in <- callModule(dataUploadModule, "data")
  
  calendar <- callModule(calendarModule, "calendar")
  
  # data_in <- reactive({
  #   readRDS("data.rds")
  # })
  
  rendered_plots <- callModule(visualizationModule, "viz", data = data_in, calendar = calendar)
  
  analysis <- callModule(analysisModule, "analysis", data = data_in)
 
  callModule(batchModule, "batch") 
  
  callModule(reportModule, "report", 
             plots = rendered_plots, 
             analysis = analysis, 
             calendar = calendar)
  
}


