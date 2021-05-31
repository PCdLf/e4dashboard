function(input, output, session) {
  # disable_link("tabCalendar")
  # disable_link("tabVisualization")
  # disable_link("tabAnalysis")
  # disable_link("tabReport")
  # 
  
  #data_in <- callModule(dataUploadModule, "data")
  
  calendar <- callModule(calendarModule, "calendar")
  
  
  data_in <- reactive({
    readRDS("data.rds")
  })
  
  callModule(visualizationModule, "viz", data = data_in, calendar = calendar)
  
  callModule(analysisModule, "analysis", data = data_in)
  
}


