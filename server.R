function(input, output, session) {
  
  
  data_in <- callModule(dataUploadModule, "data")
  
  calendar <- callModule(calendarModule, "calendar")
  
  
  callModule(visualizationModule, "viz", data = data_in, calendar = calendar)
  
  callModule(analysisModule, "analysis", data = data_in)
  
  
  # 
  # 
  # enable_link("tabCalendar")
  # enable_link("tabVisualization")
  # enable_link("tabAnalysis")
  # 
  

  
}


