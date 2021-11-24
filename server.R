function(input, output, session) {
  disable_link("tabCalendar")
  disable_link("tabVisualization")
  disable_link("tabAnalysis")
  disable_link("tabCut")


  data_in <- callModule(dataUploadModule, "data")
  
  calendar <- callModule(calendarModule, "calendar")

  rendered_plots <- callModule(visualizationModule, "viz", data = data_in, calendar = calendar)
  
  # Rmarkdown could not find pandoc and thus has to be set before calling the downloadhandler
  # Please note that this could be a different path depending on your computer.
  Sys.setenv(RSTUDIO_PANDOC="C:/Program Files/RStudio/bin/pandoc")
  
  callModule(analysisModule, "analysis", data = data_in, plots = rendered_plots,
             calendar = calendar)
 
  callModule(batchModule, "batch") 
  
  callModule(cutDataModule, "cut", data = data_in)
  
}


