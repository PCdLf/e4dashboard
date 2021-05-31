

batchModuleUI <- function(id){
  
  ns <- NS(id)
  
  fluidPage(
    fluidRow(
      
      shinydashboard::box(
        width = 6,
        title = "Batch Analysis",
        
        
       actionButton(ns("btn1"), "BUTTON")
        
        
      ),
      
      verbatimTextOutput(ns("txt_out"))
    )
    
  )
  
}

choose_directory <- function(caption = 'Select data directory') {
  if (.Platform$OS.type == "windows") {
    choose.dir(caption = caption) 
  } else {
    tk_choose.dir(caption = caption)
  }
}

batchModule <- function(input, output, session){
  
  out <- reactiveVal()
  
  observeEvent(input$btn1, {
    out(
      choose_directory()
    )
  })
  
  output$txt_out <- renderPrint(out())
  
  
}


