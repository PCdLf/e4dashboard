

helpButtonUI <- function(id){
  
  ns <- NS(id)
  
  
  actionButton(ns("btn_help"), "Help", icon = icon("question-circle"), class = "btn-help")
  
}


helpButton <- function(input, output, session, helptext = "Your help text here!<br><p>And another line!</p>"){
  
  
  observeEvent(input$btn_help, {
    
    
    showModal(
      modalDialog(
        title = tagList(icon("question-circle"), "Help"),
        size = "m",
        easyClose = TRUE,
        
        HTML(helptext),
        
        footer = actionButton("iubdvfiufdbv", "Close", class = "btn-success",
                              `data-dismiss`="modal")
        
      )
    )
    
    
  })
  
  
}



if(FALSE){
  library(shiny)
  
  ui <- fluidPage(
    
    helpButtonUI("help1")
    
  )
  
  server <- function(input, output, session) {
    
    callModule(helpButton, "help1", helptext = "test test<br> test test")
    
  }
  
  shinyApp(ui, server)
  
}

