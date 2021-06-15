visSeriesOptionsUI <- function(id, y_range){
  
  ns <- NS(id)
  
  side_by_side(
    numericRangeInput(ns("num_yaxis"), "Y-axis range", value = y_range),
    tags$div(style = "padding-left: 30px;",
      radioButtons(ns("chk_line_type"), "Horizontal line", 
                         choices = c("Mean line" = "mean", "Custom" = "custom"),
                   selected = character(0), inline = TRUE)
    ),
    tags$div(style = "padding-left: 25px;",
      uiOutput(ns("ui_custom_line_val"), inline = TRUE)
    )
  )
  
}



visSeriesOptionsModule <- function(input, output, session, selected = c("mean","custom"), custom_y = 0){

  selected <- match.arg(selected)
  updateRadioButtons(session, "chk_line_type", selected = selected)
  
  output$ui_custom_line_val <- renderUI({
    
    req(input$chk_line_type)
    
    if(input$chk_line_type == "custom"){
      numericInput(session$ns("num_custom_y"), "Y-value", value = custom_y, width = 100)
    } else {
      NULL
    }
    
  })
  
  
  reactive({
    list(
      yaxis_range = input$num_yaxis,
      line_type = input$chk_line_type,
      custom_y_val = input$num_custom_y
    )
  })
  
}



if(FALSE){
  
  library(shiny)
  
  ui <- fluidPage(
    visSeriesOptionsUI("hr"),
    tags$hr(),
    verbatimTextOutput("txt_out")
  )
  
  server <- function(input, output, session) {
    out <- callModule(visSeriesOptionsModule, "hr")
    
    output$txt_out <- renderPrint(out())
    
  }
  
  shinyApp(ui, server)
  
}




