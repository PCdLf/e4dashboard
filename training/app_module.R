


library(shiny)


mtcarsPlotUI <- function(id){
  
  ns <- NS(id)
  
  tagList(
    plotOutput(ns("plot_mtcars")),
    selectInput(ns("sel_x"), "X-as", choices = names(mtcars)),
    selectInput(ns("sel_y"), "Y-as", choices = names(mtcars))
  )
  
}



mtcarsPlotModule <- function(input, output, session){
  
  output$plot_mtcars <- renderPlot({
    
    xas <- input$sel_x
    yas <- input$sel_y
    
    plot(mtcars[[xas]], mtcars[[yas]], pch=19)
    
  })
  
  
}




ui <- fluidPage(
  
  fluidRow(
    column(6, 
           mtcarsPlotUI("plot1")
           ),
    column(6, 
           mtcarsPlotUI("plot2")
           )
  )


)

server <- function(input, output, session) {
  
  callModule(mtcarsPlotModule, "plot1")
  callModule(mtcarsPlotModule, "plot2")
  
  
}

shinyApp(ui, server)

