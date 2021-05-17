


library(shiny)

ui <- fluidPage(
  
  fluidRow(
    column(6, 
           plotOutput("plot1"),
           selectInput("sel_x", "X-as", choices = names(mtcars)),
           selectInput("sel_y", "Y-as", choices = names(mtcars))
           ),
    column(6, 
           plotOutput("plot2"),
           selectInput("sel_x", "X-as", choices = names(mtcars)),
           selectInput("sel_y", "Y-as", choices = names(mtcars))
           )
  )


)

server <- function(input, output, session) {
  
  
  output$plot1 <- renderPlot({
    
    xas <- input$sel_x
    yas <- input$sel_y
    
    plot(mtcars[[xas]], mtcars[[yas]], pch=19)
    
  })
  
  
  output$plot2 <- renderPlot({
    
    xas <- input$sel_x
    yas <- input$sel_y
    
    plot(mtcars[[xas]], mtcars[[yas]], pch=19)
    
  })
  
}

shinyApp(ui, server)

