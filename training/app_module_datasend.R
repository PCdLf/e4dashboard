


library(shiny)


mtcarsPlotUI <- function(id){
  
  ns <- NS(id)
  
  tagList(
    plotOutput(ns("plot_mtcars")),
    selectInput(ns("sel_x"), "X-as", choices = names(mtcars)),
    selectInput(ns("sel_y"), "Y-as", choices = names(mtcars))
  )
  
}



mtcarsPlotModule <- function(input, output, session, data = reactive(NULL)){
  
  output$plot_mtcars <- renderPlot({
   
    xas <- input$sel_x
    yas <- input$sel_y
    
    data_in <- data()
    
    req(xas)
    req(yas)
    req(nrow(data_in) > 0)
    
    plot(data_in[[xas]], data_in[[yas]], pch=19)
    
  })
  
  
}



dataFilterUI <- function(id){
  
  ns <- NS(id)
  
  tagList(
    selectInput(ns("sel1"), "Cylinder choice", choices = unique(mtcars$cyl),
                multiple = TRUE)
  )
  
}

# data is een reactive!
dataFilterModule <- function(input, output, session, data){
  
  
  data_out <- reactive({
    
    data() %>%
      dplyr::filter(cyl %in% !!input$sel1)
    
  })
  
return(data_out)
}





ui <- fluidPage(
  
  fluidRow(
    dataFilterUI("fil1")
  ),
  
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
  
  mtcars_filtered <- callModule(dataFilterModule, "fil1", data = reactive(mtcars))
  
  callModule(mtcarsPlotModule, "plot1", data = mtcars_filtered)
  callModule(mtcarsPlotModule, "plot2", data = mtcars_filtered)
  
  
}

shinyApp(ui, server)

