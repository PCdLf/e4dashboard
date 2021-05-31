

visualizationModuleUI <- function(id){
  
  ns <- NS(id)
  
  fluidPage(
    fluidRow(
      tabBox(width = 12, title = "Visualization", id = "plottabbox",
             tabPanel(
               title = tagList(icon("cogs"), "Settings"),
               value = "settingstab",
               
               fluidPage(
                 fluidRow(
                   column(6,
                          tags$label(class = "control-label", "Annotations"),
                          checkboxInput(ns("check_add_calendar_annotation"), 
                                        label = "Calendar events",
                                        value = TRUE),
                          textInput(ns("txt_plot_main_title"), "Title"),
                          checkboxGroupInput(ns("check_average_lines"), "Add averages", 
                                             choices = c("EDA","HR","TEMP","MOVE"))
                   ),
                   column(6,
                          
                          numericRangeInput(ns("slide_yaxis_eda"), "EDA Y-axis range", value = c(0,20)),
                          numericRangeInput(ns("slide_yaxis_hr"),  "HR Y-axis range", value = c(40, 160)),
                          numericRangeInput(ns("slide_yaxis_temp"),"TEMP Y-axis range", value = c(24, 38)),
                          numericRangeInput(ns("slide_yaxis_move"),"MOVE Y-axis range", value = c(0.98, 1.25))
                   )
                 ),
                 fluidRow(
                   actionButton(ns("btn_make_plot"), 
                                "Make plot", icon = icon("check"), class = "btn-success")  
                 )
               )
               
             ),
             
             tabPanel(
               title = tagList(icon("bar-chart"), "Plot"),
               value = "plottab",
               withSpinner(
                 dygraphOutput(ns("dygraph_current_data1"), height = "140px")
                ),
               dygraphOutput(ns("dygraph_current_data2"), height = "140px"),
               dygraphOutput(ns("dygraph_current_data3"), height = "140px"),
               dygraphOutput(ns("dygraph_current_data4"), height = "140px")
             ),
             
             tabPanel(
               title = tagList(icon("list-ol"), "Annotations"),
               value = "plotannotations",
               tags$br(),
               tags$h5("Annotations (selected time window)"),
               DTOutput(ns("dt_annotations_visible"))
             )
             
      )
      
    )
  )
  
}



visualizationModule <- function(input, output, session, 
                                data = reactive(NULL), calendar = reactive(NULL)){
  
  
  hide_tab("plottab")
  hide_tab("plotannotations")
  
  yaxis_ranges <- reactive(
    list(
      EDA = input$slide_yaxis_eda,
      HR = input$slide_yaxis_hr,
      TEMP = input$slide_yaxis_temp,
      MOVE = input$slide_yaxis_move
    )
  )
 
  observeEvent(input$btn_make_plot, {
    
    data <- data()
    
    req(data$timeseries)

    show_tab("plottab")

    if(isTRUE(nrow(calendar()))){
      show_tab("plotannotations")
    }
    
    updateTabsetPanel(session, "plottabbox", selected = "plottab")
    
    if(input$check_add_calendar_annotation){
      annotatedata <- calendar()
    } else {
      annotatedata <- NULL
    }
    
    plots <- e4_timeseries_plot(data$timeseries,
                                main_title = input$txt_plot_main_title,
                                calendar_data = annotatedata,
                                average_lines =  input$check_average_lines,
                                yaxis_ranges =  yaxis_ranges()
    )
    
    output$dygraph_current_data1 <- renderDygraph(plots[[1]])
    output$dygraph_current_data2 <- renderDygraph(plots[[2]])
    output$dygraph_current_data3 <- renderDygraph(plots[[3]])
    output$dygraph_current_data4 <- renderDygraph(plots[[4]])
    
  })
  
  current_visible_annotations <- reactive({
    
    # !!!! WATCH THE TIMEZONE!
    # Problem: cannot read timezone info from rv$calendar$Start, should
    # be saved there (as tzone attribute) when reading calendar
    ran <- suppressWarnings({
      lubridate::ymd_hms(input$dygraph_current_data4_date_window, tz = "CET")
    })
    
    calendar() %>% 
      filter(
        !((Start > ran[[2]] && End > ran[[2]]) |
               (Start < ran[[1]] && End < ran[[1]]))
      )
    
  })
  
  
  observeEvent(input$btn_panel_float, {
    
    shinyjs::show("thispanel")
    
  })
  
  output$dt_panel_annotations <- DT::renderDT({
    
    current_visible_annotations() %>%
      mutate(Date = format(Date, "%Y-%m-%d"),
             Start = format(Start, "%H:%M:%S"),
             End = format(End, "%H:%M:%S")
      ) %>%
      datatable(width = 500)
    
  })
  
  output$dt_annotations_visible <- DT::renderDT({
    
    current_visible_annotations() %>%
      mutate(Date = format(Date, "%Y-%m-%d"),
             Start = format(Start, "%H:%M:%S"),
             End = format(End, "%H:%M:%S")
      ) %>%
      datatable(width = 500)
    
  })
  
  
  
  
}





if(FALSE){
  
  
  library(shiny)
  
  ui <- fluidPage(
    
    dataUploadUI("test"),
    tags$hr(),
    calendarUI("cal"),
    tags$hr(),
    visualizationModuleUI("viz")
  )
  
  server <- function(input, output, session) {
    
    cal <- callModule(calendarModule, "cal")
    data <- callModule(dataUploadModule, "test")
    
    callModule(visualizationModule, "viz", data = data, calendar = cal)
  }
  
  shinyApp(ui, server)
  
}
