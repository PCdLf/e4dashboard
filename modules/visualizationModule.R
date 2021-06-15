

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
                   column(5,
                          
                          textInput(ns("txt_plot_main_title"), "Title"),
                          
                          tags$label(class = "control-label", "Annotations"),
                          checkboxInput(ns("check_add_calendar_annotation"), 
                                        label = "Calendar events",
                                        value = TRUE),
                          
                          
                          tags$br(),
                          tags$hr(),
                          actionButton(ns("btn_make_plot"), 
                                       "Make plot", icon = icon("check"), 
                                       class = "btn-success btn-lg"),
                          
                          tags$br(),
                          tags$hr(),
                          helpButtonUI(ns("help"))
                          
                          # other plot options?
                            
                   ),
                   column(7,
                          
                          
                          tags$h4("EDA"),
                          visSeriesOptionsUI(ns("eda"), y_range = .cc$visualisation$eda$yrange),
                          tags$hr(),
                          
                          tags$h4("HR"),
                          visSeriesOptionsUI(ns("hr"), y_range =.cc$visualisation$hr$yrange),
                          tags$hr(),
                          
                          tags$h4("TEMP"),
                          visSeriesOptionsUI(ns("temp"), y_range = .cc$visualisation$temp$yrange),
                          tags$hr(),
                          
                          tags$h4("MOVE"),
                          visSeriesOptionsUI(ns("move"), 
                                             y_range = .cc$visualisation$move$yrange
                                             )
                          
                   )
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
  
  callModule(helpButton, "help", helptext = .help$visualization)
  
  # Call each module
  y_eda <- callModule(visSeriesOptionsModule, "eda")
  y_hr <- callModule(visSeriesOptionsModule, "hr")
  y_temp <- callModule(visSeriesOptionsModule, "temp")
  y_move <- callModule(visSeriesOptionsModule, "move", selected = "custom",
                       custom_y = .cc$visualisation$move$custom_y)
  
  # Collect submodule output in a single reactive
  series_options <- reactive(
    list(
      EDA = y_eda(),
      HR = y_hr(),
      TEMP = y_temp(),
      MOVE = y_move()
    )
  )
 
  
  plot_output <- reactiveVal()
  
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
                                series_options = series_options(),
                                config = .cc$visualisation
    )
    
    plot_output(plots)
    
    output$dygraph_current_data1 <- renderDygraph(plots[[1]])
    output$dygraph_current_data2 <- renderDygraph(plots[[2]])
    output$dygraph_current_data3 <- renderDygraph(plots[[3]])
    output$dygraph_current_data4 <- renderDygraph(plots[[4]])
    
    
    # 
    toastr_success("Plot constructed, click on the 'Plot' tab!")
    updateActionButton(session, "btn_make_plot", label = "Update plot", icon = icon("refresh"))
    enable_link("tabAnalysis")
    
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
  
  
  
return(plot_output)
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
