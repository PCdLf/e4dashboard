

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
                          numericRangeInput(ns("slide_yaxis_hr"),  "HR Y-axis range", value = c(0,20)),
                          numericRangeInput(ns("slide_yaxis_temp"),"TEMP Y-axis range", value = c(0,20)),
                          numericRangeInput(ns("slide_yaxis_move"),"MOVE Y-axis range", value = c(0,20))
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
               actionButton(ns("btn_panel_float"), "Add floating annotation panel",
                            class = "btn btn-large", icon = icon("plus")),
               tags$br(),
               tags$h5("Annotations (selected time window)"),
               DTOutput(ns("dt_annotations_visible"))
             )
             
      ),
      shinyjs::hidden(
        absolutePanel(
          id = "thispanel", # give the panel an id so we can close it easily
          
          # Add extra CSS.
          style <- glue("background-color: white;",
                       "padding: 30px;",
                       "border: 1px solid black;",
                       "border-radius: 5px;",
                       "z-index: 10000;"),
          
          tags$h5("Annotations"),
          DT::dataTableOutput(ns("dt_panel_annotations")),
          actionButton(ns("btn_close_panel"), "Close", 
                       icon = icon("remove"),
                       class = "btn btn-danger",
                       onclick=glue("document.getElementById('thispanel').style.display = 'none';")),
          
          top = 20, 
          left = 100, 
          
          width = 600,
          height = 500,
          draggable = TRUE
        )
      )
    )
  )
  
}


