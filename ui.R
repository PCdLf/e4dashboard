function(){

  # shinydashboardPlus
  # dashboardPagePlus
  
  sidebar <- dashboardSidebar(

    sidebarMenu(
      id = "sidebar",
      menuItem("Data", tabName = "tabData", icon = icon("home")),
      menuItem("Calendar", tabName = "tabCalendar", icon = icon("calendar-alt")),
      menuItem("Visualization", tabName = "tabVisualization", icon = icon("chart-bar")),
      menuItem("Analysis", tabName = "tabAnalysis", icon = icon("chart-line")),
      menuItem("Report", tabName = "tabReport", icon = icon("file-alt"))
      #actionButton("browse","browser()")
    )

  )

  body <- dashboardBody(
    shinyjs::useShinyjs(),
    includeCSS("www/custom.css"),
    
#----- Choose directory, read files ----- 
    tabItems(
      tabItem("tabData",

              fluidPage(
                fluidRow(

                  shinydashboard::box(
                    width = 6,
                    title = "Data input",
                    tags$p("Click Browse to select E4 zip files to use in the application"),
                    tags$p("The data will not be permanently stored on the server."),
                    fileInput("select_zip_files",
                              label = "Choose ZIP file(s)", 
                              multiple = TRUE, 
                              accept = ".zip",
                              buttonLabel = "Browse..."),
                    
                    uiOutput("msg_files_selected"),
                    tags$br(),
                    tags$br(),
                    
                    shinyjs::hidden(
                      actionButton("btn_read_data", "Read data", icon = icon("chart-line"),
                                   class = "btn btn-success")
                    ),
                    shinyjs::hidden(
                      actionButton("btn_reset", "Start over", icon = icon("refresh"))
                    ),
                    htmlOutput("msg_data_read")
                  )
                )

              )
      ),
#----- Calendar data -----
      tabItem("tabCalendar",
              
              fluidPage(
                fluidRow(
                  shinydashboard::box(width = 8,
                                      title = "Calendar",
                                      
                        tags$div(id = "calendar_in_block",
                          tags$p("Optionally, select an Excel spreadsheet with Calendar data."),
                          tags$p("Please consult the documentation for the format of the calendar."),
                              
                          fileInput("select_calendar_file",
                                    label = "Choose Calendar (XLS/XLSX) file", 
                                    multiple = FALSE, 
                                    accept = c(".xls",".xlsx"),
                                    buttonLabel = "Browse...")
                        ),
                        
                        tags$br(),
                        shinyjs::hidden(
                          tags$div(id = "calendar_block",
                               tags$h4("Calendar data"),
                               withSpinner(
                                 dataTableOutput("dt_calendar")
                               )           
                          )
                        )
                               
                  )
                )
              )
              
      ),
#----- Visualization -----
      tabItem("tabVisualization",

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
                                 checkboxInput("check_add_calendar_annotation", 
                                               label = "Calendar events",
                                               value = TRUE),
                                 textInput("txt_plot_main_title", "Title"),
                                 checkboxGroupInput("check_average_lines", "Add averages", 
                                                    choices = c("EDA","HR","TEMP","MOVE"))
                          ),
                          column(6,
                                 
                                 numericRangeInput("slide_yaxis_eda", "EDA Y-axis range", value = c(0,20)),
                                 numericRangeInput("slide_yaxis_hr",  "HR Y-axis range", value = c(0,20)),
                                 numericRangeInput("slide_yaxis_temp","TEMP Y-axis range", value = c(0,20)),
                                 numericRangeInput("slide_yaxis_move","MOVE Y-axis range", value = c(0,20))
                                 )
                        ),
                        fluidRow(
                          actionButton("btn_make_plot", "Make plot", icon = icon("check"))  
                        )
                      )
                      
                      
                      
                      
                    ),
                    
                    tabPanel(
                      title = tagList(icon("bar-chart"), "Plot"),
                      value = "plottab",
                      withSpinner(dygraphOutput("dygraph_current_data1", height = "140px")),
                      dygraphOutput("dygraph_current_data2", height = "140px"),
                      dygraphOutput("dygraph_current_data3", height = "140px"),
                      dygraphOutput("dygraph_current_data4", height = "140px")
                    ),
                    
                    tabPanel(
                      title = tagList(icon("list-ol"), "Annotations"),
                      value = "plotannotations",
                      actionButton("btn_panel_float", "Add floating annotation panel",
                                   class = "btn btn-large", icon = icon("plus")),
                      tags$br(),
                      tags$h5("Annotations (selected time window)"),
                      DTOutput("dt_annotations_visible")
                    )

                  ),
                  shinyjs::hidden(
                    absolutePanel(
                      id = "thispanel", # give the panel an id so we can close it easily
                      
                      # Add extra CSS.
                      style = glue("background-color: white;",
                                   "padding: 30px;",
                                   "border: 1px solid black;",
                                   "border-radius: 5px;",
                                   "z-index: 10000;"),
                      
                      tags$h5("Annotations"),
                      DT::dataTableOutput("dt_panel_annotations"),
                      actionButton("btn_close_panel", "Close", 
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

      ),
#----- Analysis -----
      tabItem("tabAnalysis",
              
            fluidPage(
              fluidRow(
                shinydashboard::box(width = 12, title = "Analysis", 

                     tags$h4("Select begin date"),
  
                     dateInput("date_analysis_start", label = "Select Date",
                               value = NULL, min = NULL, max = NULL,
                               width = 200),
                      
                     side_by_side(
                       numericInput("time_hour_start", "Hour", value = 0, width = 200),
                       numericInput("time_minute_start", "Minutes", value = 0, width = 200),
                       numericInput("time_second_start", "Seconds", value = 0, width = 200)
                     ),tags$br(),
                     
                     tags$h4("Select end date"),
                     dateInput("date_analysis_end", label = "Select Date",
                               value = NULL, min = NULL, max = NULL,
                               width = 200),
                     
                     side_by_side(
                       numericInput("time_hour_end", "Hour", value = 0, width = 200),
                       numericInput("time_minute_end", "Minutes", value = 0, width = 200),
                       numericInput("time_second_end", "Seconds", value = 0, width = 200)
                     ),tags$br(),
                                                      
                     actionButton("btn_do_analysis", "Run analysis"),
                     tags$hr(),
                     dataTableOutput("dt_analysis_output"),
                     actionButton("btn_download_analysis", "Download")
                                    
                )
              )
            )
                
              
              
      ),
      tabItem("tabReport",
              
              fluidPage(
                fluidRow(
                  shinydashboard::box(width = 8, title = "Report", 
                                      
                                      
                      tags$p("Download a report of the current analysis."),
                      downloadButton("btn_download_report", "Download Report",
                                   icon = icon("file-download"),
                                   class = "btn btn-larger btn-success")
                                      
                                      
                  )
                )
              )
              
      )
    )



  )

  header <- dashboardHeader(
    title = "Empatica dashboard"
  )

  ui <- dashboardPage(
    skin = "blue",
    header = header,
    sidebar = sidebar,
    body = body
  )

return(ui)
}


