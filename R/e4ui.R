

e4ui <- function(){

  sidebar <- dashboardSidebar(

    sidebarMenu(
      id = "sidebar",
      menuItem("Data", tabName = "tabData", icon = icon("home")),
      menuItem("Calendar", tabName = "tabCalendar", icon = icon("home")),
      menuItem("Visualization", tabName = "tabVisualization", icon = icon("home")),
      menuItem("Analysis", tabName = "tabAnalysis", icon = icon("home")),
      menuItem("Report", tabName = "tabReport", icon = icon("home"))
    )

  )

  body <- dashboardBody(
    useShinyjs(),

    tabItems(
      tabItem("tabData",

              fluidPage(
                fluidRow(

                  shinydashboard::box(
                    title = "Data input",
                    tags$p("Choose a directory containing E4 ZIP files."),
                    actionButton("btn_choose_dir", "Folder", icon = icon("folder-open")),
                    tags$br(),
                    tags$br(),
                    tags$br(),
                    shinyjs::hidden(
                      pickerInput("pick_zips", "Select ZIP files",
                                  multiple = TRUE,
                                  # see ?pickerOptions for many other options
                                  options = pickerOptions(actionsBox = TRUE), 
                                  choices = "")
                    ),
                    tags$br(),
                    shinyjs::hidden(
                      actionButton("btn_read_data", "Read data", icon = icon("chart-line"))
                    ),
                    shinyjs::hidden(
                      actionButton("btn_reset", "Start over", icon = icon("refresh"))
                    ),
                    htmlOutput("msg_data_read")
                  )
                )

              )
      ),
      tabItem("tabCalendar",
              
              fluidPage(
                fluidRow(
                  shinydashboard::box(width = 12,
                                      title = "Calendar",
                             
                        withSpinner(
                          dataTableOutput("dt_calendar")
                        )
                               
                  )
                )
              )
              
      ),
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
                      
                    )

                  )
                )
              )

      ),
      tabItem("tabAnalysis",
              
            fluidPage(
              fluidRow(
                shinydashboard::box(width = 12, title = "Visualization", 
                
                                  
                     actionButton("btn_do_analysis", "Run analysis"),
                     tags$hr(),
                     verbatimTextOutput("dt_analysis_output"),
                     actionButton("btn_download_analysis", "Download")
                                    
                )
              )
            )
                
              
              
      ),
      tabItem("tabReport")
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


