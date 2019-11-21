

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
                  tabBox(width = 12, title = "Visualization",
                    tabPanel(
                      title = tagList(icon("cogs"), "Settings"),
                      
                      checkboxInput("check_add_calendar_annotation", 
                                    label = "Calendar annotations",
                                    value = TRUE),
                      textInput("txt_plot_main_title", "Title")
                      
                      
                    ),
                    tabPanel(
                      title = tagList(icon("bar-chart"), "Plot"),
                      withSpinner(dygraphOutput("dygraph_current_data1", height = "140px")),
                      dygraphOutput("dygraph_current_data2", height = "140px"),
                      dygraphOutput("dygraph_current_data3", height = "140px"),
                      dygraphOutput("dygraph_current_data4", height = "140px")
                      
                    )

                  )
                )
              )

      ),
      tabItem("tabAnalysis"),
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


