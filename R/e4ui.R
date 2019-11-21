

e4ui <- function(){

  sidebar <- dashboardSidebar(

    sidebarMenu(
      id = "sidebar",
      menuItem("Data", tabName = "tabData", icon = icon("home")),
      menuItem("Calendar", tabName = "tabCalendar", icon = icon("home")),
      menuItem("Visualization", tabName = "tabVisualization", icon = icon("home")),
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
                    shinyjs::hidden(
                      pickerInput("pick_zips", "Selecteer bestand(en)",
                                  multiple = TRUE,
                                  choices = "")
                    ),

                    shinyjs::hidden(
                      actionButton("btn_read_data", "Go!", icon = icon("chart-line"))
                    )
                  )
                )

              )
      ),
      tabItem("tabCalendar"),
      tabItem("tabVisualization",

              fluidPage(
                fluidRow(
                  shinydashboard::box(width = 12,

                                      dygraphOutput("dygraph_current_data1", height = "150px"),
                                      dygraphOutput("dygraph_current_data2", height = "150px"),
                                      dygraphOutput("dygraph_current_data3", height = "150px"),
                                      dygraphOutput("dygraph_current_data4", height = "150px")
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


