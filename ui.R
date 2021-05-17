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

              dataUploadUI("data")
              
      ),
#----- Calendar data -----
      tabItem("tabCalendar",
              
              calendarUI("calendar")
              
      ),
#----- Visualization -----
      tabItem("tabVisualization",

              visualizationModuleUI("viz")

      ),
#----- Analysis -----
      tabItem("tabAnalysis",
              
              analysisModuleUI("analysis")
              
      ),
      tabItem("tabReport",
              
              reportModuleUI("report")
              
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


