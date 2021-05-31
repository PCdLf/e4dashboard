function(){

  # shinydashboardPlus
  # dashboardPagePlus
  
  sidebar <- dashboardSidebar(
    sidebarMenu(
      id = "sidebar",
      menuItem("Data", tabName = "tabData", icon = icon("file-upload")),
      menuItem("Calendar", tabName = "tabCalendar", icon = icon("calendar-alt")),
      menuItem("Visualization", tabName = "tabVisualization", icon = icon("chart-bar")),
      menuItem("Analysis", tabName = "tabAnalysis", icon = icon("chart-line")),
      menuItem("Report", tabName = "tabReport", icon = icon("file-alt")),
      menuItem("Batch analysis", tabName = "tabBatch", icon = icon("list-ol"))

    )
  )

  body <- dashboardBody(
    shinyjs::useShinyjs(),
    shinytoastr::useToastr(),
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
              
      ),
      tabItem("tabBatch",
              
              batchModuleUI("batch")
              
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


