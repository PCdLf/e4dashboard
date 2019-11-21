pacman::p_load(e4tools, shiny, shinydashboard, zip, dplyr, lubridate, 
               dygraphs, shinyjs, shinyWidgets, shinycssloaders,
               readxl, DT)

source("R/e4server.R")
source("R/e4ui.R")
source("R/functions.R")
source("R/dygraph_functions.R")

if(packageVersion("e4tools") < "0.1.1")stop("Please update e4tools!")

shiny::shinyApp(e4ui(), e4server)
