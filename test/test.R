pacman::p_load(e4tools, shiny, shinydashboard, zip, dplyr, lubridate, 
               dygraphs, shinyjs, shinyWidgets, shinycssloaders,
               readxl, DT, glue, xts)


library(RHRV)
library(varian)


source("R/e4server.R")
source("R/e4ui.R")
source("R/functions.R")
source("R/dygraph_functions.R")

source("R/heartrate.R")

# Method to check if a package has at least some version number.
#if(packageVersion("e4tools") < "0.1.1")stop("Please update e4tools!")

shiny::shinyApp(e4ui(), e4server)
