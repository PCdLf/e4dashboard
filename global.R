


source("preload/load_packages.R")

# !! alleen voor dev
#load_all("../e4tools/")



source("R/functions.R")
source("R/dygraph_functions.R")

source("R/heartrate.R")


source("modules/analysisModule.R")
source("modules/calendarModule.R")
source("modules/datauploadModule.R")
source("modules/reportModule.R")
source("modules/visualizationModule.R")
source("modules/visSeriesOptionsModule.R")


options(shiny.maxRequestSize = 30*1024^2)
