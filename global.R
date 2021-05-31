


source("preload/load_packages.R")

# Allow larger file uploads.
options(shiny.maxRequestSize = 30*1024^2)


# !! alleen voor dev
#load_all("../e4tools/")

#- Configuration
.cc <- yaml::read_yaml("config/config.yml")


# - Load functions
source("R/functions.R")
source("R/dygraph_functions.R")
source("R/ibi_analysis.R")


#- Load modules
source("modules/analysisModule.R")
source("modules/calendarModule.R")
source("modules/datauploadModule.R")
source("modules/reportModule.R")
source("modules/visualizationModule.R")
source("modules/visSeriesOptionsModule.R")


