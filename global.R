


source("preload/load_packages.R")

if(packageVersion("e4tools") < "0.6"){
  stop("Please update e4tools!")
}


# Allow larger file uploads.
options(shiny.maxRequestSize = 30*1024^2)


# !! alleen voor dev
#load_all("../e4tools/")

#- Configuration
.cc <- yaml::read_yaml("config/config.yml")
.help <- yaml::read_yaml("config/help.yml")

# - Load functions
source("R/functions.R")


#- Load modules
source("modules/analysisModule.R")
source("modules/calendarModule.R")
source("modules/datauploadModule.R")
source("modules/reportModule.R")
source("modules/visualizationModule.R")
source("modules/visSeriesOptionsModule.R")
source("modules/batchModule.R")
source("modules/helpButton.R")


