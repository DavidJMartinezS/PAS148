# install.packages("shinyjs")
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyEffects)
library(shinyWidgets)
library(shinyalert)
library(shinybusy)
library(shinyFiles)
library(shinyjs)
library(bslib)
library(bsplus)
library(dplyr)
library(tibble)
library(tidyr)
library(sf)
library(units)
library(openxlsx)
library(igraph)
library(stringr)
library(stringi)
library(stars)
library(starsExtra)
library(leaflet)
library(leaflet.extras)
library(leafem)
library(leafpm)
library(mapedit)
library(tools)
# library(dipsaus)
library(zip)

# Daniel weco
options(shiny.maxRequestSize=150*1024^2)

source("./Modules/leer_sf.R")
source("./Modules/selectdir.R")
source("./functions/Accordion_info.R")
source("./functions/st_order.R")
source("./functions/functions.R")
source("./functions/check_carto.R")
source("./functions/get_carto.R")

comunas <- read_sf("n:/Dashboard PAS 150/COMUNAS/COMUNAS_v1.shp")
red_hidro <- read_sf("n:/Dashboard PAS 150/Hidrografia_V2/Hidrografia_V2 (1).shp")
red_vial <- read_sf("n:/Dashboard PAS 150/Red_Vial_Chile_18_07_2023.gdb")