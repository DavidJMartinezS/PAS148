# install.packages("shinyjs")
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyEffects)
library(shinyWidgets)
library(shinyalert)
library(shinybusy)
library(shinyjs)
library(bslib)
library(bsplus)
library(dplyr)
library(tibble)
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
library(dipsaus)
library(zip)

options(shiny.maxRequestSize=150*1024^2)

source("./Modules/leer_sf.R")
source("./functions/Accordion_info.R")
source("./functions/st_order.R")
source("./functions/functions.R")
source("./functions/check_carto.R")

# comunas <- read_sf("n:/Dashboard PAS 150/COMUNAS/COMUNAS_v1.shp")

css <- HTML(
  "/* move logo to center */
    #logo {
        position: absolute;
        left: 50%;
        top: 50%;
        transform: translate(-50%, -50%);
    }
    /* remove hover effect */
    #logo > a:hover {
        background-color: transparent !important;
        color: transparent !important;
    }"
)