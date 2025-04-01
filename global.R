library(bslib)
library(bsplus)
library(dataPAS)
library(dplyr)
library(igraph)
library(janitor)
library(leaflet)
library(leaflet.extras)
library(leafem)
library(leafpm)
library(mapedit)
library(openxlsx2)
library(purrr)
library(sf)
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyEffects)
library(shinyWidgets)
library(shinyalert)
library(shinybusy)
library(shinyFiles)
library(shinyvalidate)
library(shinyjs)
library(stars)
library(starsExtra)
library(stringr)
library(stringi)
library(tibble)
library(tidyr)
library(tools)
library(units)
library(writexl)
library(zip)

options(shiny.maxRequestSize = 1000 * 1024 ^ 2, timeout = 100)

source("./Modules/leer_sf.R")
source("./Modules/downfiles.R")
source("./Modules/RCA.R")
source("./functions/Accordion_info.R")
source("./functions/st_order.R")
source("./functions/functions.R")
source("./functions/check_carto.R")
source("./functions/get_carto.R")

provincias_list <- read_sf(system.file("Comunas.gdb", package = "dataPAS")) %>% 
  st_drop_geometry() %>% 
  group_by(CUT_REG, REGION, PROVINCIA) %>% 
  tally() %>% ungroup() %>% 
  mutate_at("CUT_REG", ~factor(., levels = c("15", "01", "02", "03", "04", "05", "13", "06", "07", "16", "08","09","14","10","11","12"))) %>% 
  group_by(CUT_REG, REGION) %>% 
  summarize(PROVINCIA = list(PROVINCIA)) %>% 
  mutate(PROVINCIA = setNames(PROVINCIA, REGION)) %>% 
  arrange(CUT_REG) %>% 
  pull(PROVINCIA)
  
