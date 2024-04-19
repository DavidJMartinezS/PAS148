# install.packages("shinyjs")
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyEffects)
library(shinyWidgets)
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
library(stars)

source("./Modules/leer_sf.R")
source("./functions/Accordion_info.R")
source("./functions/st_order.R")

# comunas <- read_sf("n:/Dashboard PAS 150/COMUNAS/COMUNAS_v1.shp")

my_union <- function(a,b) {
  st_agr(a) = "constant"
  st_agr(b) = "constant"
  a %>% st_difference(st_union(b)) %>% bind_rows(st_intersection(a,b))
}

group_by_distance <- function(x, distance){
  dist_matrix = st_distance(x, by_element = FALSE)
  class(dist_matrix) = NULL
  connected = dist_matrix <= distance
  g = igraph::graph_from_adjacency_matrix(connected)
  return(components(g)$membership)
}
install.packages("stars")
install.packages("starsExtra")