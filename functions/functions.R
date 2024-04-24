add_help_text <- function(x, text,...){
  x %>% shinyInput_label_embed(
    shiny_iconlink() %>%
      bs_embed_popover(
        content = text,..., placement = "left"
      )
  )
}


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