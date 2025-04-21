library(dplyr)
library(sf)

# Priorizar poligonos al nortes para evitar poligonos que queden rezagados. Se podria realizar alguna lista de poligonos que estan a cierto radio del poligono a evaluar, que se vayan agregando y quitando o actualizando a medida que avanza el ciclo while. Se podria realizar una condicion de que si la distancia del poligono rezagado es mayor y esta más al norte entonces que continue en el ciclo 

st_order <- function(x, order = "NS-OE"){
  order_selected <- list(
    "NS-EO" = c(expr(desc(Y)), expr(desc(X))),
    "NS-OE" = c(expr(desc(Y)), expr(X)),
    "SN-EO" = c(expr(Y), expr(desc(X))),
    "SN-OE" = c(expr(Y), expr(X)),
    "EO-NS" = c(expr(desc(X)),expr(desc(Y))),
    "EO-SN" = c(expr(desc(X)),expr(Y)),
    "OE-NS" = c(expr(X),expr(desc(Y))),
    "OE-SN" = c(expr(X),expr(Y))
  )[order] %>% unlist()
  
  if("sf" %in% class(x)){
    x <- x %>% st_centroid()
    ord <- rep(NA, nrow(x))
    listo <- rep(F, nrow(x))
    area_last <- x %>% 
      mutate(X = st_coordinates(geometry)[,1], Y = st_coordinates(geometry)[,2]) %>% 
      arrange(!!!order_selected) %>% 
      slice(1)
    i = 1
    ord[which(st_geometry(x) == st_geometry(area_last))] <- i
    listo[which(st_geometry(x) == st_geometry(area_last))] <- T
    while (any(is.na(ord))) {
      n <- st_nearest_feature(area_last, x[!listo,])
      area_last <- x[!listo,][n,]
      i <- i + 1
      ord[which(st_geometry(x) == st_geometry(area_last))] <- i
      listo[which(st_geometry(x) == st_geometry(area_last))] <- T
    }
    return(as.integer(ord))
  } else {
    x <- x %>% st_centroid()
    ord <- rep(NA, length(x))
    listo <- rep(F, length(x))
    area_last <- x %>% 
      st_coordinates() %>% 
      as.data.frame() %>% 
      arrange(!!!order_selected) %>% 
      slice(1) %>% 
      unlist() %>% 
      st_point() %>% 
      st_sfc(crs = st_crs(x))
    i = 1
    ord[which(st_geometry(x) == st_geometry(area_last))] <- i
    listo[which(st_geometry(x) == st_geometry(area_last))] <- T
    while (any(is.na(ord))) {
      n <- st_nearest_feature(area_last, x[!listo])
      area_last <- x[!listo][n]
      i <- i + 1
      ord[which(st_geometry(x) == st_geometry(area_last))] <- i
      listo[which(st_geometry(x) == st_geometry(area_last))] <- T
    }
    return(as.integer(ord))
  }
}

#### EXAMPLES ####

# predios <- read_sf("c:/Users/dmartinez/Documents/KIMAL PAS SEA/PAS 148/Anexo_11-06.C_PAS_148_Elqui/Apéndice 4. Cartografía/SHP/Limite_Predial_Elqui.KIMAL.shp") %>% st_zm()
# st_order(predios,order = "NS-EO")
# predios %>%
#   mutate(N_Predio = st_order(geometry))
# 
# areas <- read_sf("c:/Users/dmartinez/Documents/KIMAL PAS SEA/PAS 148/Anexo_11-06.C_PAS_148_Elqui/Apéndice 4. Cartografía/SHP/Area_Elqui.KIMAL.shp") %>% st_zm() %>% arrange(N_a)
# areas %>% st_order()
# areas %>%
#   group_by(Nom_Predio) %>%
#   mutate(N = st_order(geometry))
