library(dplyr)
library(sf)

st_order <- function(x){
  if("sf" %in% class(x)){
    x <- x %>% st_centroid()
    ord <- rep(NA, nrow(x))
    listo <- rep(F, nrow(x))
    area_last <- x %>% 
      mutate(X = st_coordinates(geometry)[,1], Y = st_coordinates(geometry)[,2]) %>% 
      arrange(desc(Y),X) %>% 
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
    return(ord)
  } else {
    x <- x %>% st_centroid()
    ord <- rep(NA, length(x))
    listo <- rep(F, length(x))
    area_last <- x %>% 
      st_coordinates() %>% 
      as.data.frame() %>% 
      arrange(desc(Y),X) %>% 
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
    return(ord)
  }
}

#### EXAMPLES ####

# predios <- read_sf("c:/Users/dmartinez/Documents/KIMAL PAS SEA/PAS 148/Anexo_11-06.C_PAS_148_Elqui/Apéndice 4. Cartografía/SHP/Limite_Predial_Elqui.KIMAL.shp") %>% st_zm() 
# st_order(predios)
# predios %>% 
#   mutate(N_Predio = st_order(geometry))
# 
# areas <- read_sf("c:/Users/dmartinez/Documents/KIMAL PAS SEA/PAS 148/Anexo_11-06.C_PAS_148_Elqui/Apéndice 4. Cartografía/SHP/Area_Elqui.KIMAL.shp") %>% st_zm() %>% arrange(N_a)
# areas %>% st_order()
# areas %>%
#   group_by(Nom_Predio) %>%
#   mutate(N = st_order(geometry))