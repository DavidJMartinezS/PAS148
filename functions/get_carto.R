get_rod_area <- function(
    LB, 
    obras, 
    predios, 
    suelos, 
    group_by_LB = NULL, 
    sep_by_CUS = F, 
    group_by_dist = F, 
    distance_max = if(group_by_distance == F) NULL
  ){
  
  if (is.null(group_by_LB)) {
    group_list <- NULL
    Rodales <- LB %>% 
      filter(str_to_sentence(str_trim(Regulacion)) == "Bosque nativo") %>% 
      mutate(N_Rodal = st_order(geometry)) %>%
      mutate(
        Tipo_For = case_when(
          Tipo_for %>% str_to_lower() %>% str_detect("alerce") ~ "1",
          Tipo_for %>% str_to_lower() %>% str_detect("araucaria") ~ "2",
          Tipo_for %>% str_to_lower() %>% str_detect("cordillera") ~ "3",
          Tipo_for %>% str_to_lower() %>% str_detect("guaitecas") ~ "4",
          Tipo_for %>% str_to_lower() %>% str_detect("magallanes") ~ "5",
          Tipo_for %>% str_to_lower() %>% str_detect("tepa") ~ "6",
          Tipo_for %>% str_to_lower() %>% str_detect("lenga") ~ "7",
          Tipo_for %>% str_to_lower() %>% str_detect("roble.*rauli") ~ "8",
          Tipo_for %>% str_to_lower() %>% str_detect("roble.*hualo") ~ "9",
          Tipo_for %>% str_to_lower() %>% str_detect("siemprev") ~ "10",
          Tipo_for %>% str_to_lower() %>% str_detect("^escle") ~ "11",
          Tipo_for %>% str_to_lower() %>% str_detect("^palma") ~ "12",
          .default = Tipo_for
        )
      ) %>% 
      relocate(N_Rodal)
  } else {
    group_list <- group_by_LB %>% syms()
    Rodales <- LB %>% 
      filter(str_to_sentence(str_trim(Regulacion)) == "Bosque nativo") %>% 
      group_by(!!!group_list) %>% 
      summarise(geometry = st_union(geometry)) %>% 
      ungroup() %>% 
      st_collection_extract("POLYGON") %>%
      st_cast("POLYGON") %>% 
      mutate(N_Rodal = st_order(geometry)) %>% 
      mutate(
        Tipo_For = case_when(
          Tipo_for %>% str_to_lower() %>% str_detect("alerce") ~ "1",
          Tipo_for %>% str_to_lower() %>% str_detect("araucaria") ~ "2",
          Tipo_for %>% str_to_lower() %>% str_detect("cordillera") ~ "3",
          Tipo_for %>% str_to_lower() %>% str_detect("guaitecas") ~ "4",
          Tipo_for %>% str_to_lower() %>% str_detect("magallanes") ~ "5",
          Tipo_for %>% str_to_lower() %>% str_detect("tepa") ~ "6",
          Tipo_for %>% str_to_lower() %>% str_detect("lenga") ~ "7",
          Tipo_for %>% str_to_lower() %>% str_detect("roble.*rauli") ~ "8",
          Tipo_for %>% str_to_lower() %>% str_detect("roble.*hualo") ~ "9",
          Tipo_for %>% str_to_lower() %>% str_detect("siemprev") ~ "10",
          Tipo_for %>% str_to_lower() %>% str_detect("^escle") ~ "11",
          Tipo_for %>% str_to_lower() %>% str_detect("^palma") ~ "12",
          .default = Tipo_for
        )
      ) %>% 
      relocate(N_Rodal)
  }
  
  BN_inter <- Rodales %>%
    st_intersection(st_union(obras)) %>% 
    st_collection_extract("POLYGON") %>%
    st_cast("POLYGON") %>% 
    my_union(predios) %>% 
    st_collection_extract("POLYGON") %>%
    st_cast("POLYGON") %>% 
    filter(!st_area(geometry) %>% round() %>% drop_units() == 0) %>% 
    mutate(N_Pred_ori = N_Predio) %>% 
    mutate_at("N_Predio", as.character) %>%
    mutate(N_Predio = replace_na(N_Predio, "S/I")) %>%
    group_by(N_Pred_ori) %>% 
    mutate(N_Predio2 = cur_group_id()) %>% 
    mutate_at("N_Predio2",as.character) %>% 
    mutate("N_Predio2" = case_when(N_Predio == "S/I" ~ N_Predio, .default = N_Predio2)) %>% 
    select(-N_Predio) %>% 
    rename(N_Predio = N_Predio2) %>% 
    arrange(N_Predio)
  
  if (sep_by_CUS) {
    BN_inter <- BN_inter %>% 
      my_union(suelos %>% select(Textcaus)) %>%
      st_collection_extract("POLYGON") %>%
      st_cast("POLYGON") %>% 
      mutate(Textcaus = replace_na(Textcaus, "S/I"))
  } else {
    BN_inter <- BN_inter %>% 
      st_join(suelos, join = st_intersects) %>% 
      group_by(N_Rodal, !!!group_list, N_Pred_ori, N_Predio, Nom_Predio, geometry) %>% 
      summarise(Textcaus = str_c(unique(Textcaus), collapse = " - ")) %>% 
      ungroup() %>% 
      mutate(Textcaus = replace_na(Textcaus, "S/I"))
  }
  
  if (group_by_dist) {
    BN_inter <- BN_inter %>% 
      group_by(N_Rodal, N_Predio, Textcaus) %>% 
      mutate(group = group_by_distance(geometry, distance = distance_max)) %>% 
      group_by(N_Rodal, !!!group_list, N_Pred_ori, N_Predio, Nom_Predio, Textcaus, group) %>% 
      summarise(geometry = st_union(geometry))
  } 
  
  BN_areas <- BN_inter %>% 
    mutate(
      Sup_ha  = st_area(geometry) %>% set_units(ha) %>% round(2) %>% drop_units(),
      Sup_m2  = st_area(geometry) %>% round() %>% drop_units()
    ) %>% 
    group_by(N_Predio) %>% 
    mutate(N_r = st_order(geometry)) %>% 
    ungroup() %>% 
    mutate(
      N_a = str_c(N_Predio, str_pad(N_r, str_length(max(N_r)), pad = "0"), sep = ".")
    ) %>% 
    select(N_Predio, N_Rodal, N_a, !!!group_list, Nom_Predio, Textcaus, Sup_ha, Sup_m2) %>% 
    arrange(N_a)
  
  Predios <- predios %>%
    filter(N_Predio %in% unique(BN_inter$N_Pred_ori)) %>% 
    group_by(N_Predio) %>% 
    mutate(N_Predio2 = cur_group_id()) %>% 
    ungroup() %>% 
    select(-N_Predio) %>% 
    rename(N_Predio = N_Predio2) %>% 
    select(N_Predio, Nom_Predio, Rol, Prop)
  
  return(
    list(
      Rodales = Rodales, 
      Areas = BN_areas,
      Predios = Predios
    )
  )
}

# Example #
# LB <- read_sf("c:/Users/dmartinez/Documents/datos_temp/example_files/PAS148/ATL750-Segmentacion_LB.shp")
# obras <- read_sf("c:/Users/dmartinez/Documents/datos_temp/example_files/PAS148/ATL750-Obras.shp")
# predios <- read_sf("c:/Users/dmartinez/Documents/datos_temp/example_files/PAS148/ATL750-Predios.shp")
# suelos <- read_sf("c:/Users/dmartinez/Documents/datos_temp/example_files/PAS148/ATL750-CIREN_Suelos.shp")
# 
rod_areas <- get_rod_area(
    LB,
    obras,
    predios,
    suelos,
    group_by_LB = c("Tipo_for","Subtipo_fo"),
    sep_by_CUS = T,
    group_by_dist = T,
    distance_max = 50
)

cart_area <- function(areas){
  areas %>% 
    mutate(Tipo_Bos = "BN") %>% 
    select(Nom_Predio, N_a, Tipo_Bos, Sup_ha)
}
cart_area(BN_areas)

cart_suelos <- function(areas){
  areas %>% 
    rename(Clase_uso = Textcaus) %>% # Revisar nombre del campo
    select(Nom_Predio, Clase_uso, Sup_ha)
}
cart_suelos(BN_areas)

cart_predios <- function(predios){
  predios %>% 
    mutate(Sup_ha = st_area(geometry) %>% set_units(ha) %>% round(2) %>% drop_units()) %>% 
    select(Nom_Predio, Rol, Sup_ha)
}
cart_predios(predios)

cart_pts_ref <- function(predios){
  pts_ref <- tibble(
    N_Predio = as.numeric(),
    Nom_Predio = as.character(),
    Nom_pto = as.character(),
    Coord_X = as.numeric(),
    Coord_Y = as.numeric()
  )
  for (p in 1:nrow(predios)) {
    name <- predios$Nom_Predio[p] %>% as.character()
    cat("Predio: ", name)
    pto <- mapview(
      predios[p,],
      map.types = c("Esri.WorldImagery","OpenStreetMap")
    ) %>%
      editMap("predio")
    if (is.null(pto)) return(pts_ref)
    df <- pto$finished %>% 
      st_transform(st_crs(predios)) %>% 
      mutate(
        N_Predio = predios$N_Predio[p],
        Nom_Predio = name,
        Nom_pto = paste("Acceso al predio", N_Predio),
        Coord_X=st_coordinates(geometry)[,1],
        Coord_Y=st_coordinates(geometry)[,2]
      ) %>% 
      dplyr::select(N_Predio, Nom_Predio, Nom_pto, Coord_X, Coord_Y)
    rm(pto)
    pts_ref <- rbind(pts_ref, df)
  }
  return(pts_ref)
}
asd <- cart_pts_ref(predios)

predios[201:460,] %>% split(.$N_Predio) %>% map(class)
