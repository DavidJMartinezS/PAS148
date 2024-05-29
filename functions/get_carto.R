get_rod_area <- function(
    LB, 
    obras, 
    predios, 
    suelos, 
    group_by_LB = NULL, 
    sep_by_CUS = F, 
    group_by_distance = F, 
    distance_max = if(group_by_distance == F) NULL
  ){
  
  if (is.null(group_by_LB)) {
    group_list <- NULL
    rodales <- LB %>% 
      filter(str_to_sentence(str_trim(Regulacion)) == "Bosque nativo") %>% 
      mutate(
        N_Rodal = st_order(geometry),
        Tipo_For = case_when(
          Tipos_Fore %>% str_to_lower() %>% str_detect("alerce") ~ "1",
          Tipos_Fore %>% str_to_lower() %>% str_detect("araucaria") ~ "2",
          Tipos_Fore %>% str_to_lower() %>% str_detect("cordillera") ~ "3",
          Tipos_Fore %>% str_to_lower() %>% str_detect("guaitecas") ~ "4",
          Tipos_Fore %>% str_to_lower() %>% str_detect("magallanes") ~ "5",
          Tipos_Fore %>% str_to_lower() %>% str_detect("tepa") ~ "6",
          Tipos_Fore %>% str_to_lower() %>% str_detect("lenga") ~ "7",
          Tipos_Fore %>% str_to_lower() %>% str_detect("roble.*rauli") ~ "8",
          Tipos_Fore %>% str_to_lower() %>% str_detect("roble.*hualo") ~ "9",
          Tipos_Fore %>% str_to_lower() %>% str_detect("siemprev") ~ "10",
          Tipos_Fore %>% str_to_lower() %>% str_detect("^escle") ~ "11",
          Tipos_Fore %>% str_to_lower() %>% str_detect("^palma") ~ "12",
          .default = Tipos_Fore
        )
      ) %>% 
      relocate(N_Rodal)
  } else {
    group_list <- group_by_LB %>% sym()
    rodales <- LB %>% 
      filter(str_to_sentence(str_trim(Regulacion)) == "Bosque nativo") %>% 
      group_by(!!group_list) %>% 
      summarise(geometry = st_union(geometry)) %>% 
      st_collection_extract("POLYGON") %>%
      st_cast("POLYGON") %>% 
      mutate(
        N_Rodal = st_order(geometry),
        Tipo_For = case_when(
          Tipos_Fore %>% str_to_lower() %>% str_detect("alerce") ~ "1",
          Tipos_Fore %>% str_to_lower() %>% str_detect("araucaria") ~ "2",
          Tipos_Fore %>% str_to_lower() %>% str_detect("cordillera") ~ "3",
          Tipos_Fore %>% str_to_lower() %>% str_detect("guaitecas") ~ "4",
          Tipos_Fore %>% str_to_lower() %>% str_detect("magallanes") ~ "5",
          Tipos_Fore %>% str_to_lower() %>% str_detect("tepa") ~ "6",
          Tipos_Fore %>% str_to_lower() %>% str_detect("lenga") ~ "7",
          Tipos_Fore %>% str_to_lower() %>% str_detect("roble.*rauli") ~ "8",
          Tipos_Fore %>% str_to_lower() %>% str_detect("roble.*hualo") ~ "9",
          Tipos_Fore %>% str_to_lower() %>% str_detect("siemprev") ~ "10",
          Tipos_Fore %>% str_to_lower() %>% str_detect("^escle") ~ "11",
          Tipos_Fore %>% str_to_lower() %>% str_detect("^palma") ~ "12",
          .default = Tipos_Fore
        )
      ) %>% 
      relocate(N_Rodal)
  }
  
  BN_inter <- rodales %>%
    st_intersection(st_union(obras)) %>% 
    st_collection_extract("POLYGON") %>%
    st_cast("POLYGON") %>% 
    my_union(predios) %>% 
    st_collection_extract("POLYGON") %>%
    st_cast("POLYGON") %>% 
    mutate_at("N_Predio",as.character) %>% 
    mutate(N_Predio = replace_na(N_Predio, "S/I")) %>% 
    group_by(N_Predio) %>% 
    mutate(N_Predio2 = cur_group_id()) %>% 
    mutate_at("N_Predio2",as.character) %>% 
    mutate("N_Predio2" = case_when(N_Predio == "S/I" ~ N_Predio, .default = N_Predio2)) %>% 
    select(-N_Predio) %>% 
    rename(N_Predio = N_Predio2)
  
  if (sep_by_CUS) {
    BN_inter <- BN_inter %>% 
      my_union(suelos %>% select(textcaus)) %>%
      st_collection_extract("POLYGON") %>%
      st_cast("POLYGON") 
  } else {
    BN_inter <- BN_inter %>% 
      st_join(suelos, join = st_intersects) %>% 
      group_by(N_Rodal, !!group_list, N_Predio, Nom_Predio, geometry) %>% 
      summarise(textcaus = str_c(unique(textcaus), collapse = " - ")) %>% 
      ungroup()
  }
  
  if (group_by_distance) {
    BN_inter <- BN_inter %>% 
      group_by(N_Rodal, N_Predio, textcaus) %>% 
      mutate(group = group_by_distance(geometry, distance = distance_max)) %>% 
      group_by(N_Rodal, !!group_list, N_Predio, Nom_Predio, textcaus, group) %>% 
      summarise(geometry = st_union(geometry))
  } 
  BN_areas <- BN_inter %>% 
    mutate(
      Sup_ha  = st_area(geometry) %>% set_units(ha) %>% round(2),
      Sup_m2  = st_area(geometry) %>% round()
    ) %>% 
    group_by(N_Predio2) %>% 
    mutate(N_r = st_order(geometry)) %>% 
    ungroup() %>% 
    mutate(N_a = str_c(N_Predio2, str_pad(N_r, str_length(max(N_r)), pad = "0"), sep = ".")) %>% 
    select(N_Predio, N_Rodal, N_a, !!group_list, Nom_Predio, textcaus)
  
  return(
    list(
      Rodales = rodales, 
      Areas = BN_areas
    )
  )
}



cart_area <- function(areas){
  areas %>% 
    mutate(Tipo_Bos = "BN") %>% 
    select(Nom_Predio, N_a, Tipo_Bos, Sup_ha)
}

cart_suelos <- function(areas){
  areas %>% 
    rename(Clase_uso = textcaus) %>% # Revisar nombre del campo
    select(Nom_Predio, Clase_uso, Sup_ha)
}

cart_predios <- function(predios){
  predios %>% 
    mutate(Sup_ha = st_area(geometry) %>% set_units(ha) %>% round(2) %>% drop_units()) %>% 
    select(Nom_Predio, Rol, Sup_ha)
}

cart_pts_ref <- function(predios){
  pts_ref <- data.frame(
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




