get_rod_area <- function(
    LB, 
    obras, 
    predios, 
    suelos, 
    group_by_LB = NULL, 
    sep_by_CUS = T, 
    group_by_dist = F, 
    distance_max = if(group_by_distance == F) NULL,
    cut_by_prov = F,
    provincia = NULL,
    n_rodal_ord = F,
    orden_rodal = "NS-OE"
  ){
  
  group_list <- c("N_Predio", "Nom_Predio", "Tipo_fores") %>% 
    {if (!is.null(group_by_LB)) c(., group_by_LB) %>% unique() else .} %>% 
    syms()
  
  Rodales <- LB %>% 
    {if (is.null(group_by_LB) & !("PID" %in% names(.))) rowid_to_column(., "PID") else .} %>% 
    filter(str_to_sentence(str_trim(Regulacion)) == "Bosque nativo") %>% 
    {if(n_rodal_ord) mutate(., N_Rodal = st_order(geometry)) else .} %>%
    {if(cut_by_prov) st_filter(., provincia, .predicate = st_intersects) else .} %>% 
    my_union(predios) %>% 
    st_collection_extract("POLYGON") %>%
    {if(is.null(group_by_LB)){
      .[] %>% 
        group_by(., PID, N_Predio, Nom_Predio, Tipo_fores) %>% 
        summarise(geometry = st_union(geometry)) %>% 
        ungroup() %>% 
        st_collection_extract("POLYGON")
    } else {
      .[] %>% 
        group_by(!!!group_list) %>% 
        summarise(geometry = st_union(geometry)) %>%
        ungroup() %>% 
        st_collection_extract("POLYGON") %>%
        st_cast("POLYGON") %>% 
        rowid_to_column("PID") 
    }} %>% 
    {if("N_Rodal" %in% names(.)) . else .[] %>% group_by(PID) %>% mutate(N_Rodal = as.integer(cur_group_id())) %>% ungroup()} %>% 
    mutate(
      Tipo_Bos = "BN",
      Tipo_For = case_when(
        Tipo_fores %>% stri_detect_regex("alerce", case_insensitive = T) ~ "1",
        Tipo_fores %>% stri_detect_regex("araucaria", case_insensitive = T) ~ "2",
        Tipo_fores %>% stri_detect_regex("cordillera", case_insensitive = T) ~ "3",
        Tipo_fores %>% stri_detect_regex("guaitecas", case_insensitive = T) ~ "4",
        Tipo_fores %>% stri_detect_regex("magallanes", case_insensitive = T) ~ "5",
        Tipo_fores %>% stri_detect_regex("tepa", case_insensitive = T) ~ "6",
        Tipo_fores %>% stri_detect_regex("lenga", case_insensitive = T) ~ "7",
        Tipo_fores %>% stri_detect_regex("roble.*raul", case_insensitive = T) ~ "8",
        Tipo_fores %>% stri_detect_regex("roble.*hualo", case_insensitive = T) ~ "9",
        Tipo_fores %>% stri_detect_regex("siemprev", case_insensitive = T) ~ "10",
        Tipo_fores %>% stri_detect_regex("escle", case_insensitive = T) ~ "11",
        Tipo_fores %>% stri_detect_regex("palma", case_insensitive = T) ~ "12",
        .default = Tipo_fores
      ),
      Sup_ha = st_area(geometry) %>% set_units(ha) %>% round(2) %>% drop_units()
    ) %>% 
    mutate_at("Tipo_For", as.numeric) %>% 
    mutate_at(vars(Nom_Predio), replace_na, "S/I") %>% 
    arrange(N_Rodal) %>% 
    select(N_Predio, Nom_Predio, PID, N_Rodal, Tipo_Bos, Tipo_fores, Tipo_For, Sup_ha)  
  
  BN_areas <- Rodales %>%
    st_intersection(st_union(st_combine(obras))) %>% 
    st_collection_extract("POLYGON") %>%
    st_cast("POLYGON") %>% 
    {if(cut_by_prov) .[] %>% st_intersection(st_union(st_combine(provincia))) %>% st_collection_extract("POLYGON") %>% st_cast("POLYGON") else .} %>% 
    filter(!st_area(geometry) %>% drop_units() %>% round_half_up() == 0) %>% 
    mutate(N_Pred_ori = N_Predio) %>% 
    mutate_at("N_Predio", as.character) %>%
    mutate(N_Predio = replace_na(N_Predio, "S/I")) %>%
    group_by(N_Pred_ori) %>% 
    mutate(N_Predio2 = cur_group_id()) %>% 
    mutate_at("N_Predio2", as.character) %>% 
    mutate("N_Predio2" = case_when(N_Predio == "S/I" ~ N_Predio, .default = N_Predio2)) %>% 
    select(-N_Predio) %>% 
    rename(N_Predio = N_Predio2) %>% 
    arrange(N_Predio) %>% 
    {if (sep_by_CUS) {
      .[] %>% 
        my_union(suelos %>% select(Clase_uso)) %>%
        st_collection_extract("POLYGON") %>%
        st_cast("POLYGON")
    } else {
      .[] %>% 
        st_join(suelos, join = st_intersects) %>% 
        group_by(N_Rodal, !!!group_list, N_Pred_ori, N_Predio, Nom_Predio, geometry) %>% 
        summarise(Clase_uso = str_c(unique(Clase_uso), collapse = " - ")) %>% 
        ungroup() 
    }} %>% 
    mutate(Clase_uso = replace_na(Clase_uso, "S/I")) %>% 
    {if (group_by_dist) {
      .[] %>% 
        group_by(N_Rodal, N_Predio, Clase_uso) %>% 
        mutate(group = group_by_distance(geometry, distance = distance_max)) %>% 
        group_by(N_Rodal, !!!group_list, Tipo_For, N_Pred_ori, Clase_uso, group) %>% 
        summarise(geometry = st_union(geometry)) %>% 
        st_collection_extract("POLYGON") %>% 
        ungroup()
    } else .} %>% 
    mutate(
      Sup_ha  = st_area(geometry) %>% set_units(ha) %>% round(2) %>% drop_units(),
      Sup_m2  = st_area(geometry) %>% round() %>% drop_units()
    ) %>% 
    group_by(N_Predio) %>% 
    mutate(N_r = st_order(geometry)) %>% 
    ungroup() %>% 
    mutate(
      Tipo_Bos = "BN",
      N_a = str_c(N_Predio, str_pad(N_r, str_length(max(N_r)), pad = "0"), sep = ".")
    ) %>% 
    select(!!!group_list, Tipo_For, Tipo_Bos, N_Rodal, N_a, N_Pred_ori, Clase_uso, Sup_ha, Sup_m2) %>% 
    arrange(N_a)
  
  Predios <- predios %>%
    filter(N_Predio %in% unique(BN_areas$N_Pred_ori)) %>% 
    group_by(N_Predio) %>% 
    mutate(N_Predio2 = cur_group_id()) %>% 
    ungroup() %>% 
    select(-N_Predio) %>% 
    rename(N_Predio = N_Predio2) %>% 
    select(N_Predio, Nom_Predio, Rol, Propietari)
  
  return(
    list(
      Rodales = Rodales, 
      Areas = BN_areas,
      Predios = Predios
    )
  )
}



get_carto_digital <- function(
    areas,
    rodales,
    predios,
    tipo_for,
    red_hidro,
    red_vial,
    TipoFor_num,
    OSM_hidro = F,
    OSM_vial = F,
    dec_sup = 2,
    buffer_to_lines = NULL
  ){

  cart_rodales <- function(rodales, TipoFor_num, dec_sup){
    if (TipoFor_num) {
      rodales %>%
        mutate_at("Tipo_For", as.integer) %>%
        mutate(
          Tipo_Bos = "BN",
          Sup_ha = st_area(geometry) %>% set_units(ha) %>% drop_units() %>% round_half_up(dec_sup)
        ) %>%
        select(Nom_Predio, N_Rodal, Tipo_Bos, Tipo_For, Sup_ha)
    } else {
      rodales %>%
        mutate_if(
          names(.) == "Tipo_For" & names(.) %>% is.character(),
          list(Tipo_For = ~case_when(
            .x %>% stri_detect_regex("alerce", case_insensitive = T) ~ "1",
            .x %>% stri_detect_regex("araucaria", case_insensitive = T) ~ "2",
            .x %>% stri_detect_regex("cordillera", case_insensitive = T) ~ "3",
            .x %>% stri_detect_regex("guaitecas", case_insensitive = T) ~ "4",
            .x %>% stri_detect_regex("magallanes", case_insensitive = T) ~ "5",
            .x %>% stri_detect_regex("tepa", case_insensitive = T) ~ "6",
            .x %>% stri_detect_regex("lenga", case_insensitive = T) ~ "7",
            .x %>% stri_detect_regex("roble.*raul", case_insensitive = T) ~ "8",
            .x %>% stri_detect_regex("roble.*hualo", case_insensitive = T) ~ "9",
            .x %>% stri_detect_regex("siemprev", case_insensitive = T) ~ "10",
            .x %>% stri_detect_regex("escle", case_insensitive = T) ~ "11",
            .x %>% stri_detect_regex("palma", case_insensitive = T) ~ "12",
            .default = .x
          ))
        ) %>%
        mutate_at("Tipo_For", as.integer) %>%
        mutate(
          Tipo_Bos = "BN",
          Sup_ha = st_area(geometry) %>% set_units(ha) %>% drop_units() %>% round_half_up(dec_sup)
        ) %>%
        select(Nom_Predio, N_Rodal, Tipo_Bos, Tipo_For, Sup_ha)
    }
  }
  carto_rodales <- cart_rodales(rodales, TipoFor_num, dec_sup)

  cart_area <- function(areas, dec_sup){
    areas %>%
      mutate(
        Tipo_Bos = "BN",
        Sup_ha = st_area(geometry) %>% set_units(ha) %>% drop_units() %>% round_half_up(dec_sup)
      ) %>%
      select(Nom_Predio, N_a, Tipo_Bos, Sup_ha)
  }
  carto_area <- cart_area(areas, dec_sup)

  cart_suelos <- function(areas, dec_sup){
    areas %>%
      rename_if(names(.) %>% stri_detect_regex("textcaus|clase_uso", case_insensitive = T), ~ "Clase_uso") %>% 
      mutate(Sup_ha = st_area(geometry) %>% set_units(ha) %>% drop_units() %>% round_half_up(dec_sup)) %>% 
      select(Nom_Predio, Clase_uso, Sup_ha)
  }
  carto_suelos <- cart_suelos(areas, dec_sup)

  cart_predios <- function(predios, dec_sup){
    predios %>%
      mutate(Sup_ha = st_area(geometry) %>% set_units(ha) %>% drop_units() %>% round_half_up(dec_sup)) %>%
      select(Nom_Predio, Rol, Sup_ha)
  }
  carto_predios <- cart_predios(predios, dec_sup)

  cart_hidro <- function(predios, buffer_to_lines){
    read_sf(
      system.file("Red_hidrografica.gdb", package = "dataPAS"),
      wkt_filter = st_as_text(st_geometry(predios %>% st_transform(9155) %>% st_buffer(10000) %>% st_bbox() %>% st_as_sfc()))
    ) %>%
      st_intersection(predios %>% select(Nom_Predio)) %>%
      mutate(
        Tipo_Dren = if_else(
          TIPO == 'Rio', 1,
          if_else(TIPO == 'Estero', 2,
                  if_else(TIPO == 'Arroyo', 3,
                          if_else(TIPO == 'Quebrada', 4, 5)))
        ),
        Tipo_Perma = case_when(
          TIPO == 'Rio' ~ 1,
          STRAHLER_N > 3 ~ 1,
          .default = 2
        )
      ) %>%
      filter(!Tipo_Dren == 5) %>%
      select(Nom_Predio, Tipo_Dren, Tipo_Perma)
  }
  carto_hidro <- cart_hidro(red_hidro, predios)

  cart_caminos <- function(caminos, predios, include.OSM){
    caminos.mop <- caminos %>%
      st_zm() %>%
      st_transform(st_crs(predios)) %>%
      st_intersection(predios) %>%
      st_collection_extract("LINESTRING") %>%
      st_cast("LINESTRING")

    if (include.OSM) {
      caminos.osm <- opq(bbox = st_bbox(predios %>% st_transform(4326))) %>%
        add_osm_feature(
          key = "highway",
          value = c("motorway", "primary","secondary", "tertiary","residential", "living_street", "unclassified","service", "footway")
        ) %>%
        osmdata_sf() %>%
        .$osm_lines %>%
        st_transform(st_crs(predios)) %>%
        mutate(incluido = map(name, ~ .x %in% caminos.mop$NOMBRE_CAMINO))

      caminos.int <- caminos.osm %>%
        mutate(dist = st_distance(geometry, caminos.mop %>% st_union())) %>%
        units::drop_units() %>%
        dplyr::filter(official_name %in% caminos.mop$NOMBRE_CAMINO | (incluido == T & dist < 1))


    } else {

    }
    cami.int <- cami.osm %>% mutate(dist=st_distance(geometry,caminos %>% st_union())) %>% units::drop_units() %>%
      filter(official_name %in% caminos$NOMBRE_CAMINO|(incluido==T & dist<1))
    cami.mop.2 <- caminos %>%
      mutate(Nom_Predio=predio$Nom_Predio[i],
             Tipo_Cam=if_else(str_detect(CLASIFICACION,'Internacional|Nacional|Regional Principal'),1,
                              if_else(str_detect(CLASIFICACION,'Regional Provincial|Regional Comunal'),2,
                                      if_else(str_detect(CLASIFICACION,'Acceso'),3,4)))) %>%
      dplyr::select(Nom_Predio,Tipo_Cam)
    cami.osm.2 <- cami.osm %>% dplyr::filter(!osm_id %in% cami.int$osm_id) %>%
      mutate(Nom_Predio=nombre.predio,Tipo_Cam=4) %>% dplyr::select(Nom_Predio,Tipo_Cam)
    caminos <- caminos.mop[st_buffer(predio.shp,cut.x.buffer),]
    cami.osm <- caminos.osm$osm_lines %>% st_transform(32719)
    cami.osm$incluido <- sapply(cami.osm$name, function(nombre) any(str_detect(caminos$NOMBRE_CAMINO, nombre)))
    cami.int <- cami.osm %>% mutate(dist=st_distance(geometry,caminos %>% st_union())) %>% units::drop_units() %>%
      filter(official_name %in% caminos$NOMBRE_CAMINO|(incluido==T & dist<1))
    cami.mop.2 <- caminos %>%
      mutate(Nom_Predio=nombre.predio,
             Tipo_Cam=if_else(str_detect(CLASIFICACION,'Internacional|Nacional|Regional Principal'),1,
                              if_else(str_detect(CLASIFICACION,'Regional Provincial|Regional Comunal'),2,
                                      if_else(str_detect(CLASIFICACION,'Acceso'),3,4)))) %>%
      dplyr::select(Nom_Predio,Tipo_Cam)
    cami.osm.2 <- cami.osm %>% dplyr::filter(!osm_id %in% cami.int$osm_id) %>%
      mutate(Nom_Predio=nombre.predio,Tipo_Cam=4) %>% dplyr::select(Nom_Predio,Tipo_Cam)
    if (is.null(OSM)) {
      caminos <- cami.mop.2 %>% st_intersection(st_buffer(predio,cut.x.buffer)) %>% group_by(Nom_Predio,Tipo_Cam) %>% tally() %>% dplyr::select(-n) %>% ungroup() %>% mutate_at('Tipo_Cam',as.character)
    } else {
      caminos <- bind_rows(cami.mop.2,cami.osm.2) %>% st_intersection(st_buffer(predio,cut.x.buffer)) %>% group_by(Nom_Predio,Tipo_Cam) %>% tally() %>% dplyr::select(-n) %>% ungroup() %>% mutate_at('Tipo_Cam',as.character)
    }
  }

  return(
    list(

    )
  )
}

# library(purrr)
# sapply(caminos.osm$name, function(nombre) any(str_detect(red_vial$NOMBRE_CAMINO,nombre))) %>% unname()
# sapply(caminos.osm$official_name, function(nombre) any(red_vial$NOMBRE_CAMINO == nombre)) %>% unname()
#
# caminos.osm %>%
#   mutate(incluido = map(name, ~ .x %in% red_vial$NOMBRE_CAMINO)) %>%
#   .$incluido %>% unlist()
#
# cart_pts_ref <- function(predios){
#   pts_ref <- tibble(
#     N_Predio = as.numeric(),
#     Nom_Predio = as.character(),
#     Nom_pto = as.character(),
#     Coord_X = as.numeric(),
#     Coord_Y = as.numeric()
#   )
#   for (p in 1:nrow(predios)) {
#     name <- predios$Nom_Predio[p] %>% as.character()
#     cat("Predio: ", name)
#     pto <- mapview(
#       predios[p,],
#       map.types = c("Esri.WorldImagery","OpenStreetMap")
#     ) %>%
#       editMap("predio")
#     if (is.null(pto)) return(pts_ref)
#     df <- pto$finished %>%
#       st_transform(st_crs(predios)) %>%
#       mutate(
#         N_Predio = predios$N_Predio[p],
#         Nom_Predio = name,
#         Nom_pto = paste("Acceso al predio", N_Predio),
#         Coord_X=st_coordinates(geometry)[,1],
#         Coord_Y=st_coordinates(geometry)[,2]
#       ) %>%
#       dplyr::select(N_Predio, Nom_Predio, Nom_pto, Coord_X, Coord_Y)
#     rm(pto)
#     pts_ref <- rbind(pts_ref, df)
#   }
#   return(pts_ref)
# }
# asd <- cart_pts_ref(predios)
#
# predios[201:460,] %>% split(.$N_Predio) %>% map(class)
