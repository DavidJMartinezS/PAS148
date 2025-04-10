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
    arrange(as.numeric(N_Predio), N_r) %>% 
    rowid_to_column("N_Area") %>% 
    ungroup() %>% 
    mutate(
      Tipo_Bos = "BN",
      N_a = str_c(N_Predio, str_pad(N_r, str_length(max(N_r)), pad = "0"), sep = ".")
    ) %>% 
    select(!!!group_list, Tipo_For, Tipo_Bos, N_Rodal, N_a, N_Area, N_Pred_ori, Clase_uso, Sup_ha, Sup_m2) %>% 
  
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

cart_rodales <- function(rodales, PAS, TipoFor_num = NULL, dec_sup = 2){
  stopifnot(c("Nom_Predio", "N_Area") %in% names(areas) %>% all())
  stopifnot(PAS %in% c(148, 151))
  
  if(is.null(TipoFor_num)){
    if(rodales$Tipo_For %>% as.character() %>% str_detect("\\d") %>% table() %>% proportions() %>% subset(names(.) == TRUE) %>% unname() %>% .[] > 0.5){
      TipoFor_num <- T
    }
  }
  rodales %>%
    {if(!TipoFor_num){
      .[] %>% 
        mutate_if(
          names(.) == "Tipo_For",
          list(Tipo_For = ~case_when(
            .x %>% stri_detect_regex("no aplica", case_insensitive = T) ~ "No aplica",
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
            .default = .x)
          )
        )
    } else .
    } %>% 
    {if(PAS == 148){
      mutate_at(., "Tipo_For", as.integer)
    } else .} %>% 
    mutate(
      Tipo_Bos = if_else(PAS == 148, "BN", "No aplica"),
      Sup_ha = st_area(geometry) %>% set_units(ha) %>% drop_units() %>% round_half_up(dec_sup),
      Fuente = "Elaboración propia"
    ) %>%
    select(Nom_Predio, N_Rodal, Tipo_Bos, Tipo_For, Sup_ha, Fuente)
}

cart_area <- function(areas, PAS, dec_sup = 2, from_RCA = F, RCA = NULL){
  stopifnot(c("Nom_Predio", "N_Area") %in% names(areas) %>% all())
  stopifnot(PAS %in% c(148, 151))
  
  fuente <- if_else(
    !from_RCA, 
    "Elaboración propia",
    if_else(
      is.null(RCA),
      "RCA",
      paste0("RCA N°", RCA %>% str_extract("\\d+"))
    )
  )
  areas %>%
    mutate(
      Tipo_Bos = if_else(PAS == 148, "BN", "No aplica"),
      Sup_ha = st_area(geometry) %>% set_units(ha) %>% drop_units() %>% round_half_up(dec_sup),
      Fuente = fuente
    ) %>%
    select(Nom_Predio, N_Area, Tipo_Bos, Sup_ha, Fuente)
}

cart_suelos <- function(areas, dec_sup = 2, from_RCA = F, RCA = NULL){
  stopifnot(c("Nom_Predio", "Clase_Uso") %in% names(areas) %>% all())
  fuente <- if_else(
    !from_RCA, 
    "Elaboración propia",
    if_else(
      is.null(RCA),
      "RCA",
      paste0("RCA N°", RCA %>% str_extract("\\d+"))
    )
  )
  areas %>%
    mutate(
      Sup_ha = st_area(geometry) %>% set_units(ha) %>% drop_units() %>% round_half_up(dec_sup),
      Fuente = fuente
    ) %>% 
    select(Nom_Predio, Clase_Uso, Sup_ha, Fuente)
}

cart_rang_pend <- function(areas, dem, PAS, dec_sup = 2){
  stopifnot(c("Nom_Predio") %in% names(areas) %>% all())
  stopifnot(PAS %in% c(148, 151))
  
  slope_per <- dem %>% 
    st_crop(areas %>% st_buffer(50)) %>% st_as_stars() %>% 
    starsExtra::slope() %>% 
    {\(x) tan(x*pi/180)*100}()
  
  areas %>% 
    mutate(
      Pend_media = 1:nrow(areas) %>% map_dbl(~ slope_per[st_bbox(areas[.x,]), , , 1] %>% .$slope %>% mean(na.rm = T) %>% round_half_up(1)),
      Sup_ha = st_area(geometry) %>% set_units(ha) %>% drop_units() %>% round_half_up(dec_sup),
      Fuente = "Elaboración propia"
    ) %>% 
    {if(PAS == 148){
      .[] %>% 
        mutate(
          Ran_Pend = case_when(
            Pend_media >= 0 & Pend_media < 30 ~ "0% - 30%",
            Pend_media >= 30 & Pend_media < 45 ~ "30% - 45%",
            Pend_media >= 45 & Pend_media < 60 ~ "45% - 60%",
            Pend_media >= 60  ~ "60% y más"
          )
        )
    } else if(PAS == 151){
      .[] %>% 
        mutate(
          Ran_pend = case_when(
            Pend_media >= 0 & Pend_media < 10 ~ "0% - 10%",
            Pend_media >= 10 & Pend_media < 30 ~ "10% - 30%",
            Pend_media >= 30 & Pend_media < 45 ~ "30% - 45%",
            Pend_media >= 45 & Pend_media < 60 ~ "45% - 60%",
            Pend_media >= 60  ~ "60% y más"
          )
        )
    }} %>% 
    select(Nom_Predio, Ran_Pend, Sup_ha)
}

cart_predios <- function(predios, dec_sup = 2){
  stopifnot(c("Nom_Predio", "Rol") %in% names(predios) %>% all())
  predios %>%
    mutate(
      Sup_ha = st_area(geometry) %>% set_units(ha) %>% drop_units() %>% round_half_up(dec_sup),
      Fuente = "Elaboración propia"
    ) %>%
    select(Nom_Predio, Rol, Sup_ha)
}

cart_hidro <- function(predios, fuente, cut = "clip", buffer = 0){
  stopifnot(c("Nom_Predio") %in% names(predios) %>% all())
  stopifnot(fuente %in% c("MOP", "BCN"))
  stopifnot(cut %in% c("clip", "buffer", "crop", "crop_by_row"))
  stopifnot("buffer must be a number" = is.numeric(buffer))
  if(cut == "clip" & (buffer > 0)){
    buffer <- 0
    warning("No buffer has been applied. cut is 'clip'. To apply the buffer you can select 'buffer', 'crop' or 'crop_by_row' in cut parameter")
  }
  
  bind_rows(
    read_sf(
      system.file("Red_hidrografica_XV_XIII.gdb", package = "dataPAS"),
      wkt_filter = st_as_text(
        predios %>% 
          st_transform(9155) %>% 
          st_buffer(buffer) %>% 
          st_bbox() %>% 
          st_as_sfc() %>% 
          st_geometry()
      )
    ),
    read_sf(
      system.file("Red_hidrografica_VI_X.gdb", package = "dataPAS"),
      wkt_filter = st_as_text(
        predios %>% 
          st_transform(9155) %>% 
          st_buffer(buffer) %>% 
          st_bbox() %>% 
          st_as_sfc() %>% 
          st_geometry()
      )
    )
  ) %>%
    st_transform(st_crs(predios)) %>% 
    {if(cut %in% c("clip", "buffer")){
      .[] %>% st_intersection(predios %>% st_buffer(buffer) %>% st_union())
    } else if(cut == "crop"){
      .[] %>% st_crop(predios %>% st_buffer(buffer))
    } else if(cut == "crop_by_row"){
      .[] %>% 
        st_intersection(
          1:nrow(predios) %>% 
            map_dfr(
              ~predios[.x, ] %>% 
                st_buffer(buffer) %>% 
                st_bbox() %>% 
                st_as_sfc() %>% 
                st_as_sf()
            ) %>% 
            st_union()
        )
    }} %>% 
    st_collection_extract("LINESTRING") %>% 
    select(strahler_n, contains(fuente)) %>% 
    rename_at(vars(contains(fuente)), ~str_extract(., ".*(?=_)")) %>%
    rename(Etiqueta = nombre) %>% 
    mutate_at("tipo", stri_trans_general, "Latin-ASCII") %>% 
    mutate(
      Tip_Dren = case_when(
        stri_detect_regex(tipo, "rio", case_insensitive = T) ~ 1,
        stri_detect_regex(tipo, "estero", case_insensitive = T) ~ 2,
        stri_detect_regex(tipo, "arroyo", case_insensitive = T) ~ 3,
        stri_detect_regex(tipo, "quebrada", case_insensitive = T) ~ 4,
        .default = 5
      ) %>% as.integer(),
      Tipo_Perma = case_when(
        stri_detect_regex(tipo, "rio", case_insensitive = T) ~ 1,
        strahler_n > 3 ~ 1,
        .default = 2
      ) %>% as.integer(),
      Fuente = str_c("Geoportal (", fuente, ")")
    ) %>% 
    my_union(predios %>% select(Nom_Predio)) %>% 
    st_collection_extract("LINESTRING") %>%
    mutate_at("Nom_Predio", replace_na, "S/I") %>% 
    select(Nom_Predio, Tip_Dren, Tipo_Perma, Fuente, Etiqueta)
}

cart_hidro_osm <- function(predios, cut = "clip", buffer = 0){
  stopifnot(c("Nom_Predio") %in% names(predios) %>% all())
  stopifnot(cut %in% c("clip", "buffer", "crop", "crop_by_row"))
  stopifnot("buffer must be a number" = is.numeric(buffer))
  if(cut == "clip" & (buffer > 0)){
    buffer <- 0
    warning("No buffer has been applied. cut is 'clip'. To apply the buffer you can select 'buffer', 'crop' or 'crop_by_row' in cut parameter")
  }
  
  bbox <- as.matrix(
    tibble(
      min = predios %>% st_buffer(buffer) %>% st_transform(4326) %>% st_bbox() %>% .[c(1,2)], 
      max = predios %>% st_buffer(buffer) %>% st_transform(4326) %>% st_bbox() %>% .[c(3,4)]
    )
  ) %>% `rownames<-`(c("x","y"))
  
  hidro_osm <- bbox %>%
    opq() %>%
    add_osm_feature(key = "waterway") %>%
    osmdata_sf()
  
  hidro_osm$osm_lines %>% 
    st_transform(st_crs(predios)) %>%
    {if(cut %in% c("clip", "buffer")){
      .[] %>% st_intersection(predios %>% st_buffer(buffer) %>% st_union())
    } else if(cut == "crop"){
      .[] %>% st_crop(predios %>% st_buffer(buffer))
    } else if(cut == "crop_by_row"){
      .[] %>% 
        st_intersection(
          1:nrow(predios) %>% 
            map_dfr(
              ~predios[.x, ] %>% 
                st_buffer(buffer) %>% 
                st_bbox() %>% 
                st_as_sfc() %>% 
                st_as_sf()
            ) %>% 
            st_union()
        )
    }} %>% 
    st_collection_extract("LINESTRING") %>% 
    mutate(
      Tip_Dren = case_when(
        name %>% stri_detect_regex("rio", case_insensitive = T) ~ 1,
        name %>% stri_detect_regex("estero", case_insensitive = T) ~ 2,
        name %>% stri_detect_regex("quebrada", case_insensitive = T) ~ 4,
        name %>% stri_detect_regex("canal", case_insensitive = T) ~ 5,
        waterway == "river" ~ 1,
        waterway == "stream" ~ 3,
        waterway == "river" ~ 1,
        .default = 5
      ) %>% as.integer(),
      Tipo_Perma = case_when(
        Tip_Dren == 1 ~ 1,
        intermittent == "no" ~ 1,
        .default = 2
      ) %>% as.integer(),
      Fuente = "Elaboración propia (OpenStreetMap)"
    ) %>% 
    my_union(predios %>% select(Nom_Predio)) %>% 
    st_collection_extract("LINESTRING") %>%
    mutate_at("Nom_Predio", replace_na, "S/I") %>% 
    rename(Etiqueta = name) %>% 
    select(Nom_Predio, Tip_Dren, Tipo_Perma, Fuente, Etiqueta, waterway) %>% 
    {if(!is.null(hidro_osm$osm_multilines)){
      .[] %>% 
        bind_rows(
          hidro_osm$osm_multilines %>% 
            st_transform(st_crs(predios)) %>%
            {if(cut %in% c("clip", "buffer")){
              .[] %>% st_intersection(predios %>% st_buffer(buffer) %>% st_union())
            } else if(cut == "crop"){
              .[] %>% st_crop(predios %>% st_buffer(buffer))
            } else if(cut == "crop_by_row"){
              .[] %>% 
                st_intersection(
                  1:nrow(predios) %>% 
                    map_dfr(
                      ~predios[.x, ] %>% 
                        st_buffer(buffer) %>% 
                        st_bbox() %>% 
                        st_as_sfc() %>% 
                        st_as_sf()
                    ) %>% 
                    st_union()
                )
            }} %>% 
            st_collection_extract("LINESTRING") %>% 
            mutate(
              Tip_Dren = case_when(
                name %>% stri_detect_regex("rio", case_insensitive = T) ~ 1,
                name %>% stri_detect_regex("estero", case_insensitive = T) ~ 2,
                name %>% stri_detect_regex("quebrada", case_insensitive = T) ~ 4,
                name %>% stri_detect_regex("canal", case_insensitive = T) ~ 5,
                waterway == "river" ~ 1,
                waterway == "stream" ~ 3,
                waterway == "river" ~ 1,
                .default = 5
              ) %>% as.integer(),
              Tipo_Perma = case_when(
                Tip_Dren == 1 ~ 1,
                Tip_Dren == 2 ~ 1,
                .default = 2
              ) %>% as.integer(),
              Fuente = "Elaboración propia (OpenStreetMap)"
            ) %>% 
            my_union(predios %>% select(Nom_Predio)) %>% 
            st_collection_extract("LINESTRING") %>%
            mutate_at("Nom_Predio", replace_na, "S/I") %>% 
            rename(Etiqueta = name) %>% 
            select(Nom_Predio, Tip_Dren, Tipo_Perma, Fuente, Etiqueta, waterway)
        )
    } else .}
}

cart_caminos <- function(predios, cut = "clip", buffer = 0){
  stopifnot(c("Nom_Predio") %in% names(predios) %>% all())
  stopifnot(cut %in% c("clip", "buffer", "crop", "crop_by_row"))
  stopifnot("buffer must be a number" = is.numeric(buffer))
  if(cut == "clip" & (buffer > 0)){
    buffer <- 0
    warning("No buffer has been applied. cut is 'clip'. To apply the buffer you can select 'buffer', 'crop' or 'crop_by_row' in cut parameter")
  }
  
  read_sf(
    system.file("Red_vial.gdb", package = "dataPAS"),
    wkt_filter = st_as_text(
      predios %>% 
        st_transform(5360) %>% 
        st_buffer(buffer) %>% 
        st_bbox() %>% 
        st_as_sfc() %>% 
        st_geometry()
    )
  ) %>%
    st_zm() %>% 
    st_transform(st_crs(predios)) %>% 
    {if(cut %in% c("clip", "buffer")){
      .[] %>% st_intersection(predios %>% st_buffer(buffer) %>% st_union())
    } else if(cut == "crop"){
      .[] %>% st_crop(predios %>% st_buffer(buffer))
    } else if(cut == "crop_by_row"){
      .[] %>% 
        st_intersection(
          1:nrow(predios) %>% 
            map_dfr(
              ~predios[.x, ] %>% 
                st_buffer(buffer) %>% 
                st_bbox() %>% 
                st_as_sfc() %>% 
                st_as_sf()
            ) %>% 
            st_union()
        )
    }} %>% 
    st_collection_extract("LINESTRING") %>% 
    mutate(
      Tipo_Cam = if_else(str_detect(CLASIFICACION,'Internacional|Nacional|Regional Principal'), 1,
                         if_else(str_detect(CLASIFICACION,'Regional Provincial|Regional Comunal'), 2,
                                 if_else(str_detect(CLASIFICACION,'Acceso'), 3, 4))) %>% as.integer(),
      Fuente = "Dirección de Vialidad, Ministerio de Obras Públicas"
    ) %>% 
    my_union(predios %>% select(Nom_Predio)) %>% 
    st_collection_extract("LINESTRING") %>%
    mutate_at("Nom_Predio", replace_na, "S/I") %>% 
    select(Nom_Predio, Tipo_Cam, Fuente)
}

cart_caminos_osm <- function(predios, cut = "clip", buffer = 0){
  stopifnot(c("Nom_Predio") %in% names(predios) %>% all())
  stopifnot(cut %in% c("clip", "buffer", "crop", "crop_by_row"))
  stopifnot("buffer must be a number" = is.numeric(buffer))
  if(cut == "clip" & (buffer > 0)){
    buffer <- 0
    warning("No buffer has been applied. cut is 'clip'. To apply the buffer you can select 'buffer', 'crop' or 'crop_by_row' in cut parameter")
  }
  
  bbox <- as.matrix(
    tibble(
      min = predios %>% st_buffer(buffer) %>% st_transform(4326) %>% st_bbox() %>% .[c(1,2)], 
      max = predios %>% st_buffer(buffer) %>% st_transform(4326) %>% st_bbox() %>% .[c(3,4)]
    )
  ) %>% `rownames<-`(c("x","y"))
  
  caminos_osm <- bbox %>%
    opq() %>%
    add_osm_feature(
      key = "highway",
      value = c("motorway", "primary","secondary", "tertiary","residential", "living_street", "unclassified","service", "footway")
    ) %>%
    osmdata_sf()
  
  caminos_osm$osm_lines %>% 
    st_transform(st_crs(predios)) %>% 
    {if(cut %in% c("clip", "buffer")){
      .[] %>% st_intersection(predios %>% st_buffer(buffer) %>% st_union())
    } else if(cut == "crop"){
      .[] %>% st_crop(predios %>% st_buffer(buffer))
    } else if(cut == "crop_by_row"){
      .[] %>% 
        st_intersection(
          1:nrow(predios) %>% 
            map_dfr(
              ~predios[.x, ] %>% 
                st_buffer(buffer) %>% 
                st_bbox() %>% 
                st_as_sfc() %>% 
                st_as_sf()
            ) %>% 
            st_union()
        )
    }} %>% 
    st_collection_extract("LINESTRING") %>% 
    mutate(
      Tipo_Cam = case_when(
        highway %in% c("primary", "motorway") ~ 1,
        highway %in% c("secondary", "tertiary", "residential") ~ 2,
        highway %in% c("unclassified", "service") ~ 3,
        .default = 4
      ),
      Fuente = "Elaboración propia (OpenStreetMap)"
    ) %>% 
    my_union(predios %>% select(Nom_Predio)) %>% 
    st_collection_extract("LINESTRING") %>%
    mutate_at("Nom_Predio", replace_na, "S/I") %>% 
    rename(Etiqueta = name) %>% 
    select(Nom_Predio, Tipo_Cam, Fuente, Etiqueta, highway)
}

cart_curv_niv <- function(predios, dem, cut = "clip", buffer = 0, step = 10){
  stopifnot(c("Nom_Predio") %in% names(predios) %>% all())
  stopifnot(cut %in% c("clip", "buffer", "crop", "crop_by_row"))
  stopifnot("buffer must be a number" = is.numeric(buffer))
  stopifnot("step must be a number greater than or equal to 10" = is.numeric(step) & (step > 10))
  if(cut == "clip" & (buffer > 0)){
    buffer <- 0
    warning("No buffer has been applied. cut is 'clip'. To apply the buffer you can select 'buffer', 'crop' or 'crop_by_row' in cut parameter")
  }
  
  st_redim <- function(x){
    dm <- st_dimensions(x)
    x %>% 
      st_set_dimensions("x", offset = dm$x$offset + (dm$x$delta * (dm$x$from - 1)), delta = dm$x$delta) %>% 
      st_set_dimensions("y", offset = dm$y$offset + (dm$y$delta * (dm$y$from - 1)), delta = dm$y$delta) %>% 
      st_set_crs(dm$y$refsys)
  }
  
  dem_predio <- dem %>% 
    .[st_bbox(predios %>% 
                # st_transform(st_crs(dem)) %>% 
                st_buffer(buffer)) %>% st_as_sfc()]  %>% 
    st_as_stars(crs = st_crs(predios)) %>% 
    st_redim() %>% 
    `names<-`("Cot_Curva")
  
  curv <- st_contour(
    dem_predio,
    contour_lines = T,
    breaks = seq(
      plyr::round_any(min(dem_predio$Cot_Curva, na.rm = T), step, ceiling),
      plyr::round_any(max(dem_predio$Cot_Curva, na.rm = T), step, floor),
      step
    )
  ) %>%
    {if(cut %in% c("clip", "buffer")){
      .[] %>% st_intersection(predios %>% st_buffer(buffer) %>% st_union())
    } else if(cut == "crop"){
      .[] %>% st_crop(predios %>% st_buffer(buffer))
    } else if(cut == "crop_by_row"){
      .[] %>% 
        st_intersection(
          1:nrow(predios) %>% 
            map_dfr(
              ~predios[.x, ] %>% 
                st_buffer(buffer) %>% 
                st_bbox() %>% 
                st_as_sfc() %>% 
                st_as_sf()
            ) %>% 
            st_union()
        )
    }} %>% 
    mutate(Fuente = "Elaboracion propia (DEM Alos Palsar 12,5 x 12,5m")
    st_collection_extract("LINESTRING") %>% 
    my_union(predios %>% select(Nom_Predio)) %>%
    st_collection_extract("LINESTRING") %>% 
    select(Nom_Predio, Cot_Curva, Fuente)
}

cart_uso_actual <- function(catastro, predios, suelos, dec_sup = 2){
  stopifnot(c("USO", "SUBUSO", "ESTRUCTURA") %in% names(catastro) %>% all())
  stopifnot(c("TEXTCAUS") %in% names(suelos))
  stopifnot(c("Nom_Predio") %in% names(predios))
  
  catastro %>%
    st_intersection(predios %>% select(Nom_Predio)) %>%
    st_collection_extract("POLYGON") %>%
    st_make_valid() %>%
    st_intersection(suelos %>% select(TEXTCAUS)) %>%
    st_collection_extract("POLYGON") %>%
    select(USO, SUBUSO, ESTRUCTURA, TEXTCAUS, Nom_Predio) %>%
    st_make_valid() %>%
    st_collection_extract("POLYGON") %>% 
    mutate(
      Uso_Actual = case_when(
        SUBUSO == "Bosque Nativo" ~ paste(SUBUSO, ESTRUCTURA),
        SUBUSO == "Bosque Mixto" ~ SUBUSO,
        SUBUSO == "Plantación" ~ paste(SUBUSO, "(Otros usos)"),
        USO == "Terrenos Agrícolas" & TEXTCAUS %in% c("I", "II", "III", "IV") ~ "Uso agrícola y/o Ganadero (I-IV)",
        USO == "Terrenos Agrícolas" & TEXTCAUS %in% c("V", "VI", "VII", "VIII") ~ "Uso agrícola y/o Ganadero (V-VIII)",
        (USO == "Terrenos Agrícolas" & (is.na(TEXTCAUS) | TEXTCAUS == "N.C.")) ~ "Uso agrícola y/o Ganadero (N.C.)",
        USO == "Áreas Desprovistas de Vegetación" ~ "Áreas sin vegetación",
        .default = paste(USO, "(Otros usos)")
      ),
      Fuente = "Elaboracion propia (A partir de las capas del Catastro de CONAF y suelos de CIREN)" 
    ) %>% 
    group_by(Nom_Predio, Uso_Actual, Fuente) %>% 
    tally() %>% ungroup() %>% 
    st_collection_extract("POLYGON") %>% 
    mutate(Sup_ha = st_area(geometry) %>% set_units(ha) %>% drop_units() %>% round_half_up(dec_sup)) %>% 
    select(Nom_Predio, Uso_Actual, Sup_ha, Fuente)
}

get_carto_digital <- function(
    PAS,
    areas,
    rodales,
    predios,
    TipoFor_num,
    from_RCA,
    RCA,
    OSM_hidro = F,
    OSM_vial = F,
    cut,
    buffer,
    step,
    dec_sup = 2
  ){

  carto_rodales <- cart_rodales(rodales, TipoFor_num, dec_sup)

  carto_area <- cart_area(areas, dec_sup)

  carto_suelos <- cart_suelos(areas, dec_sup)

  carto_predios <- cart_predios(predios, dec_sup)

  carto_hidro <- cart_hidro(red_hidro, predios)

  carto_caminos <- cart_caminos(red_hidro, predios)

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
