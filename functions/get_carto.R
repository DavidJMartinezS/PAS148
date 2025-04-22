# Funciones de apoyo
group_by_distance <- function(x, distance){
  dist_matrix = st_distance(x, by_element = FALSE)
  class(dist_matrix) = NULL
  connected = dist_matrix <= distance
  g = igraph::graph_from_adjacency_matrix(connected)
  return(components(g)$membership)
}
my_union <- function(a,b) {
  st_agr(a) = "constant"
  st_agr(b) = "constant"
  a %>% st_difference(st_union(st_combine(b))) %>% bind_rows(st_intersection(a,b))
}
extract2.0 <- function (x, v, fun, opt = "intersect", buffer = 7, progress = TRUE, ...){
  stopifnot(opt %in% c("intersect", "bbox", "buffer"))
  
  v = st_geometry(v)
  # x = check_2d_3d(x)
  if (progress) 
    pb = utils::txtProgressBar(min = 0, max = length(v), 
                               initial = 0, style = 3)
  result = list()
  for (i in 1:length(v)) {
    if (length(dim(x)) == 2) {
      if (opt == "intersect") {
        result[[i]] = fun(x[v[i]][[1]], ...)
      }
      if (opt == "bbox") {
        result[[i]] = fun(x[st_bbox(v[i])][[1]], ...)
      }
      if (opt == "buffer") {
        result[[i]] = fun(x[st_buffer(v[i], buffer)][[1]], ...)
      }
    }
    if (length(dim(x)) == 3) {
      if (opt == "intersect") {
        result[[i]] = apply(x[v[i]][[1]], 3, fun, ...)
      }
      if (opt == "bbox") {
        result[[i]] = apply(x[st_bbox(v[i])][[1]], 3, fun, ...)
      }
      if (opt == "buffer") {
        result[[i]] = apply(x[st_buffer(v[i], buffer)][[1]], 3, fun, ...)
      }
    }
    if (progress) 
      utils::setTxtProgressBar(pb, i)
  }
  if (progress) 
    cat("\n")
  if (length(dim(x)) == 2) {
    result = do.call(c, result)
  }
  if (length(dim(x)) == 3) {
    result = do.call(rbind, result)
  }
  result[is.nan(result)] = NA
  return(result)
}

# Funciones para resultados finales 
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
  
  stopifnot(c("Tipo_fores", "Subtipo_fo", "Tipo_veg") %in% names(LB) %>% all())
  stopifnot(c("N_Predio", "Nom_Predio") %in% names(predios) %>% all())
  stopifnot(c("Clase_Uso") %in% names(suelos) %>% all())
  
  group_list <- c("N_Predio", "Nom_Predio", "Tipo_fores") %>% 
    {if (!is.null(group_by_LB)) c(., group_by_LB) %>% unique() else .} %>% 
    syms()
  
  areas <- LB %>%
    st_intersection(st_union(st_combine(obras))) %>% 
    st_collection_extract("POLYGON") %>%
    st_cast("POLYGON") %>% 
    {if(cut_by_prov) .[] %>% st_intersection(st_union(st_combine(provincia))) %>% st_collection_extract("POLYGON") %>% st_cast("POLYGON") else .} %>% 
    filter(!st_area(geometry) %>% drop_units() %>% round_half_up() == 0)
  
  Rodales <- LB %>% 
    st_filter(areas, .predicate = st_intersects) %>% 
    {if (is.null(group_by_LB) & !("PID" %in% names(.))) rowid_to_column(., "PID") else .} %>% 
    filter(str_to_sentence(str_trim(Regulacion)) == "Bosque nativo") %>% 
    # {if(cut_by_prov) st_filter(., provincia, .predicate = st_intersects) else .} %>% 
    {if(n_rodal_ord) .[] %>% mutate(N_Rodal = st_order(geometry)) else .} %>%
    my_union(predios) %>% 
    st_collection_extract("POLYGON") %>%
    {if(is.null(group_by_LB)){
      .[] %>% 
        group_by(., PID, N_Predio, Nom_Predio, Tipo_fores, Subtipo_fo, Tipo_veg) %>% 
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
        rowid_to_column("PID") %>% 
        {if(!c("Subtipo_fo", "Tipo_veg") %in% group_list %>% all()){
          .[] %>% 
            st_join(LB %>% select(c("Subtipo_fo", "Tipo_veg")[!c("Subtipo_fo", "Tipo_veg") %in% group_list]), largest = T)
        } else .}
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
    select(N_Predio, Nom_Predio, PID, N_Rodal, Tipo_Bos, Tipo_For, Tipo_fores, Subtipo_fo, Tipo_veg, Sup_ha)  
  
  if (any(Rodales$Sup_ha < 0.5)) {
    warning(
      str_c(
        "Los siguientes rodales presentan una superficie inferior a 0,5 ha:","\n",
        Rodales[which(Rodales$Sup_ha < 0.5),]$N_Rodal %>% shQuote() %>% str_c(collapse = ", ")
      )
    )
  }
  
  BN_areas <- Rodales %>%
    # st_intersection(st_union(st_combine(obras))) %>% 
    # st_collection_extract("POLYGON") %>%
    # st_cast("POLYGON") %>% 
    # {if(cut_by_prov) .[] %>% st_intersection(st_union(st_combine(provincia))) %>% st_collection_extract("POLYGON") %>% st_cast("POLYGON") else .} %>% 
    # filter(!st_area(geometry) %>% drop_units() %>% round_half_up() == 0) %>% 
    mutate(N_Pred_ori = N_Predio) %>% 
    mutate_at("N_Predio", as.character) %>%
    mutate(N_Predio = replace_na(N_Predio, "S/I")) %>%
    group_by(N_Pred_ori) %>% 
    mutate(N_Predio2 = cur_group_id()) %>% 
    ungroup() %>% 
    mutate_at("N_Predio2", as.character) %>% 
    mutate("N_Predio2" = case_when(N_Predio == "S/I" ~ N_Predio, .default = N_Predio2)) %>% 
    select(-N_Predio) %>% 
    rename(N_Predio = N_Predio2) %>% 
    arrange(N_Predio) %>% 
    {if (sep_by_CUS) {
      .[] %>% 
        my_union(suelos %>% select(Clase_Uso)) %>%
        st_collection_extract("POLYGON") %>%
        st_cast("POLYGON") %>% 
        st_make_valid() %>% 
        st_collection_extract("POLYGON")
    } else {
      .[] %>% 
        st_join(suelos %>% select(Clase_Uso)) %>% 
        group_by(N_Rodal, !!!group_list, N_Pred_ori, N_Predio, Nom_Predio, geometry) %>% 
        summarise(Clase_Uso = str_c(unique(Clase_Uso), collapse = " - ")) %>% 
        ungroup() 
    }} %>% 
    mutate(Clase_Uso = replace_na(Clase_Uso, "S/I")) %>% 
    {if (group_by_dist) {
      .[] %>% 
        group_by(N_Rodal, N_Predio, Clase_Uso) %>% 
        mutate(group = group_by_distance(geometry, distance = distance_max)) %>% 
        group_by(N_Rodal, !!!group_list, Tipo_For, N_Pred_ori, Clase_Uso, group) %>% 
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
    select(!!!group_list, Tipo_For, Tipo_Bos, N_Rodal, N_a, N_Area, N_Pred_ori, Clase_Uso, Sup_ha, Sup_m2) 
  
  Rodales %>% 
    # group_by(N_Rodal) %>% 
    .[BN_areas,]
  
  Predios <- predios %>%
    filter(N_Predio %in% unique(BN_areas$N_Pred_ori)) %>% 
    group_by(N_Predio) %>% 
    mutate(N_Predio2 = cur_group_id()) %>% 
    ungroup() %>% 
    select(-N_Predio) %>% 
    rename(N_Predio = N_Predio2) %>% 
    arrange(N_Predio) %>% 
    select(N_Predio, Nom_Predio, Rol, Propietari)
  
  return(
    list(
      Rodales = Rodales, 
      Areas = BN_areas,
      Predios = Predios
    )
  )
}

cart_rodales <- function(rodales, PAS, TipoFor_num = T, dec_sup = 2){
  stopifnot(c("Nom_Predio", "Tipo_For") %in% names(rodales) %>% all())
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
    } else . } %>%
    {if(PAS == 148){
      mutate_at(., "Tipo_For", as.integer)
    } else . } %>% 
    mutate_at("N_Rodal", as.integer) %>% 
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

cart_rang_pend <- function(areas, dem, PAS, dec_sup = 2, opt = "intersect", buffer = 7){
  stopifnot(c("Nom_Predio") %in% names(areas) %>% all())
  stopifnot(opt %in% c("intersect", "bbox", "buffer"))
  stopifnot(PAS %in% c(148, 151))

  slope_per <- dem %>%
    `st_crs<-`(st_crs(areas)) %>%
    st_crop(areas %>% st_buffer(50)) %>%
    st_as_stars() %>%
    starsExtra::slope() %>%
    {\(x) tan(x*pi/180)*100}() %>%
    extract2.0(v = areas, mean, na.rm = T, opt = opt, buffer = buffer) %>% round_half_up(1)

  areas %>%
    mutate(
      Pend_media = slope_per,
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
    select(Nom_Predio, Pend_media, Ran_Pend, Sup_ha)
}

cart_predios <- function(predios, dec_sup = 2){
  stopifnot(c("Nom_Predio", "Rol", "Propietari") %in% names(predios) %>% all())
  predios %>%
    mutate(
      Sup_ha = st_area(geometry) %>% set_units(ha) %>% drop_units() %>% round_half_up(dec_sup),
      Fuente = "Elaboración propia"
    ) %>%
    select(Nom_Predio, Rol, Propietari, Sup_ha)
}

cart_parcelas <- function(bd_parcelas, rodales){
  stopifnot(c("Parcela", "UTM_E", "UTM_E", "N_ind") %in% names(bd_parcelas) %>% all())
  stopifnot(c("Nom_Predio", "N_Rodal") %in% names(rodales) %>% all())
  
  bd_parcelas %>% 
    mutate_at("N_ind", as.integer) %>% 
    filter(
      Habito %>% stri_trans_general("Latin-ASCII") %>% stri_detect_regex("arbol", case_insensitive = T),
      !Cob_BB %>% str_to_lower() %in% c(NA_character_, "fp", "---"),
      !N_ind %in% c(NA, 0)
    ) %>% 
    select(-matches("Nom_Predio|N_Rodal|Tipo_veg|Tipo_For|Subtipo_fo")) %>% 
    st_as_sf(coords = c("UTM_E","UTM_N"), crs = st_crs(rodales), remove = F) %>%
    st_join(rodales %>% select(Nom_Predio, N_Rodal, Tipo_For, Subtipo_fo, Tipo_veg)) %>% 
    mutate_at("N_Rodal", as.integer) %>% 
    st_drop_geometry() %>% 
    mutate_at("N_ind", as.integer) %>% 
    mutate(Nha = N_ind * 20) %>% 
    arrange(N_Rodal) %>% 
    group_by(Parcela, UTM_E, UTM_N) %>%
    arrange(N_Rodal) %>% 
    mutate(N = cur_group_id()) %>% 
    group_by(N_Rodal, N) %>% 
    mutate(N_Parc = cur_group_id()) %>% 
    ungroup() %>% 
    count(Nom_Predio, N_Rodal, N_Parc, UTM_E, UTM_N) %>% select(-n) %>% 
    mutate(Fuente = "Elaboracion propia") %>% 
    st_as_sf(coords = c("UTM_E","UTM_N"), crs = st_crs(rodales), remove = F) %>% 
    rename(Coord_X = UTM_E, Coord_Y = UTM_N) %>% 
    mutate_at(vars(starts_with("Coord")), round_half_up) %>% 
    mutate_at(vars(starts_with("Coord")), as.integer) %>% 
    arrange(N_Parc) %>% 
    select(Nom_Predio, N_Rodal, N_Parc, Coord_X, Coord_Y, Fuente)
}

cart_uso_actual <- function(catastro, predios, suelos, dec_sup = 2){
  stopifnot(c("USO", "SUBUSO", "ESTRUCTURA") %in% names(catastro) %>% all())
  stopifnot(c("TEXTCAUS") %in% names(suelos))
  stopifnot(c("Nom_Predio") %in% names(predios))
  
  catastro %>%
    st_intersection(predios %>% select(Nom_Predio)) %>%
    st_collection_extract("POLYGON") %>%
    st_make_valid() %>%
    st_collection_extract("POLYGON") %>%
    my_union(suelos %>% select(TEXTCAUS)) %>%
    st_collection_extract("POLYGON") %>%
    st_make_valid() %>%
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
    filter(st_area(geometry) %>% drop_units() %>% round_half_up() != 0) %>% 
    mutate(Sup_ha = st_area(geometry) %>% set_units(ha) %>% drop_units() %>% round_half_up(dec_sup)) %>% 
    select(Nom_Predio, Uso_Actual, Sup_ha, Fuente)
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
    {if(nrow(.) == 0){
      NULL
    } else {
      .[] %>% 
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
    }}
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
    {if(nrow(.) == 0){
      NULL
    } else {
      .[] %>% 
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
        select(Nom_Predio, Tip_Dren, Tipo_Perma, Fuente, Etiqueta, waterway)
    }} %>% 
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
    {if(nrow(.) == 0){
      NULL
    } else {
      .[] %>% 
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
    }} 
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
    {if(nrow(.) == 0){
      NULL
    } else {
      .[] %>% 
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
    }} 
}

cart_curv_niv <- function(predios, dem, cut = "clip", buffer = 0, step = 10){
  stopifnot(c("Nom_Predio") %in% names(predios) %>% all())
  stopifnot(cut %in% c("clip", "buffer", "crop", "crop_by_row"))
  stopifnot("buffer must be a number" = is.numeric(buffer))
  stopifnot("step must be a number greater than or equal to 10" = is.numeric(step) & (step >= 10))
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
    `st_crs<-`(st_crs(predios)) %>% 
    .[st_bbox(predios %>% st_buffer(buffer)) %>% st_as_sfc()]  %>% 
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
    mutate(Fuente = "Elaboracion propia (DEM Alos Palsar 12,5 x 12,5m)") %>% 
    st_collection_extract("LINESTRING") %>% 
    my_union(predios %>% select(Nom_Predio)) %>%
    st_collection_extract("LINESTRING") %>% 
    mutate_at("Nom_Predio", replace_na, "S/I") %>%     
    select(Nom_Predio, Cot_Curva, Fuente)
}

get_carto_digital <- function(
    PAS = 148,
    areas,
    rodales,
    TipoFor_num = T,
    predios,
    dem,
    add_parcelas = F,
    bd_parcelas = NULL,
    from_RCA = F,
    RCA = NULL,
    add_uso_actual = F,
    catastro = NULL, 
    suelos = NULL,
    add_caminos = F,
    add_caminos_osm = F,
    caminos_arg = list(cut = "clip", buffer = 0),
    add_hidro = F,
    fuente_hidro = NULL,
    add_hidro_osm = F,
    hidro_arg = list(cut = "clip", buffer = 0),
    add_curv_niv = F,
    curv_niv_arg = list(cut = "clip", buffer = 0),
    step = 10,
    dec_sup = 2
  ){
  stopifnot(c("Nom_Predio", "Tipo_fores", "Tipo_For", "Subtipo_fo", "Tipo_veg") %in% names(rodales) %>% all())
  stopifnot(PAS %in% c(148, 151))
  stopifnot("Sobran rodales" = nrow(rodales %>% count(N_Rodal)) == nrow(rodales %>% count(N_Rodal) %>% .[areas, ]))
  stopifnot("Sobran predios" = nrow(predios) == nrow(predios[areas, ]))
  
  comunas <- read_sf(
    system.file("Comunas.gdb", package = "dataPAS"),
    wkt_filter = st_as_text(st_geometry(st_union(st_transform(predios, 5360))))
  ) %>% 
    st_transform(st_crs(areas))

  carto_rodales <- cart_rodales(rodales, PAS, TipoFor_num, dec_sup)

  carto_area <- cart_area(areas, PAS, dec_sup, from_RCA, RCA)

  carto_suelos <- cart_suelos(areas, dec_sup, from_RCA, RCA)

  carto_ran_pend <- cart_rang_pend(areas, dem, PAS, dec_sup, opt = "intersect", buffer = 7)
  
  carto_predios <- cart_predios(predios, dec_sup)
  
  if (add_parcelas) {
    stopifnot(!is.null(bd_parcelas))
    carto_parcelas <- cart_parcelas(bd_parcelas, carto_rodales)
  }
  if (add_uso_actual) {
    stopifnot(!is.null(catastro) &  !is.null(suelos))
    carto_uso_actual <- cart_uso_actual(catastro, predios, suelos, dec_sup)
  }
  if (add_caminos) {
    stopifnot(!is.null(caminos_arg) & is.list(caminos_arg))
    stopifnot(c("cut", "buffer") %in% names(caminos_arg) %>% all())
    carto_caminos <- cart_caminos(predios, caminos_arg$cut, caminos_arg$buffer)
  }
  if (add_caminos_osm) {
    stopifnot(!is.null(caminos_arg) & is.list(caminos_arg))
    stopifnot(c("cut", "buffer") %in% names(caminos_arg) %>% all())
    carto_caminos_osm <- cart_caminos_osm(predios, caminos_arg$cut, caminos_arg$buffer)
  }
  if (add_hidro) {
    stopifnot(!is.null(hidro_arg) & is.list(hidro_arg))
    stopifnot(c("cut", "buffer") %in% names(hidro_arg) %>% all())
    carto_hidro <- cart_hidro(predios, fuente_hidro, hidro_arg$cut, hidro_arg$buffer)
  }
  if (add_hidro_osm) {
    stopifnot(!is.null(hidro_arg) & is.list(hidro_arg))
    stopifnot(c("cut", "buffer") %in% names(hidro_arg) %>% all())
    carto_hidro_osm <- cart_hidro_osm(predios, hidro_arg$cut, hidro_arg$buffer)
  }
  if (add_curv_niv) {
    stopifnot(!is.null(curv_niv_arg) & is.list(curv_niv_arg))
    stopifnot(c("cut", "buffer") %in% names(curv_niv_arg) %>% all())
    carto_curv_niv <- cart_curv_niv(predios, dem, curv_niv_arg$cut, curv_niv_arg$buffer, step)
  }
  
  tabla_predios <- predios %>% 
    mutate(Sup_ha = st_area(geometry) %>% set_units(ha) %>% drop_units() %>% round_half_up(dec_sup)) %>% 
    select(N_Predio, Nom_Predio, Rol, Propietari, Sup_ha) %>% 
    st_intersection(comunas[,4:6]) %>% 
    mutate(
      Sup_ind = st_area(geometry) %>% set_units(ha) %>% drop_units() %>% round_half_up(dec_sup),
      Sup_prop = map2_dbl(Sup_ind, Sup_ha, ~round_half_up((.x/.y)*100, dec_sup))
    ) %>% 
    filter(Sup_prop > 10) %>% 
    group_by(N_Predio, Nom_Predio, Rol, Propietari, Sup_ha) %>% 
    summarise(
      Comuna = str_c(unique(COMUNA), collapse = " - "),
      Provincia = str_c(unique(PROVINCIA), collapse = " - "),
      Región = str_c(unique(REGION), collapse = " - "),
      .groups = "drop"
    ) %>% 
    st_drop_geometry() %>% 
    mutate_at(vars(Nom_Predio, Propietari), replace_na, "S/I") %>% 
    mutate_at(vars(Rol), replace_na, "S/R") %>% 
    arrange(N_Predio)
  
  tabla_areas <- carto_area %>% select(N_Area, Nom_Predio, Sup_ha) %>% 
    st_join(comunas[,4:6], largest = T) %>% 
    rename(Región = REGION, Provincia = PROVINCIA, Comuna = COMUNA) %>% 
    st_join(predios %>% select(N_Predio), largest = T) %>% 
    st_join(carto_rodales %>% select(N_Rodal), largest = T) %>% 
    st_join(carto_ran_pend %>% select(Pend_media, Ran_Pend), join = st_equals) %>% 
    st_join(carto_suelos %>% select(Clase_Uso), join = st_equals) %>% 
    mutate(
      Tipo_Dren    = carto_hidro[st_nearest_feature(carto_area, st_geometry(carto_hidro)),]$Tip_Dren,
      Tipo_Perma   = carto_hidro[st_nearest_feature(carto_area, st_geometry(carto_hidro)),]$Tipo_Perma,
      Nombre_curso = carto_hidro[st_nearest_feature(carto_area, st_geometry(carto_hidro)),]$Etiqueta,
      Distancia    = c(1:nrow(carto_area)) %>%
        map_dbl(function(x) {
          st_distance(carto_area[x,], carto_hidro[st_nearest_feature(carto_area[x,], st_geometry(carto_hidro)),]) %>% drop_units() %>% round_half_up()
        })
    ) %>% 
    st_drop_geometry() %>% 
    mutate(
      Nombre_curso = case_when(Tipo_Dren == 4 & is.na(Nombre_curso) ~ "Quebrada sin nombre", .default = Nombre_curso),
      Tipo_Perma   = case_when(Tipo_Perma == 2 ~ "Temporal", T ~ "Permanente")
    ) %>% 
    arrange(N_Predio, N_Area)
    

  return(
    list(
      Areas = carto_area,
      Rodales = carto_rodales,
      Predios = carto_predios %>% select(-Propietari),
      Suelos = carto_suelos,
      Ran_pend = carto_ran_pend %>% select(-Pend_media),
      tabla_predios = tabla_predios,
      tabla_areas = tabla_areas,
      Parcelas = if(add_parcelas) carto_parcelas,
      Uso_actual = if(add_uso_actual) carto_uso_actual,
      Caminos = if(add_caminos) carto_caminos,
      Caminos_osm = if(add_caminos_osm) carto_caminos_osm,
      Hidrografia = if(add_hidro) carto_hidro,
      Hidrografia_osm = if(add_hidro_osm) carto_hidro_osm,
      Curvas_nivel = if(add_curv_niv) carto_curv_niv
    ) %>% 
      subset(c(rep(T, 7), add_parcelas, add_uso_actual, add_caminos, add_caminos_osm, add_hidro, add_hidro_osm, add_curv_niv))
  )
}















