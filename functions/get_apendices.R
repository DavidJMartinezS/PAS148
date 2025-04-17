# funciones de apooyo
cup_coverage <- function(x, y, method = "circle", Na.value = 0){
  stopifnot(method %in% c("circle", "ellipse"))
  
  if (method == "circle") {
    val <- pi*(mean(c(x,y), na.rm = T)/2)^2
  } 
  if (method == "ellipse"){
    val <- pi * x/2 * y/2
  }
  if (is.na(val)) {
    return(Na.value)
  } else {
    return(val)
  }
}

plot_coverage <- function(x, plot_size = 500, percent = F, digits = 1, ...){
  x <- sum(x, na.rm = T)/plot_size
  if (percent) {
    paste0(formatC(x * 100, format = "f", digits = digits, ...), "%")
  } else {
    formatC(x, digits = digits, format = "f", ...)
  }
}

italic_sp <- function(x){
  tv <- str_split(x, " de ")
  el1 <- lapply(tv, function(a) {return(a[1])})
  el2 <- lapply(tv, function(a) {
    if (length(a) > 1) {
      return(str_c("*",a[2],"*"))
    } else {
      return(NA) # O cualquier otro valor que desees para los elementos que no tienen un segundo elemento
    }
  })
  
  map2_chr(el1, el2, function(x, y){
    if (is.na(y)) {
      return(x)
    } else {
      return(stri_c(x, " de ", y) )
    }
  })
}

nha_x_sp_fun <- function(parcelas, bd){
  bd %>% 
    select(N_Parc, Especie, Nha) %>% 
    filter(N_Parc %in% parcelas) %>% 
    complete(N_Parc, Especie, fill = list(Nha = 0)) %>% 
    group_by(Especie) %>% 
    summarise(Nha = mean(Nha,na.rm = T))
}

# Funciones Apénices
apendice_2_3 <- function(bd_flora, bd_pcob = NULL, rodales, predios, portada = "default", provincia, huso = NULL){
  if (is.null(huso)) {
    huso <- if_else(
      st_crs(rodales)[[2]] %>% str_split_1("\n") %>% .[length(.)] %>% str_extract("\\d+") == -32719,
      "19S", 
      "18S"
    )
  }
  bd_flora <- bd_flora %>% 
    rename_if(names(.) %>% stri_trans_general("Latin-ASCII") %>% stri_cmp_equiv("campana", strength = 1), ~ "Campana") 
  stopifnot(all(c('Parcela', 'UTM_E', 'UTM_N', 'Especie', 'N_ind', 'Habito', 'Cob_BB') %in% names(bd_flora)))
  if(!is.null(bd_pcob)){
    stopifnot(all(c('Parcela', 'Especie', 'Copa_NS', 'Copa_EO') %in% names(bd_pcob)))
  }
  stopifnot(all(c('Nom_Predio', 'N_Rodal') %in% names(rodales)))
  stopifnot(all(c('N_Predio') %in% names(predios)))
  
  parcelas_rodales <- bd_flora %>% 
    filter(
      Habito %>% stri_trans_general("Latin-ASCII") %>% stri_detect_regex("arbol", case_insensitive = T),
      !Cob_BB %>% str_to_lower() %in% c(NA_character_, "fp")
    ) %>% 
    st_as_sf(coords = c("UTM_E","UTM_N"), crs = st_crs(rodales), remove = F) %>% 
    st_intersection(st_union(rodales)) %>%
    st_join(rodales %>% select(Nom_Predio, N_Rodal)) %>% 
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
    ungroup() 
  
  parcelas <- parcelas_rodales %>%
    count(Nom_Predio, N_Rodal, Tipo_veg, N_Parc, UTM_E, UTM_N) %>% select(-n) %>%
    mutate(Fuente = "Elaboracion propia") %>%
    st_as_sf(coords = c("UTM_E","UTM_N"), crs = st_crs(rodales), remove = F) %>% 
    rename(Coord_X = UTM_E, Coord_Y = UTM_N) %>%
    mutate_at(vars(starts_with("Coord")), round_half_up) %>%
    arrange(N_Parc) %>%
    select(Nom_Predio, N_Rodal, Tipo_veg, N_Parc, Coord_X, Coord_Y, Fuente)
  
  if (!is.null(bd_pcob)) {
    df_cob <- bd_pcob %>% 
      select(-starts_with("UTM")) %>% 
      inner_join(
        parcelas_rodales %>% count(Campana, Parcela, N_Parc, UTM_E, UTM_N) %>% select(-n)
      ) %>% 
      mutate(
        Cob_arb = map2_dbl(Copa_NS, Copa_EO, cup_coverage, method = "ellipse")
      ) %>% 
      group_by(N_Parc, UTM_E, UTM_N, Especie) %>% 
      summarise(
        Cob_arb = sum(Cob_arb, na.rm = T),
        Nha = n() * 20
      ) %>% 
      group_by(N_Parc) %>% 
      mutate(
        Cob_parc_p = plot_coverage(Cob_arb, percent = T, digits = 1, decimal.mark = ","),
        Cob_parc = map_dbl(Cob_parc_p, ~as.numeric(str_extract(str_replace(., ",", "\\."),"\\d+\\.\\d"))),
        Fuente = "PCob"
      ) %>% 
      ungroup() %>% 
      mutate_at("Cob_arb", ~round_half_up(. / 500, 2)) %>% 
      select(N_Parc, UTM_E, UTM_N, Especie, Nha, Cob_arb, Cob_parc, Fuente) %>% 
      {if(nrow(.[]) == 0) NULL else .}
  }
  
  df_flora <- parcelas_rodales %>% 
    {if(count(.[],N_Parc, Especie, sort = T) %>% filter(n > 1) %>% nrow() >= 1){
      .[] %>% group_by(N_Parc, UTM_E, UTM_N, Especie) %>% slice_sample() %>% ungroup()
    } else .[]} %>% 
    mutate(
      Cob_arb = case_when(
        Cob_BB %in% c("r", "+", "1") ~ 2.5,
        Cob_BB == "2" ~ 15,
        Cob_BB == "3" ~ 37.5,
        Cob_BB == "4" ~ 62.5,
        Cob_BB == "5" ~ 87.5,
        .default = 0
      ),
      Fuente = "Flora"
    ) %>% 
    group_by(N_Parc) %>% 
    mutate(Cob_parc = sum(Cob_arb, na.rm = T)) %>%
    ungroup() %>% 
    select(N_Parc, UTM_E, UTM_N, Especie, Nha, Cob_arb, Cob_parc, Fuente)
  
  set_flextable_defaults(
    decimal.mark = ",",
    big.mark = "."
  )
  
  ft_ap2 <- bind_rows(df_cob, df_flora) %>% 
    pivot_wider(
      names_from = Fuente,
      values_from = c(Nha, Cob_arb, Cob_parc)
    ) %>% 
    arrange(N_Parc, Especie) %>%  
    select(N_Parc, UTM_E, UTM_N, Especie, ends_with("PCob"), ends_with("Flora")) %>% 
    `names<-`(
      c(
        "Parcela",
        str_c("Coordenadas UTM Datum WGS84 Huso ", huso, "_Coordenada Este"), 
        str_c("Coordenadas UTM Datum WGS84 Huso ", huso, "_Coordenada Norte"), 
        "Especie",
        {if(!is.null(df_cob)){
          c("P.COB_NHA\n(árb/ha)",
            "P.COB_Cobertura por especie (%)",
            "P.COB_Cobertura por parcela (%)")
        }},
        "FLORA_NHA\n(árb/ha)",
        "FLORA_Cobertura por especie (%)",
        "FLORA_Cobertura por parcela (%)"
      )
    ) %>% 
    flextable() %>% 
    merge_v(j = c(1:3, 7, 10)) %>% 
    separate_header(split = "_") %>% 
    italic(j = 4) %>% 
    autofit() %>% 
    theme_box() %>% 
    valign(part = "header", valign = "center") %>% 
    align(part = "header", align = "center") %>% 
    bg(bg = "#bcc5d4", part = "header")
  
  wb_ap2 <- wb_workbook(theme = "Integral") %>% 
    {if(portada == "KIMAL"){
      wb_portada_148_kimal(., apendice = 2, provincia = provincia)
    } else {
      wb_portada_148_default(., apendice = 2, provincia = provincia)
    }} %>% 
    wb_add_worksheet("SP_Nha_y_Cobertura_Parcelas", grid_lines = F) %>% 
    wb_add_flextable(sheet = "SP_Nha_y_Cobertura_Parcelas", ft = ft_2, start_col = 1, start_row = 1)
  
  ft_3 <- parcelas %>% 
    st_join(predios %>% select(N_Predio)) %>% 
    st_drop_geometry() %>% 
    mutate_at("Tipo_veg", italic_sp) %>% 
    select(N_Predio, N_Rodal, N_Parc, Tipo_veg, Coord_X, Coord_Y) %>% 
    `names<-`(
      c(
        'N° Predio',
        'N° Rodal',
        'N° Parcela',
        'Tipo vegetacional',
        str_c("Coordenadas UTM Datum WGS84 Huso ", huso,"_Coordenada Este"), 
        str_c("Coordenadas UTM Datum WGS84 Huso ", huso,"_Coordenada Norte")
      )
    ) %>% 
    flextable() %>% 
    merge_v(j = c(1:2)) %>% 
    colformat_md() %>%
    flextable::separate_header(split = "_") %>%
    autofit() %>% 
    theme_box() %>% 
    valign(part = "header", valign = "center") %>% 
    align(part = "header", align = "center") %>% 
    bg(bg = "#bcc5d4", part = "header")
  
  wb_ap3 <- wb_workbook(theme = "Integral") %>% 
    {if(portada == "KIMAL"){
      wb_portada_148_kimal(., apendice = 3, provincia = provincia)
    } else {
      wb_portada_148_default(., apendice = 3, provincia = provincia)
    }} %>% 
    wb_add_worksheet("Ubicación_Parcelas", grid_lines = F) %>% 
    wb_add_flextable(sheet = "Ubicación_Parcelas", ft = ft_3, start_col = 1, start_row = 1)
  
  return(
    Apendice_2 = wb_ap2,
    Apendice_3 = wb_ap3
  )
}
apendice_5 <- function(bd_flora, rodales, tabla_predios, tabla_areas){
  set_flextable_defaults(
    decimal.mark = ",",
    big.mark = "."
  )
  
  wb_ap5 <- wb_workbook(theme = "Integral") %>% 
    {if(portada == "KIMAL"){
      wb_portada_148_kimal(., apendice = 2, provincia = "Limarí")
    } else {
      wb_portada_148_default(., apendice = 2, provincia = "Limarí")
    }} 
  
  new_border <- create_border(
    bottom = "thin", bottom_color = wb_color("black"),
    top = "thin", top_color = wb_color("black"),
    left = "thin", left_color = wb_color("black"),
    right = "thin", right_color = wb_color("black")
  )
  wb_ap5$styles_mgr$add(new_border, "new_border")
  new_fill <- create_fill(patternType = "solid", fgColor = wb_color(hex = "#bcc5d4"))
  wb_ap5$styles_mgr$add(new_fill, "new_fill")
  new_font <- create_font(b = TRUE, color = wb_color("black"))
  wb_ap5$styles_mgr$add(new_font, "new_font")
  header_cellxfs <- create_cell_style(
    num_fmt_id = 0,
    horizontal = "center",
    text_rotation = 0,
    fill_id = wb_ap5$styles_mgr$get_fill_id("new_fill"),
    font_id = wb_ap5$styles_mgr$get_font_id("new_font"),
    border_id = wb_ap5$styles_mgr$get_border_id("new_border")
  )
  wb_ap5$styles_mgr$add(header_cellxfs, "header_cellxfs")
  
  tbl_1 <- tabla_predios %>% 
    select(N_Predio, Nom_Predio, Propietari) %>% 
    `names<-`(c("N° Predio", "Nombre Predio", "Propietario"))
  
  tbl_2 <- tabla_predios %>% 
    select(N_Predio, Nom_Predio, Rol) %>% 
    `names<-`(c("N° Predio", "Nombre Predio", "Rol"))
  
  tbl_3 <- tabla_predios %>% 
    select(N_Predio, Nom_Predio, Comuna, Provincia, Región) %>% 
    `names<-`(c("N° Predio", "Nombre Predio", "Comuna", "Provincia", "Región")) 
  
  tbl_4 <- tabla_predios %>% 
    mutate(Titulo_dominio = "-", SII = "-") %>% 
    select(N_Predio, Titulo_dominio, SII, Sup_ha) %>% 
    `names<-`(c("N° Predio", "Título de dominio", "servicio de Impuestos Internos", "Estudio Técnico")) 
  
  tbl_5 <- carto_uso_actual %>%
    st_join(predios %>% select(N_Predio), largest = T) %>%
    st_drop_geometry() %>% 
    select(N_Predio, Uso_Actual, Sup_ha) %>% 
    mutate(
      Uso_Actual = case_when(
        Uso_Actual %>% str_detect("Bosque") ~ str_c("Bosques_", Uso_Actual),
        Uso_Actual %>% str_detect("Uso agrícola") ~ Uso_Actual %>% str_replace(" \\(", "_") %>% str_replace("\\)", ""),
        Uso_Actual %>% str_detect("Otros usos") ~ "Otros usos",
        .default = Uso_Actual
      )
    ) %>% 
    group_by(N_Predio, Uso_Actual) %>% 
    summarise(Sup_ha = sum(Sup_ha), .groups = "drop") %>% 
    pivot_wider(
      names_from = Uso_Actual, values_from = Sup_ha
    ) %>% 
    mutate(Total = rowSums(select(., -c(1)), na.rm = TRUE)) %>%
    select(
      N_Predio,
      matches("Bosques.*Adulto"),
      matches("Bosques.*Renoval"),
      matches("Bosques.*Mixto"),
      matches("Uso.*I-IV"),
      matches("Uso.*V-VIII"),
      matches("Uso.*N.C."),
      starts_with("Áreas"),
      starts_with("Otros"),
      Total
    ) %>%  
    rename("N° Predio" = N_Predio) %>% 
    flextable() %>% 
    flextable::separate_header(split = "_") %>%
    autofit() %>% 
    theme_box() %>% 
    valign(part = "header", valign = "center") %>% 
    align(part = "header", align = "center") %>% 
    bg(bg = "#bcc5d4", part = "header")
  
  tbl_6 <- tabla_areas %>% 
    select(N_Predio, N_Area, Clase_Uso, Pend_media, Sup_ha) %>% 
    `names<-`(c("Predio N°", "Área N°", "CUS", "Pendiente media (%)", "Superficie (ha)")) %>% 
    flextable() %>% 
    merge_v(j = c(1)) %>% 
    autofit() %>% 
    theme_box() %>% 
    valign(part = "header", valign = "center") %>% 
    align(part = "header", align = "center") %>% 
    bg(bg = "#bcc5d4", part = "header")
  
  tbl_7 <- tabla_areas %>% 
    mutate(Ancho = "") %>% 
    select(N_Predio, N_Area, Nombre_curso, Tipo_Perma, Distancia, Ancho) %>% 
    `names<-`(c("Predio N°", "Área N°", "Cursos de agua", "Temporalidad", "Distancia al área a intervenir (m)", "Ancho del cauce (m)")) %>% 
    flextable() %>% 
    merge_v(j = c(1)) %>% 
    autofit() %>% 
    theme_box() %>% 
    valign(part = "header", valign = "center") %>% 
    align(part = "header", align = "center") %>% 
    bg(bg = "#bcc5d4", part = "header")
  
  parcelas_rodales <- bd_flora %>% 
    filter(
      Habito %>% stri_trans_general("Latin-ASCII") %>% stri_detect_regex("arbol", case_insensitive = T),
      !Cob_BB %>% str_to_lower() %in% c(NA_character_, "fp")
    ) %>% 
    select(-matches("Nom_Predio|N_Rodal|Tipo_veg|Tipo_For|Subtipo_fo")) %>% 
    st_as_sf(coords = c("UTM_E","UTM_N"), crs = st_crs(rodales), remove = F) %>% 
    st_intersection(st_union(rodales)) %>%
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
    ungroup() 
  
  nha_parc <- parcelas_rodales %>% 
    group_by(Nom_Predio, N_Rodal, N_Parc, UTM_E, UTM_N, Tipo_veg) %>% 
    summarise(Nha = sum(Nha,na.rm = T), .groups = "drop") %>%
    rename(Coord_X = UTM_E, Coord_Y = UTM_N) %>% 
    st_as_sf(coords = c("Coord_X","Coord_Y"), crs = 32719, remove = F) %>% 
    arrange(N_Parc)
  
  estimaciones_x_tipo <- parcelas_rodales %>% 
    select(Tipo_veg, N_Parc, Especie, Nha) %>% 
    split(.$Tipo_veg) %>%
    map(~complete(.,Tipo_veg, N_Parc, Especie, fill = list(Nha = 0))) %>% 
    map(function(x) x %>% group_by(Tipo_veg, Especie) %>% summarise(Nha = mean(Nha, na.rm = T) %>% round()) %>% ungroup()) %>% 
    bind_rows() %>% 
    filter(Nha != 0) %>% 
    arrange(Tipo_veg, desc(Nha))
  
  tabla_attr_rodal <- rodales %>% 
    count(N_Rodal, Tipo_fores, Subtipo_fo, Tipo_veg) %>% select(-n) %>% 
    st_join(nha_parc %>% select(N_Parc, Nha)) %>% 
    group_by(N_Rodal, Tipo_fores, Subtipo_fo, Tipo_veg, geometry) %>%
    summarise(
      Parcelas = str_c(N_Parc, collapse = '-'),
      NHA = mean(Nha, na.rm=T),
      .groups = "drop"
    ) %>% 
    st_drop_geometry() %>% 
    mutate(
      Tipo_attr = if_else(is.na(Parcelas), "Estimación", "Parcela directa"),
      Nom_attr = case_when(
        !is.na(Parcelas) ~ str_c("Parcela ", Parcelas),
        is.na(Parcelas) & !(Tipo_veg %in% unique(estimaciones_x_tipo$Tipo_veg)) ~ "Estimación por Tipo vegetacional similar",
        .default = "Estimación por Tipo vegetacional"
      )
    ) %>% 
    select(-c(Parcelas, NHA)) %>% 
    unnest(N_Rodal) %>% 
    arrange(N_Rodal)
  
  nha_ptos <- tabla_areas %>% 
    left_join(
      tabla_attr_rodal %>% 
        mutate(Parcelas = map(Nom_attr, function(x){x %>% str_remove("Parcela ") %>% str_split("-") %>% unlist() %>% as.integer()}))
    ) %>% 
    filter(Nom_attr %>% str_detect("Parcela")) %>% 
    mutate(
      nha_x_sp = map(Parcelas, nha_x_sp_fun, bd = parcelas_rodales)
    ) %>% 
    select(N_Predio, N_Area, nha_x_sp) %>% 
    unnest_legacy() %>% arrange(N_Predio, N_Area)
  
  nha_est <- tabla_areas %>% 
    left_join(
      tabla_attr_rodal %>% 
        select(N_Rodal, Tipo_veg, Tipo_attr)
    ) %>% 
    filter(Tipo_attr == "Estimación") %>%  
    left_join(estimaciones_x_tipo) %>% 
    drop_na(Nha) %>% 
    select(N_Predio, N_Area, Especie, Nha)
  
  nha_otros <- tabla_areas %>% 
    left_join(tabla_attr_rodal %>% dplyr::select(N_Rodal, Subtipo_fo, Tipo_veg, Tipo_attr)) %>% 
    filter(Tipo_veg %in% c(setdiff(unique(rodales$Tipo_veg),nha_parc$Tipo_veg))) %>% # filtrar areas con tipos vegetacionales sin parcelas
    mutate(sp = map(Tipo_veg, ~str_trim(str_split_1(str_extract(.,"(?<=de ).*"), "-")))) %>%
    mutate(nha_x_sp = map2(sp,Subtipo_fo, function(x,y){
      bd1 <- parcelas_rodales %>% 
        left_join(
          tabla_attr_rodal %>% 
            select(N_Rodal, Subtipo_fo, Tipo, Tipo_attr)
        ) %>% 
        filter(N_Parc %in% c(
          parcelas_rodales %>% 
            filter(Especie %in% x, Subtipo_fo == y) %>% 
            group_by(N_Parc, Especie) %>% 
            tally() %>% 
            group_by(N_Parc) %>% 
            filter(n() == length(x)) %>% 
            .$N_Parc %>% unique()
        )
        ) %>% 
        select(N_Parc, Especie, Nha) %>% 
        arrange(N_Parc, desc(Nha)) %>%
        complete(N_Parc, Especie) 
      bd2 <- bd1 %>% 
        filter(!Especie %in% c(bd1 %>% filter(is.na(Nha)) %>% .$Especie %>% unique())) %>% 
        group_by(Especie) %>% 
        summarise(Nha = mean(Nha) %>% round())
      return(bd2)
    })) %>% 
    select(N_Predio, N_Area, nha_x_sp) %>% 
    unnest_legacy() 
  
  tbl_8 <- bind_rows(nha_ptos, nha_est, nha_otros) %>% 
    arrange(N_Predio, N_Area, Nha) %>% 
    mutate_at("Nha", as.integer) %>% 
    left_join(
      tabla_areas %>% select(N_Rodal, N_Area, Sup_ha)
    ) %>% 
    left_join(
      tabla_attr_rodal %>% 
        select(N_Rodal, Tipo_fores, Subtipo_fo)
    ) %>% 
    mutate_at("Tipo_fores", str_remove, "Tipo Forestal ") %>% 
    mutate(
      Estructura = "",
      Estado_desarrollo = "",
      Estado_fitosanitario = ""
    ) %>% 
    arrange(N_Predio, N_Area, desc(Nha)) %>% 
    select(N_Predio, N_Area, Tipo_fores, Subtipo_fo, Sup_ha, Especie, Nha, Estructura, Estado_desarrollo, Estado_fitosanitario) 
  
  stf <- tbl_8 %>% 
    rowid_to_column("ID") %>% 
    group_by(Subtipo_fo) %>%
    slice_min(ID) %>% ungroup() %>% arrange(ID) %>% select(-ID)
  
  ft_8 <- tbl_8 %>% 
    select(-Subtipo_fo) %>% 
    `names<-`(c("Predio N°", "Área N°", "Tipo forestal", "Superficie (ha)", "Especies dominantes", "Densidad (ind/ha)", "Estructura actual", "Estado de desarrollo", "Estado sanitario")) %>% 
    flextable() %>% 
    merge_v(j = c(1:2, 4)) %>% 
    autofit() %>% 
    theme_box() %>% 
    valign(part = "header", valign = "center") %>% 
    align(part = "header", align = "center") %>% 
    bg(bg = "#bcc5d4", part = "header")
  
  for (i in seq_len(nrow(stf))) {
    ft_8 <- footnote(
      ft_8,
      part = 'body', i = c(which(tbl_8$Subtipo_fo == stf$Subtipo_fo[i])), j = 3,
      value = as_paragraph(stf$Subtipo_fo[1]),
      ref_symbols = str_c(rep('*', i), collapse = "")
    )
  }
  
  tbl_9 <- tabla_areas %>% 
    left_join(rodales %>% count(N_Rodal, Tipo_fores, Subtipo_fo) %>% st_drop_geometry()) %>% 
    mutate(Año = "-", ) %>% 
    select(N_Predio, N_Area, Sup_ha, Año, Clase_Uso, Tipo_fores, Subtipo_fo) %>% 
    arrange(N_Predio, N_Area) %>% 
    mutate_at("N_Area", as.character) %>% 
    adorn_totals() 
  
  ft_9 <- tbl_9 %>% select(-Subtipo_fo) %>% 
    `names<-`(c("Predio N°", "Área a reforestar_N°", "Área a reforestar_Superficie (ha)", "Año", "Clase Capac. Uso", "Tipo forestal y/o especies a eliminar")) %>% 
    flextable() %>% 
    flextable::separate_header(split = "_") %>%
    merge_v(j = c(1)) %>% 
    autofit() %>% 
    theme_box() %>% 
    valign(part = "header", valign = "center") %>% 
    align(part = "header", align = "center") %>%
    merge_h_range(i = ~`Predio N°` == "Total", j1 = "Predio N°", j2 = "Área a reforestar_N°", part = "body") %>% 
    align(i = ~`Predio N°` == "Total", j = c(1:2), align = "center") %>% 
    bg(bg = "#bcc5d4", part = "header")
  
  for (i in seq_len(nrow(stf))) {
    ft_9 <- footnote(
      ft_9,
      part = 'body', i = c(which(tbl_9$Subtipo_fo == stf$Subtipo_fo[i])), j = 6,
      value = as_paragraph(stf$Subtipo_fo[1]),
      ref_symbols = str_c(rep('*', i), collapse = "")
    )
  }
  
  tbl_10 <- tabla_areas %>% 
    group_by(Comuna, Provincia, Región, N_Predio) %>% 
    summarise(Sup_ha = sum(Sup_ha)) %>% 
    group_by(Comuna, Provincia, Región) %>% 
    summarise(N_Predio = n(), Sup_ha = sum(Sup_ha)) %>% 
    mutate(N_Predio_ref = as.integer(NA), Sup_ha_ref = as.double(NA)) %>% 
    adorn_totals() %>% 
    `names<-`(c("Comuna", "Provincia", "Región", "Corta_N° predios", "Corta_Superficie (ha)", "Reforestación_N° predios", "Reforestación_Superficie (ha)")) %>% 
    flextable() %>% 
    flextable::separate_header(split = "_") %>%
    autofit() %>% 
    merge_h_range(i = ~ Comuna == "Total", j1 = "Comuna", j2 = "Región", part = "body") %>% 
    theme_box() %>% 
    valign(part = "header", valign = "center") %>% 
    align(part = "header", align = "center") %>% 
    align(i = ~ Comuna == "Total", align = "center", j = c(1:3), part = "body") %>% 
    bg(bg = "#bcc5d4", part = "header")
  
  tbl_11 <- areas %>% 
    st_intersection(obras %>% select(Tipo, Obra)) %>% 
    count(Tipo, Obra) %>% select(-n) %>% 
    mutate(
      Sup_ha = st_area(geometry) %>% set_units(ha) %>% drop_units() %>% round_half_up(2),
      Sup_m2 = st_area(geometry) %>% drop_units() %>% round_half_up(),
    ) %>% 
    st_drop_geometry() %>% 
    janitor::adorn_totals() %>% 
    `names<-`(c("Tipo de obra", "Obra", "Superficie (ha)", "Superficie (m2)")) %>% 
    flextable() %>% 
    flextable::separate_header(split = "_") %>%
    autofit() %>% 
    merge_v(j = c(1)) %>% 
    merge_h_range(i = ~ `Tipo de obra` == "Total", j1 = "Tipo de obra", j2 = "Obra", part = "body") %>% 
    theme_box() %>% 
    colformat_md() %>% 
    valign(part = "header", valign = "center") %>% 
    align(part = "header", align = "center") %>% 
    align(i = ~ `Tipo de obra` == "Total", align = "center", j = c(1:2), part = "body") %>% 
    bg(bg = "#bcc5d4", part = "header")
  
  wb <- wb_ap5 %>% 
    # Tabla Propietarios
    wb_add_worksheet("Propietarios") %>% 
    wb_add_data(sheet = "Propietarios", x = tbl_1, start_col = 1, start_row = 1) %>% 
    wb_set_cell_style(dims = wb_dims(x = tbl_1, select = "col_names"), style = wb_ap5$styles_mgr$get_xf_id("header_cellxfs")) %>% 
    wb_add_border(dims = wb_dims(x = tbl_1, select = "data"), inner_hgrid = "thin", inner_vgrid = "thin") %>%
    wb_set_col_widths(cols = seq_len(ncol(tbl_1)), width = "auto") %>% 
    # Tabla Roles
    wb_add_worksheet("Roles") %>% 
    wb_add_data(sheet = "Roles", x = tbl_2, start_col = 1, start_row = 1) %>% 
    wb_set_cell_style(dims = wb_dims(x = tbl_2, select = "col_names"), style = wb_ap5$styles_mgr$get_xf_id("header_cellxfs")) %>% 
    wb_add_border(dims = wb_dims(x = tbl_2, select = "data"), inner_hgrid = "thin", inner_vgrid = "thin") %>%
    wb_set_col_widths(cols = seq_len(ncol(tbl_2)), width = "auto") %>% 
    # Tabla Localidad
    wb_add_worksheet("Localidad") %>% 
    wb_add_data(sheet = "Localidad", x = tbl_3, start_col = 1, start_row = 1) %>% 
    wb_set_cell_style(dims = wb_dims(x = tbl_3, select = "col_names"), style = wb_ap5$styles_mgr$get_xf_id("header_cellxfs")) %>% 
    wb_add_border(dims = wb_dims(x = tbl_3, select = "data"), inner_hgrid = "thin", inner_vgrid = "thin") %>%
    wb_set_col_widths(cols = seq_len(ncol(tbl_3)), width = "auto") %>% 
    # Tabla Superficies
    wb_add_worksheet("Superficies") %>% 
    wb_add_data(sheet = "Superficies", x = tbl_4, start_col = 1, start_row = 1) %>% 
    wb_set_cell_style(dims = wb_dims(x = tbl_4, select = "col_names"), style = wb_ap5$styles_mgr$get_xf_id("header_cellxfs")) %>% 
    wb_add_border(dims = wb_dims(x = tbl_4, select = "data"), inner_hgrid = "thin", inner_vgrid = "thin") %>%
    wb_add_numfmt(dims = wb_dims(x = tbl_4, cols = 4, select = "data"), numfmt = "#,##0.00") %>% 
    wb_set_col_widths(cols = seq_len(ncol(tbl_4)), width = "auto") %>% 
    # Tabla uso actual
    wb_add_worksheet("Uso_Actual", grid_lines = F) %>% 
    wb_add_flextable(sheet = "Uso_Actual", ft = tbl_5, start_col = 1, start_row = 1) %>% 
    # Tabla Suelos
    wb_add_worksheet("Suelos", grid_lines = F) %>% 
    wb_add_flextable(sheet = "Suelos", ft = tbl_6, start_col = 1, start_row = 1) %>% 
    # Tabla Recursos hídricos
    wb_add_worksheet("Recursos_hídricos", grid_lines = F) %>% 
    wb_add_flextable(sheet = "Recursos_hídricos", ft = tbl_7, start_col = 1, start_row = 1) %>% 
    # Tabla Vegetación
    wb_add_worksheet("Vegetación", grid_lines = F) %>% 
    wb_add_flextable(sheet = "Vegetación", ft = ft_8, start_col = 1, start_row = 1) %>% 
    # Tabla Corta
    wb_add_worksheet("Corta", grid_lines = F) %>% 
    wb_add_flextable(sheet = "Corta", ft = ft_9, start_col = 1, start_row = 1) %>% 
    # Tabla Resumen
    wb_add_worksheet("Resumen", grid_lines = F) %>% 
    wb_add_flextable(sheet = "Resumen", ft = tbl_10, start_col = 1, start_row = 1) %>%  
    # Tabla Obras
    wb_add_worksheet("Obras", grid_lines = F) %>% 
    wb_add_flextable(sheet = "Obras", ft = tbl_11, start_col = 1, start_row = 1) 
  
  return(wb)
}

get_apendices <- function(bd_flora){
  
}