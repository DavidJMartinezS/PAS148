# Funciones de apooyo
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
nha_x_sp_fun <- function(parcelas, bd, add_var = NULL){
  bd %>% 
    select(N_Parc, Especie, Nha) %>% 
    filter(N_Parc %in% parcelas) %>% 
    complete(N_Parc, Especie, fill = list(Nha = 0)) %>% 
    left_join(bd %>% count(Especie, across(add_var)) %>% select(-n)) %>% 
    group_by(Especie, across(add_var)) %>% 
    summarise(Nha = mean(Nha,na.rm = T) %>% round_half_up(), .groups = "drop") %>% 
    mutate_at("Nha", as.integer) %>% 
    suppressMessages() %>% suppressWarnings()
}
cob_x_sp_fun <- function(parcelas, bd_flora){
  bd_flora %>% 
    select(N_Parc, Especie, Habito, Cob_BB) %>% 
    filter(N_Parc %in% parcelas) %>% 
    mutate(
      Cob_ind = case_match(
        Cob_BB,
        "r" ~ "1",
        "+" ~ "3",
        "1" ~ "<5",
        "2" ~ "7.5",
        "3" ~ "10-25",
        "4" ~ "25-50",
        "5" ~ "50-75",
        "6" ~ "75-100",
        .default = ""
      )
    ) %>% 
    group_by(Especie, Habito) %>% 
    summarise(Cob_ind = str_c(unique(Cob_ind), collapse = "; "), .groups = "drop") %>% 
    suppressMessages() %>% suppressWarnings()
}

# Funciones apéndices
apendice_2_3 <- function(
    PAS, 
    bd_flora, 
    bd_pcob = NULL, 
    rodales, 
    predios, 
    portada = "default", 
    provincia, 
    huso = NULL  
  ){
  stopifnot(all(c('Parcela', 'UTM_E', 'UTM_N', 'Especie', 'N_ind', 'Habito', 'Cob_BB') %in% names(bd_flora)))
  stopifnot(all(c('Nom_Predio', 'N_Rodal') %in% names(rodales)))
  stopifnot(all(c('N_Predio') %in% names(predios)))
  stopifnot(PAS %in% c(148, 151))
  
  if (is.null(huso)) {
    huso <- if_else(
      st_crs(rodales)[[2]] %>% str_split_1("\n") %>% .[length(.)] %>% str_extract("\\d+") == 32719,
      "19S", 
      "18S"
    )
  }
  
  bd_flora <- bd_flora %>% 
    rename_if(names(.) %>% stri_trans_general("Latin-ASCII") %>% stri_cmp_equiv("campana", strength = 1), ~ "Campana") 
  
  if(!is.null(bd_pcob)){
    stopifnot(all(c('Parcela', 'Especie', 'Copa_NS', 'Copa_EO') %in% names(bd_pcob)))
  }
  
  parcelas <- bd_flora %>%
    count(Nom_Predio, N_Rodal, Tipo_veg, N_Parc, UTM_E, UTM_N) %>% select(-n) %>%
    mutate(Fuente = "Elaboracion propia") %>%
    st_as_sf(coords = c("UTM_E","UTM_N"), crs = st_crs(rodales), remove = F) %>% 
    rename(Coord_X = UTM_E, Coord_Y = UTM_N) %>%
    mutate_at(vars(starts_with("Coord")), round_half_up) %>%
    arrange(N_Parc) %>%
    select(Nom_Predio, N_Rodal, Tipo_veg, N_Parc, Coord_X, Coord_Y, Fuente)
  
  if (PAS == 148) {
    if (!is.null(bd_pcob)) {
      df_cob <- bd_pcob %>% 
        select(-starts_with("UTM")) %>% 
        inner_join(
          bd_flora %>% count(Campana, Parcela, N_Parc, UTM_E, UTM_N) %>% select(-n)
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
    } else {
      df_cob <- NULL
    }
  }
  
  df_flora <- bd_flora %>% 
    {if (count(.[], N_Parc, Especie, sort = T) %>% filter(n > 1) %>% nrow() >= 1){
      .[] %>% group_by(N_Parc, UTM_E, UTM_N, Especie) %>% slice_sample() %>% ungroup()
    } else .[]} %>% 
    {if (PAS == 148) {
      .[] %>% mutate(
        Cob_arb = case_when(
          Cob_BB == "r" ~ 1,
          Cob_BB == "+" ~ 3,
          Cob_BB == "1" ~ 5,
          Cob_BB == "2" ~ 7.5,
          Cob_BB == "3" ~ 17.5,
          Cob_BB == "4" ~ 37.5,
          Cob_BB == "5" ~ 62.5,
          Cob_BB == "6" ~ 87.5,
          .default = 0
        ),
        Fuente = "Flora"
      )
    } else {
      .[] %>% mutate(
        Cob_arb = case_when(
          Cob_BB == "r" ~ "1",
          Cob_BB == "+" ~ "3",
          Cob_BB == "1" ~ "<5",
          Cob_BB == "2" ~ "7.5",
          Cob_BB == "3" ~ "10-25",
          Cob_BB == "4" ~ "25-50",
          Cob_BB == "5" ~ "50-75",
          Cob_BB == "6" ~ "75-100",
          .default = ""
        )
      )
    }} %>% 
    {if(PAS == 148) {
      .[] %>% group_by(N_Parc) %>% mutate(Cob_parc = sum(Cob_arb, na.rm = T)) %>% ungroup() %>%
        select(N_Parc, UTM_E, UTM_N, Especie, Nha, Cob_arb, Cob_parc, Fuente)
    } else {
      .[] %>% select(N_Parc, UTM_E, UTM_N, Especie, Habito, Nha, Cob_arb)
    }}
  
  set_flextable_defaults(
    decimal.mark = ".",
    big.mark = ","
  )
  
  if (PAS == 148) {
    ft_2 <- bind_rows(df_cob, df_flora) %>% 
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
      flextable::flextable() %>% 
      flextable::separate_header(split = "_", ) %>%
      delete_columns(1) %>% 
      merge_v(j = c(1:3, 7, 10)) %>% 
      italic(j = 4) %>% 
      autofit() %>% 
      theme_box() %>% 
      bg(i = ~ `FLORA_Cobertura por parcela (%)` < 10, j = 7, bg = "yellow") %>% 
      {if(!is.null(df_cob)) {
        bg(i = ~ `P.COB_Cobertura por parcela (%)` < 10, j = 10, bg = "yellow")
      } else .} %>% 
      valign(part = "header", valign = "center") %>% 
      align(part = "header", align = "center") %>% 
      bg(bg = "#bcc5d4", part = "header") 
  }
  if (PAS == 151) {
    ft_2 <- df_flora %>% 
      arrange(N_Parc, Especie) %>%  
      `names<-`(
        c(
          "Parcela",
          str_c("Coordenadas UTM Datum WGS84 Huso ", huso, "_Coordenada Este"), 
          str_c("Coordenadas UTM Datum WGS84 Huso ", huso, "_Coordenada Norte"), 
          "Especie",
          "Hábito",
          "NHA\n(árb/ha)",
          "Cobertura por especie (%)"
        )
      ) %>% 
      flextable() %>% 
      flextable::separate_header(split = "_") %>% 
      merge_v(j = c(1:3)) %>% 
      italic(j = 4) %>% 
      autofit() %>% 
      bg(
        i = ~`Hábito` %>% stri_trans_general("Latin-ASCII") %>% stri_detect_regex("arbol", case_insensitive = T) & 
          `Cobertura por especie (%)` %in% c("10-25", "25-50", "50-75", "75-100"), 
        j = 7, bg = "yellow"
      ) %>% 
      theme_box() %>% 
      valign(part = "header", valign = "center") %>% 
      align(part = "header", align = "center") %>% 
      bg(bg = "#bcc5d4", part = "header")
  }
  
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
    list(
      Apendice_2 = wb_ap2,
      Apendice_3 = wb_ap3
    )
  )
}

apendice_5_PAS148 <- function(
    bd_flora, 
    rodales, 
    tabla_predios, 
    tabla_areas, 
    tabla_attr_rodal, 
    portada = "default", 
    provincia, 
    carto_uso_actual = NULL, 
    obras = NULL, 
    bd_fauna = NULL
  ){
  set_flextable_defaults(
    decimal.mark = ",",
    big.mark = "."
  )
  
  wb_ap5 <- wb_workbook(theme = "Integral") %>% 
    {if(portada == "KIMAL"){
      wb_portada_148_kimal(., apendice = 5, provincia = provincia)
    } else {
      wb_portada_148_default(., apendice = 5, provincia = provincia)
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
  
  tbl_predios <- tabla_predios %>% 
    select(N_Predio, Nom_Predio, Rol, Propietari, Comuna, Provincia, Región) %>% 
    `names<-`(c("N° Predio", "Nombre Predio", "Rol", "Propietario", "Comuna", "Provincia", "Región"))
  
  # tbl_1 <- tabla_predios %>% 
  #   select(N_Predio, Nom_Predio, Propietari) %>% 
  #   `names<-`(c("N° Predio", "Nombre Predio", "Propietario"))
  # 
  # tbl_2 <- tabla_predios %>% 
  #   select(N_Predio, Nom_Predio, Rol) %>% 
  #   `names<-`(c("N° Predio", "Nombre Predio", "Rol"))
  # 
  # tbl_3 <- tabla_predios %>% 
  #   select(N_Predio, Nom_Predio, Comuna, Provincia, Región) %>% 
  #   `names<-`(c("N° Predio", "Nombre Predio", "Comuna", "Provincia", "Región")) 
  
  tbl_4 <- tabla_predios %>% 
    mutate(Titulo_dominio = "-", SII = "-") %>% 
    select(N_Predio, Titulo_dominio, SII, Sup_ha) %>% 
    `names<-`(c("N° Predio", "Título de dominio", "servicio de Impuestos Internos", "Estudio Técnico")) 
  
  if (!is.null(carto_uso_actual)) {
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
  }
  
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
  
  if (any(c("Nombre_curso", "Tipo_Perma", "Distancia", "Ancho") %in% names(tabla_areas))) {
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
  }
  
  nha_est_x_tipo <- bd_flora %>% 
    select(Tipo_veg, N_Parc, Especie, Nha) %>% 
    split(.$Tipo_veg) %>%
    map(~complete(.,Tipo_veg, N_Parc, Especie, fill = list(Nha = 0))) %>% 
    map(function(x) x %>% group_by(Tipo_veg, Especie) %>% summarise(Nha = mean(Nha, na.rm = T) %>% round()) %>% ungroup()) %>% 
    bind_rows() %>% 
    filter(Nha != 0) %>% 
    arrange(Tipo_veg, desc(Nha))
  
  nha_ptos <- tabla_areas %>% 
    left_join(
      tabla_attr_rodal %>% 
        mutate(Parcelas = map(Nom_attr, function(x){x %>% str_remove("Parcela ") %>% str_split("-") %>% unlist() %>% as.integer()}))
    ) %>% 
    filter(Nom_attr %>% str_detect("Parcela")) %>% 
    mutate(
      nha_x_sp = map(Parcelas, nha_x_sp_fun, bd = bd_flora)
    ) %>% 
    select(N_Predio, N_Area, nha_x_sp) %>% 
    unnest_legacy() %>% arrange(N_Predio, N_Area)
  
  nha_est <- tabla_areas %>% 
    left_join(
      tabla_attr_rodal %>% 
        select(N_Rodal, Tipo_veg, Tipo_attr)
    ) %>% 
    filter(Tipo_attr == "Estimación") %>%  
    left_join(nha_est_x_tipo) %>% 
    drop_na(Nha) %>% 
    select(N_Predio, N_Area, Especie, Nha)
  
  nha_otros <- tabla_areas %>% 
    left_join(tabla_attr_rodal %>% dplyr::select(N_Rodal, Subtipo_fo, Tipo_veg, Tipo_attr)) %>% 
    filter(Tipo_veg %in% c(setdiff(unique(rodales$Tipo_veg),unique(bd_flora$Tipo_veg)))) %>% # filtrar areas con tipos vegetacionales sin parcelas
    mutate(sp = map(Tipo_veg, ~str_trim(str_split_1(str_extract(.,"(?<=de ).*"), "-")))) %>%
    mutate(nha_x_sp = map2(sp,Subtipo_fo, function(x,y){
      bd1 <- bd_flora %>% 
        left_join(
          tabla_attr_rodal %>% 
            select(N_Rodal, Subtipo_fo, Tipo, Tipo_attr)
        ) %>% 
        filter(N_Parc %in% c(
          bd_flora %>% 
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
      value = as_paragraph(stf$Subtipo_fo[i]),
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
      value = as_paragraph(stf$Subtipo_fo[i]),
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
  
  if (!is.null(obras)) {
    tbl_11 <- areas %>% 
      st_intersection(obras %>% select(Tipo, Obra)) %>% 
      count(Tipo, Obra) %>% select(-n) %>% 
      mutate(
        Sup_ha = st_area(geometry) %>% set_units(ha) %>% drop_units() %>% round_half_up(2),
        Sup_m2 = st_area(geometry) %>% drop_units() %>% round_half_up(),
      ) %>% 
      st_drop_geometry() %>% 
      janitor::adorn_totals() %>% 
      `names<-`(c("Temporalidad de la obra", "Obra", "Superficie (ha)", "Superficie (m2)")) %>% 
      flextable() %>% 
      flextable::separate_header(split = "_") %>%
      autofit() %>% 
      merge_v(j = c(1)) %>% 
      merge_h_range(i = ~ `Temporalidad de la obra` == "Total", j1 = "Temporalidad de la obra", j2 = "Obra", part = "body") %>% 
      theme_box() %>% 
      colformat_md() %>% 
      valign(part = "header", valign = "center") %>% 
      align(part = "header", align = "center") %>% 
      align(i = ~ `Temporalidad de la obra` == "Total", align = "center", j = c(1:2), part = "body") %>% 
      bg(bg = "#bcc5d4", part = "header")
  }
  
  tbl_12 <- bd_flora %>% 
    group_by(Nom_Predio, N_Rodal, Parcela, N_Parc, UTM_E, UTM_N, Tipo_veg) %>% 
    summarise(Nha = sum(Nha,na.rm = T), .groups = "drop") %>% 
    summarise(
      Promedio = mean(Nha, na.rm = T) %>% round(),
      n = n(),
      Rango = str_c(min(Nha)," - ", max(Nha)),
      cuasivarianza = ((1-(n*(500/10000)/(Rodales$Sup_ha %>% sum())))*(sd(Nha)^2/n))%>% round(2),
      CV = ((sqrt(cuasivarianza)/Promedio)*100) %>% round(1),
      T_est = qt(0.975,n-1) %>% round_half_up(3),
      E_abs = (T_est * sqrt(cuasivarianza)) %>% round(),
      E_rel = ((E_abs/Promedio)*100) %>% round(1),
      Int_conf = str_c(round(Promedio - E_abs), " - ", round(Promedio + E_abs))
    ) %>% 
    mutate_all(as.character) %>% 
    pivot_longer(cols = everything(), names_to = "Parámetro", values_to = "Nha Total") %>% 
    mutate_at(2, str_replace, "\\.", "\\,")
  
  if (!is.null(bd_fauna)) {
    tbl_13 <- bd_fauna %>% 
      filter(Nombre_cientifico != "Sin registro", Categoria != "-") %>% 
      select(Nombre_cientifico, matches("utm_"),Categoria, Decreto) %>% 
      st_as_sf(coords = c("UTM_E","UTM_N"), crs = st_crs(predios), remove = F) %>% 
      st_intersection(predios %>% select(N_Predio)) %>% 
      st_drop_geometry() %>% 
      count(N_Predio, Nombre_cientifico, Categoria, Decreto)
  }
  
  wb <- wb_ap5 %>% 
    # Tabla Predios
    wb_add_worksheet("Info.Predios") %>% 
    wb_add_data(sheet = "Info.Predios", x = tbl_predios, start_col = 1, start_row = 1) %>% 
    wb_set_cell_style(dims = wb_dims(x = tbl_predios, select = "col_names"), style = wb_ap5$styles_mgr$get_xf_id("header_cellxfs")) %>% 
    wb_add_border(dims = wb_dims(x = tbl_predios, select = "data"), inner_hgrid = "thin", inner_vgrid = "thin") %>%
    wb_set_col_widths(cols = seq_len(ncol(tbl_predios)), width = "auto") %>% 
    # # Tabla Propietarios
    # wb_add_worksheet("Propietarios") %>% 
    # wb_add_data(sheet = "Propietarios", x = tbl_1, start_col = 1, start_row = 1) %>% 
    # wb_set_cell_style(dims = wb_dims(x = tbl_1, select = "col_names"), style = wb_ap5$styles_mgr$get_xf_id("header_cellxfs")) %>% 
    # wb_add_border(dims = wb_dims(x = tbl_1, select = "data"), inner_hgrid = "thin", inner_vgrid = "thin") %>%
    # wb_set_col_widths(cols = seq_len(ncol(tbl_1)), width = "auto") %>% 
    # # Tabla Roles
    # wb_add_worksheet("Roles") %>% 
    # wb_add_data(sheet = "Roles", x = tbl_2, start_col = 1, start_row = 1) %>% 
    # wb_set_cell_style(dims = wb_dims(x = tbl_2, select = "col_names"), style = wb_ap5$styles_mgr$get_xf_id("header_cellxfs")) %>% 
    # wb_add_border(dims = wb_dims(x = tbl_2, select = "data"), inner_hgrid = "thin", inner_vgrid = "thin") %>%
    # wb_set_col_widths(cols = seq_len(ncol(tbl_2)), width = "auto") %>% 
    # # Tabla Localidad
    # wb_add_worksheet("Localidad") %>% 
    # wb_add_data(sheet = "Localidad", x = tbl_3, start_col = 1, start_row = 1) %>% 
    # wb_set_cell_style(dims = wb_dims(x = tbl_3, select = "col_names"), style = wb_ap5$styles_mgr$get_xf_id("header_cellxfs")) %>% 
    # wb_add_border(dims = wb_dims(x = tbl_3, select = "data"), inner_hgrid = "thin", inner_vgrid = "thin") %>%
    # wb_set_col_widths(cols = seq_len(ncol(tbl_3)), width = "auto") %>% 
  
    # Tabla Superficies
    wb_add_worksheet("Sup.Predios") %>% 
    wb_add_data(sheet = "Sup.Predios", x = tbl_4, start_col = 1, start_row = 1) %>% 
    wb_set_cell_style(dims = wb_dims(x = tbl_4, select = "col_names"), style = wb_ap5$styles_mgr$get_xf_id("header_cellxfs")) %>% 
    wb_add_border(dims = wb_dims(x = tbl_4, select = "data"), inner_hgrid = "thin", inner_vgrid = "thin") %>%
    wb_add_numfmt(dims = wb_dims(x = tbl_4, cols = 4, select = "data"), numfmt = "#,##0.00") %>% 
    wb_set_col_widths(cols = seq_len(ncol(tbl_4)), width = "auto") %>% 
    # Tabla Suelos
    wb_add_worksheet("Suelos", grid_lines = F) %>% 
    wb_add_flextable(sheet = "Suelos", ft = tbl_6, start_col = 1, start_row = 1) %>% 
    # Tabla Vegetación
    wb_add_worksheet("Vegetación", grid_lines = F) %>% 
    wb_add_flextable(sheet = "Vegetación", ft = ft_8, start_col = 1, start_row = 1) %>% 
    # Tabla Corta
    wb_add_worksheet("Corta", grid_lines = F) %>% 
    wb_add_flextable(sheet = "Corta", ft = ft_9, start_col = 1, start_row = 1) %>% 
    # Tabla Resumen
    wb_add_worksheet("Resumen", grid_lines = F) %>% 
    wb_add_flextable(sheet = "Resumen", ft = tbl_10, start_col = 1, start_row = 1) %>%  
    # tabla Estadisticos
    wb_add_worksheet("Estadisticos") %>% 
    wb_add_data(sheet = "Estadisticos", x = tbl_12, start_col = 1, start_row = 1) %>% 
    wb_set_cell_style(dims = wb_dims(x = tbl_12, select = "col_names"), style = wb_ap5$styles_mgr$get_xf_id("header_cellxfs")) %>% 
    wb_add_border(dims = wb_dims(x = tbl_12, select = "data"), inner_hgrid = "thin", inner_vgrid = "thin") %>%
    wb_add_numfmt(dims = wb_dims(x = tbl_12, cols = 2, rows = 4, select = "data"), numfmt = "#,##0.00") %>% 
    wb_set_col_widths(cols = seq_len(ncol(tbl_12)), width = "auto") 
  
  # Tabla Recursos hídricos
  if (exists("tbl_7")) {
    wb <- wb %>%
      wb_add_worksheet("Recursos_hídricos", grid_lines = F) %>% 
      wb_add_flextable(sheet = "Recursos_hídricos", ft = tbl_7, start_col = 1, start_row = 1)  
  }
  # Tabla uso actual
  if (exists("tbl_5")) {
    wb <- wb %>% 
      wb_add_worksheet("Uso_Actual", grid_lines = F) %>% 
      wb_add_flextable(sheet = "Uso_Actual", ft = tbl_5, start_col = 1, start_row = 1)
  } 
  # Tabla fauna
  if (exists("tbl_13")) {
    wb <- wb %>% 
      wb_add_worksheet("BD_Fauna") %>% 
      wb_add_data(sheet = "BD_Fauna", x = tbl_13, start_col = 1, start_row = 1) %>% 
      wb_set_cell_style(dims = wb_dims(x = tbl_13, select = "col_names"), style = wb_ap5$styles_mgr$get_xf_id("header_cellxfs")) %>% 
      wb_add_border(dims = wb_dims(x = tbl_13, select = "data"), inner_hgrid = "thin", inner_vgrid = "thin") %>%
      wb_set_col_widths(cols = seq_len(ncol(tbl_13)), width = "auto")
  }
  # Tabla Obras
  if (exists("tbl_11")) {
    wb <- wb %>%
      wb_add_worksheet("Obras", grid_lines = F) %>% 
      wb_add_flextable(sheet = "Obras", ft = tbl_11, start_col = 1, start_row = 1)
  }
  
  return(wb)
}

apendice_5_PAS151 <- function(
    bd_flora, 
    rodales, 
    tabla_predios, 
    tabla_areas, 
    tabla_attr_rodal, 
    portada = "default", 
    provincia, 
    obras = NULL
  ) {
  set_flextable_defaults(
    decimal.mark = ",",
    big.mark = "."
  )
  wb_ap5 <- wb_workbook(theme = "Integral") %>% 
    {if(portada == "KIMAL"){
      wb_portada_148_kimal(., apendice = 5, provincia = provincia)
    } else {
      wb_portada_148_default(., apendice = 5, provincia = provincia)
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
  
  # Tabla predios ----
  tbl_predios <- tabla_predios %>% 
    select(N_Predio, Nom_Predio, Rol, Propietari, Comuna, Provincia, Región) %>% 
    `names<-`(c("N° Predio", "Nombre Predio", "Rol", "Propietario", "Comuna", "Provincia", "Región"))
  wb_ap5 <- wb_ap5 %>% 
    wb_add_worksheet("Info.Predios") %>% 
    wb_add_data(sheet = "Info.Predios", x = tbl_predios, start_col = 1, start_row = 1) %>% 
    wb_set_cell_style(dims = wb_dims(x = tbl_predios, select = "col_names"), style = wb_ap5$styles_mgr$get_xf_id("header_cellxfs")) %>% 
    wb_add_border(dims = wb_dims(x = tbl_predios, select = "data"), inner_hgrid = "thin", inner_vgrid = "thin") %>%
    wb_set_col_widths(cols = seq_len(ncol(tbl_predios)), width = "auto")
  
  # Tabla superficie predios ----
  tbl_sup_predios <- tabla_predios %>% 
    mutate(Titulo_dominio = "-", SII = "-") %>% 
    select(N_Predio, Titulo_dominio, SII, Sup_ha) %>% 
    `names<-`(c("N° Predio", "Título de dominio", "servicio de Impuestos Internos", "Estudio Técnico")) 
  wb_ap5 <- wb_ap5 %>% 
    wb_add_worksheet("Sup.Predios") %>% 
    wb_add_data(sheet = "Sup.Predios", x = tbl_sup_predios, start_col = 1, start_row = 1) %>% 
    wb_set_cell_style(dims = wb_dims(x = tbl_sup_predios, select = "col_names"), style = wb_ap5$styles_mgr$get_xf_id("header_cellxfs")) %>% 
    wb_add_border(dims = wb_dims(x = tbl_sup_predios, select = "data"), inner_hgrid = "thin", inner_vgrid = "thin") %>%
    wb_add_numfmt(dims = wb_dims(x = tbl_sup_predios, cols = 4, select = "data"), numfmt = "#,##0.00") %>% 
    wb_set_col_widths(cols = seq_len(ncol(tbl_sup_predios)), width = "auto")
  
  # Tabla info áreas de corta ----
  tbl_areas <- tabla_areas %>% 
    mutate(SG_SI = "", SG_NO = "", Dist_Area_Prot = "") %>% 
    {if(!"Distancia" %in% names(.)) {
      mutate(., Distancia = "")
    } else .} %>% 
    select(N_Predio, N_Area, Sup_ha, Ran_Pend, SG_SI, SG_NO, Clase_Eros, Distancia, Dist_Area_Prot) %>% 
    `names<-`(
      c(
        "Predio N°",
        "Área N°",
        "Superficie (ha)",
        "Rango pendiente (%)",
        "Suelo Granítico_Si",
        "Suelo Granítico_No",
        "Grado de Erosión",
        "Distancia a cursos, cuerpos de agua o humedales (m)",
        "Distancia a áreas bajo protección oficial (m)"
      )
    ) %>% 
    flextable() %>% 
    flextable::separate_header(split = "_") %>% 
    merge_v(j = c(1)) %>% 
    autofit() %>% 
    theme_box() %>% 
    valign(part = "header", valign = "center") %>% 
    align(part = "header", align = "center") %>% 
    bg(bg = "#bcc5d4", part = "header")
  wb_ap5 <- wb_ap5 %>% 
    wb_add_worksheet("Áreas") %>% 
    wb_add_flextable(sheet = "Áreas", ft = tbl_areas, start_col = 1, start_row = 1) 
  
  # Tabla densidad y coberturas ----
  nha_est_x_tipo <- bd_flora %>% 
    select(Tipo_veg, N_Parc, Especie, Nha) %>% 
    split(.$Tipo_veg) %>%
    map(~complete(.,Tipo_veg, N_Parc, Especie, fill = list(Nha = 0))) %>% 
    map(function(x) x %>% group_by(Tipo_veg, Especie) %>% summarise(Nha = mean(Nha, na.rm = T) %>% round()) %>% ungroup()) %>% 
    bind_rows() %>% 
    mutate_at("Nha", ~as.integer(round_half_up(.))) %>% 
    filter(Nha != 0) %>% 
    arrange(Tipo_veg, desc(Nha)) %>% 
    left_join(bd_flora %>% count(Especie, Habito) %>% select(-n)) %>% 
    suppressMessages() %>% suppressWarnings()
  
  cob_est_x_tipo <- bd_flora %>% 
    select(Tipo_veg, N_Parc, Especie, Habito, Cob_BB) %>% 
    split(.$Tipo_veg) %>%
    map(function(x){
      x %>% 
        mutate(
          Cob_ind = case_match(
            Cob_BB,
            "r" ~ "1",
            "+" ~ "3",
            "1" ~ "<5",
            "2" ~ "7.5",
            "3" ~ "10-25",
            "4" ~ "25-50",
            "5" ~ "50-75",
            "6" ~ "75-100",
            .default = ""
          )
        ) %>% 
        group_by(Tipo_veg, Especie, Habito) %>% 
        summarise(Cob_ind = str_c(unique(Cob_ind), collapse = "; "), .groups = "drop")
    }) %>% 
    bind_rows() %>% 
    arrange(Tipo_veg) %>% 
    suppressMessages() %>% suppressWarnings()
  
  nha_ptos <- tabla_areas %>% 
    left_join(
      tabla_attr_rodal %>% 
        mutate(Parcelas = map(Nom_attr, function(x){x %>% str_remove("Parcela ") %>% str_split("-") %>% unlist() %>% as.integer()}))
    ) %>% 
    filter(Nom_attr %>% str_detect("Parcela")) %>% 
    mutate(
      nha_x_sp = map(Parcelas, nha_x_sp_fun, bd = bd_flora, add_var = "Habito"),
      cob_x_sp = map(Parcelas, cob_x_sp_fun, bd = bd_flora)
    ) %>% 
    mutate_at("cob_x_sp", ~map(., select, -c(Especie, Habito))) %>% 
    select(N_Predio, N_Rodal, N_Area, nha_x_sp, cob_x_sp) %>% 
    unnest_legacy() %>% arrange(N_Predio, N_Area) %>% 
    suppressMessages() %>% suppressWarnings()
  
  nha_est <- tabla_areas %>% 
    left_join(tabla_attr_rodal) %>% 
    filter(Tipo_attr == "Estimación", !Nom_attr %>% stri_detect_regex("similar", case_insensitive = T)) %>%  
    left_join(nha_est_x_tipo) %>% 
    left_join(cob_est_x_tipo) %>%
    drop_na(Nha) %>% 
    select(N_Predio, N_Rodal, N_Area, Especie, Habito, Nha, Cob_ind) %>% 
    suppressMessages() %>% suppressWarnings()
  
  nha_otros <- tabla_areas %>% 
    left_join(tabla_attr_rodal) %>% 
    filter(Nom_attr %>% stri_detect_regex("similar", case_insensitive = T)) %>% # filtrar areas con tipos vegetacionales sin parcelas
    # mutate(Tipo_veg = "Matorral de colliguaja integerrima") %>% 
    mutate(sp = map(Tipo_veg, ~str_trim(str_split_1(str_extract(.,"(?<=de ).*"), "-")))) %>% # identificar sp en estos tipos vegetacionales
    mutate(
      bd1 = map(sp, function(x){
        bd_flora %>% 
          filter(
            N_Parc %in% c(
              bd_flora %>%
                filter(Especie %in% x) %>%
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
      }),
      nha_x_sp = map(bd1, function(x){
        x %>% 
          filter(!Especie %in% c(x %>% filter(is.na(Nha)) %>% .$Especie %>% unique())) %>% # Ver si filtrar o no el Nha, porque no debiera haber 
          group_by(Especie) %>% 
          summarise(Nha = mean(Nha) %>% round_half_up() %>% as.integer())
      }),
      Parcelas = map_chr(bd1, function(x){
        unique(x$N_Parc) %>% str_c(collapse = ", ")
      }),
      cob_x_sp = map(Parcelas, cob_x_sp_fun, bd = bd_flora)
    ) %>% 
    mutate_at("cob_x_sp", ~map(., select, -c(Especie))) %>% 
    select(N_Predio, N_Rodal, N_Area, nha_x_sp, cob_x_sp, Tipo_veg, Parcelas) %>%
    unnest_legacy() %>% 
    suppressMessages() %>% suppressWarnings()
  
  obs_fun <- function(x){
    x <- if (is.na(x)) NA else x %>% str_split_1(pattern = "[:punct:]") %>% str_trim()
    if (is.na(x)) {
      NA_character_
    } else if (length(x) == 1) {
      str_c("Se utilizó la parcela ", x)
    } else {
      str_c("Se utilizaron las parcelas: ", str_c(x %>% shQuote(), collapse = ", "))
    }
  }
  
  tabla_attr_rodal_final <- tabla_attr_rodal %>% 
    mutate(
      Nom_attr = case_when(
        !N_Rodal %in% unique(bind_rows(nha_ptos, nha_est, nha_otros) %>% .$N_Rodal) ~ "Encontrar otro método de estimación",
        .default = Nom_attr
      )
    ) %>% 
    left_join(nha_otros %>% count(N_Rodal, Parcelas) %>% select(-n)) %>% 
    mutate(Obs = map_chr(Parcelas, obs_fun)) %>% 
    select(-Parcelas)
  
  tbl_nha_cob <- bind_rows(nha_ptos, nha_est, nha_otros %>% select(-c(Tipo_veg, Parcelas))) %>% 
    select(-N_Rodal) %>% 
    arrange(N_Predio, N_Area, Nha, Cob_ind) %>% 
    rename(Cobertura = Cob_ind)
  
  habitos <- tbl_nha_cob$Habito %>% unique() 
  
  for (i in seq_along(habitos)) {
    tbl_nha <- tbl_nha_cob %>%
      filter(Habito == habitos[i]) %>% 
      pivot_wider(names_from = "Habito", values_from = c("Nha", "Cobertura"), names_glue = "{Habito}_{.value}") %>% 
      arrange(N_Predio, N_Area, Especie) %>% 
      rename("Predio N°" = 1, "N° Sector" = 2) %>% 
      rename_at("Especie", ~str_c(habitos[i], "_", .)) %>% 
      rename_at(vars(contains("Nha")), str_replace, "Nha", "Densidad (Ind/ha)") %>% 
      rename_at(vars(contains("Cobertura")), str_replace, "Cobertura", "Cobertura (%)") %>% 
      flextable() %>% 
      flextable::separate_header(split = "_") %>% 
      merge_v(j = c(1:2)) %>% 
      italic(j = 3) %>% 
      autofit() %>% 
      theme_box() %>% 
      valign(part = "header", valign = "center") %>% 
      align(part = "header", align = "center") %>% 
      bg(bg = "#bcc5d4", part = "header")
    wb_ap5 <- wb_ap5 %>% 
      wb_add_worksheet(sheet = habitos[i]) %>% 
      wb_add_flextable(sheet = habitos[i], ft = tbl_nha, start_col = 1, start_row = 1) 
  }
  
  # Tabla corta ----
  tbl_corta <- tabla_areas %>% 
    mutate(Año = "-", Tipo = "Corta y descepado", Descripcion = "Extracción de ejemplares para instalación de obras", Cob_final = 0) %>% 
    select(N_Predio, N_Area, Año, Sup_ha, Tipo, Descripcion, Cob_final) %>% 
    `names<-`(c("Predio N°", "N° Sector", "Año de intervención (ha)", "Superficie a intervenir (ha)", "Tipo de intervención", "Descripción de la intervención", "Cobertura final (%)")) %>% 
    flextable() %>% 
    flextable::separate_header(split = "_") %>% 
    merge_v(j = c(1)) %>% 
    autofit() %>% 
    theme_box() %>% 
    valign(part = "header", valign = "center") %>% 
    align(part = "header", align = "center") %>% 
    bg(bg = "#bcc5d4", part = "header") 
  wb_ap5 <- wb_ap5 %>% 
    wb_add_worksheet(sheet = "Programa_de_actividades") %>% 
    wb_add_flextable(sheet = "Programa_de_actividades", ft = tbl_nha, start_col = 1, start_row = 1) 
  
  # Tabla obras ----
  if (!is.null(obras)) {
    tbl_obras <- areas %>% 
      st_intersection(obras %>% select(Tipo, Obra)) %>% 
      count(Tipo, Obra) %>% select(-n) %>% 
      mutate(
        Sup_ha = st_area(geometry) %>% set_units(ha) %>% drop_units() %>% round_half_up(2),
        Sup_m2 = st_area(geometry) %>% drop_units() %>% round_half_up(),
      ) %>% 
      st_drop_geometry() %>% 
      janitor::adorn_totals() %>% 
      `names<-`(c("Temporalidad de la obra", "Obra", "Superficie (ha)", "Superficie (m2)")) %>% 
      flextable() %>% 
      flextable::separate_header(split = "_") %>%
      autofit() %>% 
      merge_v(j = c(1)) %>% 
      merge_h_range(i = ~ `Temporalidad de la obra` == "Total", j1 = "Temporalidad de la obra", j2 = "Obra", part = "body") %>% 
      theme_box() %>% 
      colformat_md() %>% 
      valign(part = "header", valign = "center") %>% 
      align(part = "header", align = "center") %>% 
      align(i = ~ `Temporalidad de la obra` == "Total", align = "center", j = c(1:2), part = "body") %>% 
      bg(bg = "#bcc5d4", part = "header") %>% 
      bold(i = ~`Temporalidad de la obra` == "Total", j = c(1:4), bold = TRUE)
    wb_ap5 <- wb_ap5 %>% 
      wb_add_worksheet(sheet = "Obras") %>% 
      wb_add_flextable(sheet = "Obras", ft = tbl_obras, start_col = 1, start_row = 1) 
  }
  
  # Tabla estadisticos ----
  tbl_estadisticos <- bd_flora %>% 
    group_by(Nom_Predio, N_Rodal, Parcela, N_Parc, UTM_E, UTM_N, Tipo_veg) %>% 
    summarise(Nha = sum(Nha,na.rm = T), .groups = "drop") %>% 
    summarise(
      Promedio = mean(Nha, na.rm = T) %>% round(),
      n = n(),
      Rango = str_c(min(Nha)," - ", max(Nha)),
      cuasivarianza = ((1-(n*(500/10000)/(Rodales$Sup_ha %>% sum())))*(sd(Nha)^2/n))%>% round(2),
      CV = ((sqrt(cuasivarianza)/Promedio)*100) %>% round(1),
      T_est = qt(0.975,n-1) %>% round_half_up(3),
      E_abs = (T_est * sqrt(cuasivarianza)) %>% round(),
      E_rel = ((E_abs/Promedio)*100) %>% round(1),
      Int_conf = str_c(round(Promedio - E_abs), " - ", round(Promedio + E_abs))
    ) %>% 
    mutate_all(as.character) %>% 
    pivot_longer(cols = everything(), names_to = "Parámetro", values_to = "Nha Total") %>% 
    mutate_at(2, str_replace, "\\.", "\\,")
  wb_ap5 <- wb_ap5 %>% 
    wb_add_worksheet(sheet = "Estadisticos") %>% 
    wb_add_flextable(sheet = "Estadisticos", ft = tbl_obras, start_col = 1, start_row = 1) 
  
  return(wb_ap5)
}
wb_ap5$open()

